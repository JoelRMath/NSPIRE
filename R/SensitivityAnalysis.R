# ==================================================
# 1. Sensitivity Runner, for audit_rate and t_start
# ==================================================

#' Run Sensitivity Analysis with Exhaustive Observables
#'
#' Executes a batch of Monte Carlo simulations across a specified parameter range. 
#' For each parameter step, it calculates a set of distributional 
#' metrics (including Tail Drop, Backlog Probability, and Compliance Risk) and 
#' extracts the raw density objects required for downstream ridge plotting.
#'
#' @param sim_env An \code{InspectionSimulation} object establishing the baseline environment.
#' @param score_csv Character. Path to the regulatory scoring config file.
#' @param param_name Character. String identifier for the parameter to sweep (e.g., "audit_rate", "t_start").
#' @param param_values Numeric vector. The sequence of values to test across the sweep.
#' @param save_raw Logical. If TRUE, saves the raw defect forensic files to disk (default: FALSE).
#' @param ... Additional arguments passed dynamically to \code{run_monte_carlo}.
#' @return A \code{data.frame} where each row represents the exhaustive metrics for a single parameter step.
#' @export
run_sensitivity_analysis <- function(sim_env, score_csv, param_name, param_values, save_raw = FALSE, ...) {
  
  # Ensure the directory structure exists to catch I/O file drops
  base_dir <- file.path("results", "sensitivity", param_name)
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  
  # Execute the sweep across specified parameter values
  results_list <- lapply(param_values, function(val) {
    cat(sprintf("Running Analysis: %s = %f\n", param_name, val))
    
    current_env <- sim_env
    wrapper_args <- list(...)
    
    # 1. Parameter Dispatcher: Dynamically mutate the environment or wrapper args based on the target study
    switch(param_name,
           "decay_rate" = { current_env@scenario$decay_rate <- val },
           "tenant_heterogeneity" = { current_env@scenario$tenant_factors$clean_proportion <- val },
           "audit_rate" = { wrapper_args$audit_catch_rate <- val },
           "t_start"    = { wrapper_args$t_start <- val },
           "t_end"      = { wrapper_args$t_end <- val },
           stop(sprintf("Parameter '%s' not defined in dispatcher.", param_name))
    )
    
    # 2. Execution: Monte Carlo ensemble for this specific parameter state
    mc_out <- do.call(run_monte_carlo, c(list(sim_env = current_env, score_csv = score_csv, save_raw = save_raw), wrapper_args))
    
    # 3. Save Raw Iteration Data
    # Save the 1,000 summary rows for this specific step
    saveRDS(mc_out$summary, file = file.path(base_dir, paste0("summary_", val, ".rds")))
    
    if (save_raw) {
      saveRDS(mc_out$raw, file = file.path(base_dir, paste0("raw_full_", val, ".rds")))
    }
    
    # Extract raw scores to calculate the smooth density function for the ridge plots
    scores_raw <- unlist(mc_out$summary$score)
    
    # Dynamically pad the density bounds so the graphical tails don't clip at exactly 0 or 100
    min_bound <- max(0, min(scores_raw) - 2)
    max_bound <- min(100, max(scores_raw) + 2)
    score_density <- density(
      scores_raw,
      from = min_bound,
      to = max_bound,
      n = 512
    )
    
    # Cache the density object for Quarto rendering
    density_filename <- sprintf("score_density_%s.rds", val)
    saveRDS(score_density, file = file.path(base_dir, paste0("score_density_", val, ".rds")))
    
    # 4. Exhaustive Observables Calculation
    # Extract the raw vectors to compute the statistical moments
    scores <- mc_out$summary$score
    labor <- mc_out$summary$labor
    labor_cost <- mc_out$summary$total_labor_cost
    overtime <- mc_out$summary$overtime_cost
    backlog <- mc_out$summary$backlog
    
    med_score <- median(scores)
    p05_score <- quantile(scores, 0.05, names = FALSE)
    mean_score <- mean(scores)
    
    # Construct the metric matrix
    res <- data.frame(
      param_val = val,
      
      # --- Score Distribution ---
      score_mean = mean_score,
      score_sd = sd(scores),
      score_min = min(scores),
      score_p01 = quantile(scores, 0.01, names = FALSE),
      score_p05 = p05_score,       # The 5th percentile "Value at Risk"
      score_p10 = quantile(scores, 0.10, names = FALSE),
      score_p25 = quantile(scores, 0.25, names = FALSE),
      score_median = med_score,
      score_p75 = quantile(scores, 0.75, names = FALSE),
      score_p90 = quantile(scores, 0.90, names = FALSE),
      score_p95 = quantile(scores, 0.95, names = FALSE),
      score_p99 = quantile(scores, 0.99, names = FALSE),
      score_max = max(scores),
      
      # --- Risk & Tail Metrics ---
      tail_drop_median = (med_score - p05_score) / med_score, # 'tau' metric from the report
      tail_drop_mean = (mean_score - p05_score) / mean_score,
      fail_risk_60 = mean(scores < 60),                       # Probability of regulatory failure
      critical_fail_risk_30 = mean(scores < 30),
      
      # --- Labor & Time ---
      labor_hours_mean = mean(labor),
      labor_hours_median = median(labor),
      labor_hours_p95 = quantile(labor, 0.95, names = FALSE),
      
      # --- Financial Cost ---
      total_cost_mean = mean(labor_cost),
      total_cost_median = median(labor_cost),
      total_cost_p95 = quantile(labor_cost, 0.95, names = FALSE),
      total_cost_max = max(labor_cost),
      
      # --- Overtime Metrics ---
      overtime_mean = mean(overtime),
      overtime_median = median(overtime),
      overtime_p95 = quantile(overtime, 0.95, names = FALSE),
      overtime_prob = mean(overtime > 0),                     # Likelihood of breaking regular hours
      
      # --- Backlog Metrics ---
      backlog_mean = mean(backlog),
      backlog_median = median(backlog),
      backlog_p95 = quantile(backlog, 0.95, names = FALSE),
      backlog_prob = mean(backlog > 0)                        # Likelihood of capacity exhaustion
    )
    
    return(res)
    
  })
  
  # Collapse the list of dataframes into a single summary table and save
  summary_df <- do.call(rbind, results_list)
  saveRDS(summary_df, file = file.path(base_dir, "summary.rds"))
  return(summary_df)
}

# ==========================================
# 2. Specific Sensitivity Studies
# ==========================================

#' Self-Contained Study: Audit Rate Sensitivity
#'
#' Automatically loads default project configurations and runs an exhaustive
#' sweep of the internal audit efficacy (\code{audit_rate}) from 0.05 to 0.95. 
#' This generates the data used to demonstrate the "Quality Assurance Floor" in the report.
#'
#' @param n_iterations Integer. Number of Monte Carlo iterations per step.
#' @param save_raw Logical. Save raw defect forensics.
#' @return A master \code{data.frame} of results across all audit rates.
#' @export
study_audit_rate <- function(n_iterations = 1000, save_raw = FALSE) {
  
  # Hardcoded data paths ensuring the study relies on its specific baseline scenario
  dag_csv <- "results/sensitivity/audit_rate/tasks_config.csv"
  fp_csv <- "results/sensitivity/audit_rate/floorplans_config.csv"
  yaml_path <- "results/sensitivity/audit_rate/scenario1.yml"
  score_csv <- "results/sensitivity/audit_rate/scoring_config.csv"
  
  mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
  sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)
  
  cat("\n")
  cat("          Starting Audit Rate Study                 \n")
  cat("\n")
  cat("Assumptions Loaded:\n")
  cat("- Total Units:", sum(mix_weights), " (Normalized to percentages internally)\n")
  cat("- Clean Tenant Proportion:", sim_env@scenario$tenant_factors$clean_proportion, "\n")
  cat("- Dirty Tenant Multiplier:", sim_env@scenario$tenant_factors$dirty_multiplier_mean, "\n")
  cat("- Environmental Decay Rate:", sim_env@scenario$decay_rate, "\n")
  cat("- Prep Window (Days):", max(1, 30 - 5), "\n")
  cat("- Iterations per step:", n_iterations, "\n")
  cat("====================================================\n\n")
  
  # Define the sweep sequence: 5% up to 95% efficacy
  audit_test_values <- seq(0.05, 0.95, by = 0.05)
  
  master_summary <- run_sensitivity_analysis(
    sim_env = sim_env,
    score_csv = score_csv,
    param_name = "audit_rate",
    param_values = audit_test_values,
    n_iterations = n_iterations,
    save_raw = save_raw
  )
  
  base_dir <- file.path("results", "sensitivity", "audit_rate")
  saveRDS(master_summary, file = file.path(base_dir, "master_audit_rate_study.rds"))
  write.csv(master_summary, file = file.path(base_dir, "master_audit_rate_study.csv"), row.names = FALSE)
  
  cat("\n====================================================\n")
  cat("Study complete. Master summaries saved to:\n")
  cat(file.path(base_dir, "master_audit_rate_study.csv"), "\n")
  cat("====================================================\n")
  
  return(master_summary)
}

#' Self-Contained Study: Time Crunch (Prep Window) Sensitivity
#'
#' Evaluates the operational limit of the workforce by compressing the 
#' available preparation window (\code{t_start}). Maps the transition from 
#' safe distributed work, to maximum overtime expenditure, to total backlog collapse.
#'
#' @param n_iterations Integer. Number of Monte Carlo iterations per step.
#' @param save_raw Logical. Save raw defect forensics.
#' @return A master \code{data.frame} of results across all time frames.
#' @export
study_prep_time <- function(n_iterations = 1000, save_raw = FALSE) {
  
  # Hardcoded data paths ensuring the study relies on its specific baseline scenario
  dag_csv <- "results/sensitivity/t_start/tasks_config.csv"
  fp_csv <- "results/sensitivity/t_start/floorplans_config.csv"
  yaml_path <- "results/sensitivity/t_start/scenario1.yml"
  score_csv <- "results/sensitivity/t_start/scoring_config.csv"
  
  mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
  sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)
  
  cat("\n")
  cat("          Starting Time Crunch Study                \n")
  cat("\n")
  
  # Define the sweep sequence: Counting backward from a safe 40 days down to a panicked 4 days
  time_test_values <- seq(40, 4, by = -1)
  
  # Note: For this study, Audit Rate is locked at 85% 
  # to isolate the effect of pure physical time constraints.
  master_summary <- run_sensitivity_analysis(
    sim_env = sim_env,
    score_csv = score_csv,
    param_name = "t_start",
    param_values = time_test_values,
    n_iterations = n_iterations,
    save_raw = save_raw,
    audit_catch_rate = 0.85
  )
  
  base_dir <- file.path("results", "sensitivity", "t_start")
  saveRDS(master_summary, file = file.path(base_dir, "master_time_crunch_study.rds"))
  write.csv(master_summary, file = file.path(base_dir, "master_time_crunch_study.csv"), row.names = FALSE)
  
  cat("\nStudy complete. Master summaries saved to:\n")
  cat(file.path(base_dir, "master_time_crunch_study.csv"), "\n")
  
  return(master_summary)
}