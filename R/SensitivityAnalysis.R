
#' Run Sensitivity Analysis with Exhaustive Observables
#'
#' Executes Monte Carlo simulations, calculates a comprehensive suite of
#' distribution metrics, and optionally saves forensic raw data.
#'
#' @param sim_env The base simulation environment.
#' @param score_csv Path to scoring rubric.
#' @param param_name String identifier.
#' @param param_values Numeric vector of values.
#' @param save_raw Logical. Save raw defect forensics.
#' @param ... Additional arguments passed to run_monte_carlo
#' @export
run_sensitivity_analysis <- function(sim_env, score_csv, param_name, param_values, save_raw = FALSE, ...) {
  
  base_dir <- file.path("results", "sensitivity", param_name)
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  
  results_list <- lapply(param_values, function(val) {
    cat(sprintf("Running Analysis: %s = %f\n", param_name, val))
    
    current_env <- sim_env
    wrapper_args <- list(...)
    
    # 1. Parameter Dispatcher
    switch(param_name,
           "decay_rate" = { current_env@scenario$decay_rate <- val },
           "tenant_heterogeneity" = { current_env@scenario$tenant_factors$clean_proportion <- val },
           "audit_rate" = { wrapper_args$audit_catch_rate <- val },
           "t_start"    = { wrapper_args$t_start <- val },
           "t_end"      = { wrapper_args$t_end <- val },
           stop(sprintf("Parameter '%s' not defined in dispatcher.", param_name))
    )
    
    # 2. Execution
    mc_out <- do.call(run_monte_carlo, c(list(sim_env = current_env, score_csv = score_csv, save_raw = save_raw), wrapper_args))
    
    # 3. Save Raw Iteration Data (The 1000 raw summary rows)
    saveRDS(mc_out$summary, file = file.path(base_dir, paste0("summary_", val, ".rds")))
    if (save_raw) {
      saveRDS(mc_out$raw, file = file.path(base_dir, paste0("raw_full_", val, ".rds")))
    }
    
    # 4. Exhaustive Observables Calculation
    scores <- mc_out$summary$score
    labor <- mc_out$summary$labor
    labor_cost <- mc_out$summary$total_labor_cost
    overtime <- mc_out$summary$overtime_cost
    backlog <- mc_out$summary$backlog
    
    med_score <- median(scores)
    p05_score <- quantile(scores, 0.05, names = FALSE)
    mean_score <- mean(scores)
    
    res <- data.frame(
      param_val = val,
      
      # --- Score Distribution ---
      score_mean = mean_score,
      score_sd = sd(scores),
      score_min = min(scores),
      score_p01 = quantile(scores, 0.01, names = FALSE),
      score_p05 = p05_score,
      score_p10 = quantile(scores, 0.10, names = FALSE),
      score_p25 = quantile(scores, 0.25, names = FALSE),
      score_median = med_score,
      score_p75 = quantile(scores, 0.75, names = FALSE),
      score_p90 = quantile(scores, 0.90, names = FALSE),
      score_p95 = quantile(scores, 0.95, names = FALSE),
      score_p99 = quantile(scores, 0.99, names = FALSE),
      score_max = max(scores),
      
      # --- Risk & Tail Metrics ---
      tail_drop_median = (med_score - p05_score) / med_score,
      tail_drop_mean = (mean_score - p05_score) / mean_score,
      fail_risk_60 = mean(scores < 60),
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
      overtime_prob = mean(overtime > 0),
      
      # --- Backlog Metrics ---
      backlog_mean = mean(backlog),
      backlog_median = median(backlog),
      backlog_p95 = quantile(backlog, 0.95, names = FALSE),
      backlog_prob = mean(backlog > 0)
    )
    
    return(res)
    
  })
  
  summary_df <- do.call(rbind, results_list)
  saveRDS(summary_df, file = file.path(base_dir, "summary.rds"))
  return(summary_df)
}

#' Self-Contained Study: Audit Rate Sensitivity
#'
#' Automatically loads default project configurations and runs an exhaustive
#' sweep of the audit_rate parameter from 0.05 to 0.95.
#'
#' @param n_iterations Number of Monte Carlo iterations per step.
#' @param save_raw Logical. Save raw defect forensics.
#' @export
study_audit_rate <- function(n_iterations = 1000, save_raw = FALSE) {
  
  dag_csv <- "results/sensitivity/tasks_config.csv"
  fp_csv <- "results/sensitivity/floorplans_config.csv"
  yaml_path <- "results/sensitivity/scenario1.yml"
  score_csv <- "results/sensitivity/scoring_config.csv"
  
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