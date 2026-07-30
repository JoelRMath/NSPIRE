#' Spatial Heterogeneity & The Lottery Effect Sweep
#'
#' This high-volume execution script investigates the "Lottery Effect" under NSPIRE's 
#' random sampling protocol. By sweeping the proportion of "clean" tenants from 
#' 0.70 to 1.0, it tracks how spatial clustering of defects impacts regulatory risk.
#' 
#' Because extreme tail risks ($P_{05}$) require massive sample sizes to stabilize, 
#' this script executes 100,000 Monte Carlo iterations per parameter step. To prevent 
#' memory exhaustion (RAM overflow) when saving raw forensics, the execution is 
#' architected in smaller batches.

# ==========================================
# High-Volume Sensitivity Sweep (100k Iterations)
# ==========================================

devtools::load_all(".")

# 1. Setup Environment
# Point to the baseline configurations used for the tenant study
dag_csv <- here::here("inst", "extdata", "tasks_config.csv")
fp_csv  <- here::here("inst", "extdata", "floorplans_config.csv")
yaml_path <- here::here("inst", "extdata", "scenario1.yml")
score_csv <- here::here("inst", "extdata", "scoring_config.csv")

# Define the baseline demographic distribution of the property
mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

# 2. Configure Batched Sweep
# Sweep the proportion of clean tenants from 70% to 100% (Homogeneous)
clean_prop_values <- seq(0.70, 1.0, by = 0.01)

# Architecture for memory management: 10 batches of 10,000 = 100,000 total iterations per step
n_batches <- 10
iter_per_batch <- 10000

cat("\n=== Starting Batched Tenant Heterogeneity Sweep ===\n")
cat(sprintf("Executing %d iterations per parameter step (%d batches of %d)\n\n", 
            n_batches * iter_per_batch, n_batches, iter_per_batch))

results_list <- list()
ridges_list <- list() 

for (cp in clean_prop_values) {
  cat(sprintf("Evaluating clean_proportion = %.2f...\n", cp))
  
  # Clone the baseline environment and manually inject the current parameter step
  temp_env <- sim_env
  if (is.null(temp_env@scenario$tenant_factors)) {
    temp_env@scenario$tenant_factors <- list()
  }
  temp_env@scenario$tenant_factors$clean_proportion <- cp
  
  # Set the severity ratio. As defined in the report's "Conservation of Defects", 
  # a dirty unit is expected to be 20x worse than a clean unit.
  temp_env@scenario$tenant_factors$dirty_severity_ratio <- 20.0 
  
  # Accumulators for the 10 sequential batches
  batch_scores <- numeric() # Will hold all 100,000 raw scores for distribution mapping
  batch_physics <- list()   # Will hold structural building metrics (total defects, variance)
  batch_risks <- list()     # Will hold failure probabilities
  
  for (b in 1:n_batches) {
    cat(sprintf("  -> Running Batch %d/%d...\n", b, n_batches))
    
    # Execute the core simulation for this batch
    mc_res <- run_monte_carlo(
      n_iterations = iter_per_batch, 
      sim_env = temp_env,
      score_csv = score_csv,
      total_units = 50,
      audit_catch_rate = 0.75, 
      t_start = 30,
      t_end = 5,
      save_raw = TRUE  # Required to calculate spatial standard deviation below
    )
    
    summary_df <- mc_res$summary
    
    # Accumulate raw scores for true percentile calculation and downstream ridgeline plots
    batch_scores <- c(batch_scores, summary_df$score)
    
    # Extract structural building physics from the raw forensics, then allow 
    # the heavy raw data to be garbage collected to free up RAM.
    avg_tot <- 0
    avg_sd <- 0
    
    if (!is.null(mc_res$raw)) {
      # 1. Total Defects across the entire building
      total_defects_per_iter <- sapply(mc_res$raw, nrow)
      avg_tot <- mean(total_defects_per_iter)
      
      # 2. Spatial Clustering (Standard Deviation of defects per unit)
      # This explicitly measures how heavily concentrated the damage is.
      sd_per_iter <- sapply(mc_res$raw, function(iter_df) {
        if (nrow(iter_df) == 0) return(0)
        
        # Count defects by apartment
        unit_counts <- table(iter_df$unit_id)
        counts_vec <- as.numeric(unit_counts)
        
        # Ensure pristine units (0 defects) are mathematically included in the variance
        missing_units <- 50 - length(counts_vec)
        if (missing_units > 0) {
          counts_vec <- c(counts_vec, rep(0, missing_units))
        }
        return(sd(counts_vec))
      })
      avg_sd <- mean(sd_per_iter)
    }
    
    # Store the linear metrics to be averaged across all batches later
    batch_physics[[b]] <- c(tot = avg_tot, sd = avg_sd)
    batch_risks[[b]] <- c(
      fail_60 = mean(summary_df$score < 60),
      fail_30 = mean(summary_df$score < 30)
    )
  }
  
  # ---------------------------------------------------------
  # AGGREGATE THE 100,000 ITERATIONS
  # ---------------------------------------------------------
  
  # 1. Save all 100k scores into a dedicated dataframe for the density ridgeline plots
  ridges_df <- data.frame(
    param_val = cp,
    score = batch_scores
  )
  ridges_list[[length(ridges_list) + 1]] <- ridges_df
  
  # 2. Calculate true percentiles from the combined 100k vector to accurately capture the extreme tail
  true_median <- median(batch_scores)
  true_p05 <- quantile(batch_scores, 0.05)
  
  # 3. Average the linear probabilities and physics metrics across the 10 batches
  physics_mat <- do.call(rbind, batch_physics)
  risks_mat <- do.call(rbind, batch_risks)
  
  agg_res <- data.frame(
    param_val = cp,
    avg_total_defects = mean(physics_mat[, "tot"]),      
    avg_clustering_sd = mean(physics_mat[, "sd"]),      
    score_median = true_median,
    score_p05 = true_p05,
    fail_risk_60 = mean(risks_mat[, "fail_60"]),
    critical_fail_risk_30 = mean(risks_mat[, "fail_30"])
  )
  
  results_list[[length(results_list) + 1]] <- agg_res
}

# Bind final outputs across all parameter steps
master_tenant_study <- do.call(rbind, results_list)
master_tenant_ridges <- do.call(rbind, ridges_list)

# Save Results to disk
save_dir <- here::here("results", "sensitivity", "tenant_mix")
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

saveRDS(master_tenant_study, file = file.path(save_dir, "master_tenant_study.rds"))
saveRDS(master_tenant_ridges, file = file.path(save_dir, "master_tenant_ridges.rds"))

cat("\nSweep Complete! High-resolution results saved to:", save_dir, "\n")