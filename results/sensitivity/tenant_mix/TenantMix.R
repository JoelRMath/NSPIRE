# ==========================================
# High-Volume Sensitivity Sweep (100k Iterations)
# ==========================================

devtools::load_all(".")

# 1. Setup Environment
dag_csv <- here::here("inst", "extdata", "tasks_config.csv")
fp_csv  <- here::here("inst", "extdata", "floorplans_config.csv")
yaml_path <- here::here("inst", "extdata", "scenario1.yml")
score_csv <- here::here("inst", "extdata", "scoring_config.csv")

mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

# 2. Configure Batched Sweep
clean_prop_values <- seq(0.70, 1.0, by = 0.01)
n_batches <- 10
iter_per_batch <- 10000

cat("\n=== Starting Batched Tenant Heterogeneity Sweep ===\n")
cat(sprintf("Executing %d iterations per parameter step (%d batches of %d)\n\n", 
            n_batches * iter_per_batch, n_batches, iter_per_batch))

results_list <- list()
ridges_list <- list() 

for (cp in clean_prop_values) {
  cat(sprintf("Evaluating clean_proportion = %.2f...\n", cp))
  
  # Clone environment
  temp_env <- sim_env
  if (is.null(temp_env@scenario$tenant_factors)) {
    temp_env@scenario$tenant_factors <- list()
  }
  temp_env@scenario$tenant_factors$clean_proportion <- cp
  temp_env@scenario$tenant_factors$dirty_severity_ratio <- 20.0 
  
  # Accumulators for the 10 batches
  batch_scores <- numeric() # Will hold all 100,000 scores
  batch_physics <- list()
  batch_risks <- list()
  
  for (b in 1:n_batches) {
    cat(sprintf("  -> Running Batch %d/%d...\n", b, n_batches))
    
    mc_res <- run_monte_carlo(
      n_iterations = iter_per_batch, 
      sim_env = temp_env,
      score_csv = score_csv,
      total_units = 50,
      audit_catch_rate = 0.75, 
      t_start = 30,
      t_end = 5,
      save_raw = TRUE  
    )
    
    summary_df <- mc_res$summary
    
    # Accumulate raw scores for true percentile calculation and ridgelines
    batch_scores <- c(batch_scores, summary_df$score)
    
    # Calculate batch physics and allow raw data to be garbage collected
    avg_tot <- 0
    avg_sd <- 0
    
    if (!is.null(mc_res$raw)) {
      total_defects_per_iter <- sapply(mc_res$raw, nrow)
      avg_tot <- mean(total_defects_per_iter)
      
      sd_per_iter <- sapply(mc_res$raw, function(iter_df) {
        if (nrow(iter_df) == 0) return(0)
        unit_counts <- table(iter_df$unit_id)
        counts_vec <- as.numeric(unit_counts)
        missing_units <- 50 - length(counts_vec)
        if (missing_units > 0) {
          counts_vec <- c(counts_vec, rep(0, missing_units))
        }
        return(sd(counts_vec))
      })
      avg_sd <- mean(sd_per_iter)
    }
    
    # Store the linear metrics to be averaged later
    batch_physics[[b]] <- c(tot = avg_tot, sd = avg_sd)
    batch_risks[[b]] <- c(
      fail_60 = mean(summary_df$score < 60),
      fail_30 = mean(summary_df$score < 30)
    )
  }
  
  # ---------------------------------------------------------
  # AGGREGATE THE 100,000 ITERATIONS
  # ---------------------------------------------------------
  
  # 1. Save all 100k scores for the density ridgeline plots
  ridges_df <- data.frame(
    param_val = cp,
    score = batch_scores
  )
  ridges_list[[length(ridges_list) + 1]] <- ridges_df
  
  # 2. Calculate true percentiles from the combined vector
  true_median <- median(batch_scores)
  true_p05 <- quantile(batch_scores, 0.05)
  
  # 3. Average the linear probabilities and physics metrics across batches
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

# Bind final outputs
master_tenant_study <- do.call(rbind, results_list)
master_tenant_ridges <- do.call(rbind, ridges_list)

# Save Results
save_dir <- here::here("results", "sensitivity", "tenant_mix")
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

saveRDS(master_tenant_study, file = file.path(save_dir, "master_tenant_study.rds"))
saveRDS(master_tenant_ridges, file = file.path(save_dir, "master_tenant_ridges.rds"))

cat("\nSweep Complete! High-resolution results saved to:", save_dir, "\n")