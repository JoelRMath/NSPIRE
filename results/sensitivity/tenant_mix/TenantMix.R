# ==========================================
# Sensitivity Sweep: Tenant Heterogeneity
# ==========================================

devtools::load_all(".")

# 1. Setup Environment
dag_csv <- here::here("inst", "extdata", "tasks_config.csv")
fp_csv  <- here::here("inst", "extdata", "floorplans_config.csv")
yaml_path <- here::here("inst", "extdata", "scenario1.yml")
score_csv <- here::here("inst", "extdata", "scoring_config.csv")

mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

# 2. Configure Sweep: Focus entirely on the "Danger Zone"
# We sweep from 80% clean up to 100% clean
clean_prop_values <- seq(0.70, 1.0, by = 0.01)

cat("\n=== Starting Tenant Heterogeneity Sweep ===\n")

results_list <- list()

for (cp in clean_prop_values) {
  cat(sprintf("Evaluating clean_proportion = %.2f...\n", cp))
  
  # Clone environment
  temp_env <- sim_env
  
  # Safely initialize the tenant_factors list if it was missing from YAML
  if (is.null(temp_env@scenario$tenant_factors)) {
    temp_env@scenario$tenant_factors <- list()
  }
  
  temp_env@scenario$tenant_factors$clean_proportion <- cp
  
  # PLANTING THE LANDMINE:
  # A 20.0x severity ratio is required to overcome the HUD sample size divisor
  # and accurately simulate a highly destructive tenant.
  temp_env@scenario$tenant_factors$dirty_severity_ratio <- 20.0 
  
  # Run Monte Carlo WITH RAW DATA to capture physical clustering
  mc_res <- run_monte_carlo(
    n_iterations = 10000,
    sim_env = temp_env,
    score_csv = score_csv,
    total_units = 50,
    audit_catch_rate = 0.75, 
    t_start = 30,
    t_end = 5,
    save_raw = TRUE  # <--- CRITICAL: We need the physical defects
  )
  
  summary_df <- mc_res$summary
  raw_df <- mc_res$raw  # Assumes your MC function returns the raw dataframe here
  
  # ---------------------------------------------------------
  # METRIC 1 & 2: Pre-Score Physics (Proving Conservation & Clustering)
  # ---------------------------------------------------------
  avg_total_defects <- 0
  avg_clustering_sd <- 0
  
  if (!is.null(mc_res$raw)) {
    # 1. Conservation of Defects: Average number of rows (defects) across all lists
    total_defects_per_iter <- sapply(mc_res$raw, nrow)
    avg_total_defects <- mean(total_defects_per_iter)
    
    # 2. Clustering: Standard Deviation of defects per unit, averaged across iterations
    sd_per_iter <- sapply(mc_res$raw, function(iter_df) {
      if (nrow(iter_df) == 0) return(0)
      
      # Count defects per unit
      unit_counts <- table(iter_df$unit_id)
      
      # Convert to a numeric vector and pad with zeros for units that had NO defects
      # (Assuming a 50-unit building, we need the length to be exactly 50)
      counts_vec <- as.numeric(unit_counts)
      missing_units <- 50 - length(counts_vec)
      if (missing_units > 0) {
        counts_vec <- c(counts_vec, rep(0, missing_units))
      }
      
      # Return the spatial standard deviation for this specific iteration
      return(sd(counts_vec))
    })
    
    avg_clustering_sd <- mean(sd_per_iter)
  }
  
  # ---------------------------------------------------------
  # METRIC 3: Regulatory Risk (Post-Score)
  # ---------------------------------------------------------
  median_score <- median(summary_df$score)
  p05_score <- quantile(summary_df$score, 0.05)
  
  agg_res <- data.frame(
    param_val = cp,
    avg_total_defects = avg_total_defects,      # Proves volume doesn't change
    avg_clustering_sd = avg_clustering_sd,      # Proves spatial clustering increases
    score_median = median_score,
    score_p05 = p05_score,
    fail_risk_60 = mean(summary_df$score < 60),
    critical_fail_risk_30 = mean(summary_df$score < 30)
  )
  
  results_list[[length(results_list) + 1]] <- agg_res
}

master_tenant_study <- do.call(rbind, results_list)

# 3. Save Results
save_dir <- here::here("results", "sensitivity", "tenant_mix")
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

saveRDS(master_tenant_study, file = file.path(save_dir, "master_tenant_study.rds"))

cat("\nSweep Complete! Results saved to:", save_dir, "\n")