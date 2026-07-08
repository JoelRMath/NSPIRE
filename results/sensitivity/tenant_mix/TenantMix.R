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

# 2. Configure Sweep
# We will sweep the proportion of "clean" tenants from 50% up to 100%
clean_prop_values <- seq(0.50, 1.0, by = 0.025)

cat("\n=== Starting Tenant Heterogeneity Sweep ===\n")

results_list <- list()

for (cp in clean_prop_values) {
  cat(sprintf("Evaluating clean_proportion = %.2f...\n", cp))
  
  # Clone environment and modify tenant factors
  temp_env <- sim_env
  temp_env@scenario$tenant_factors$clean_proportion <- cp
  
  # Pump up the dirty multiplier to expose the "Lottery Effect"
  temp_env@scenario$tenant_factors$dirty_multiplier_mean <- 5.0 
  
  # Run Monte Carlo (locking other variables to safe baseline values)
  mc_res <- run_monte_carlo(
    n_iterations = 1000,
    sim_env = temp_env,
    score_csv = score_csv,
    total_units = 50,
    audit_catch_rate = 0.75, # 75% catch rate leaves some defects hidden
    t_start = 30,
    t_end = 5,
    save_raw = FALSE
  )
  
  # Extract summary
  summary_df <- mc_res$summary
  
  # Calculate custom tail drop and metrics
  median_score <- median(summary_df$score)
  p05_score <- quantile(summary_df$score, 0.05)
  
  agg_res <- data.frame(
    param_val = cp,
    score_median = median_score,
    score_p05 = p05_score,
    score_sd = sd(summary_df$score),
    tail_drop_median = ifelse(median_score > 0, (median_score - p05_score) / median_score, 0),
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