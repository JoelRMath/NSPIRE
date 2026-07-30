#' Post-Prep Decay Sensitivity Sweep (t_end)
#'
#' This execution script investigates the "Too Early" penalty.
#' It quantifies the stochastic re-emergence of defects when a property sits idle 
#' in the time gap between the completion of repairs (t_end) and the actual HUD inspection.
#' By executing this script, we map the exact threshold where natural entropic 
#' decay begins to trigger regulatory failure.

# ==========================================
# Sensitivity Sweep: Post-Prep Decay (t_end)
# ==========================================

devtools::load_all(".")

base_dir <- file.path("results", "sensitivity", "t_end")

# 1. Setup the Environment
# Point to the configuration files specifically isolated for the decay study
dag_csv <- file.path(base_dir,"tasks_config.csv")
fp_csv  <- file.path(base_dir,"floorplans_config.csv")
yaml_path <- file.path(base_dir,"scenario1.yml")
score_csv <- file.path(base_dir,"scoring_config.csv")

# Define the baseline demographic distribution of the property
mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

# 2. Configure the Sweep
# We want to see what happens when the property sits idle for 0 to 35 days.
# 0 days means repairs finish the morning of the inspection (zero decay risk).
t_end_values <- seq(0, 35, by = 1)

cat("\n=== Starting Post-Prep Decay Sweep ===\n")

# Note: We lock t_start at 45 to ensure the team has PLENTY of time 
# to fix things, completely isolating the 'decay' effect from the 'backlog' effect.
# By forcing a massive 45-day prep window, we mathematically guarantee that 
# labor capacity is never exhausted. Therefore, any failures observed here are 
# strictly the result of post-prep asset decay, not abandoned work.
decay_summary <- run_sensitivity_analysis(
  sim_env = sim_env,
  score_csv = score_csv,
  param_name = "t_end",
  param_values = t_end_values,
  n_iterations = 1000,
  save_raw = FALSE,
  audit_catch_rate = 0.85, 
  t_start = 45 
)

# 3. Save Results
# Ensure the target directory exists before attempting to save the aggregated RDS file
save_dir <- here::here("results", "sensitivity", "t_end")
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)

saveRDS(decay_summary, file = file.path(save_dir, "master_decay_study.rds"))

cat("\nSweep Complete! Results saved to:", save_dir, "\n")