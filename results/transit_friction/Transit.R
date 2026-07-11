# ==========================================
# Operational Reality: Transit Friction
# ==========================================

devtools::load_all(".")
library(ggplot2)
library(tidyr)

# Setup Environment
dag_csv <- here::here("inst", "extdata", "tasks_config.csv")
fp_csv  <- here::here("inst", "extdata", "floorplans_config.csv")
yaml_path <- here::here("results","transit_friction","scenario1.yml")
score_csv <- here::here("inst", "extdata", "scoring_config.csv")
mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)

sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

cat("\nRunning 10000 paired simulations to quantify Transit Friction...\n")

# 1. Run Monte Carlo Loop for Paired Data
n_iterations <- 10000
results_list <- list()

set.seed(42) # Lock seed for reproducibility
for (i in 1:n_iterations) {
  # Generate a unique building realization
  raw_defects <- run_simulation(sim_env, total_units = 50)
  
  # Schedule repairs to apply topological and spatial transit friction
  scheduled <- schedule_repairs(raw_defects, sim_env, t_prep_days = 30)
  
  if (nrow(scheduled) > 0) {
    # Extract total wrench time
    ideal_hours <- sum(scheduled$repair_time_mins) / 60
    
    # Extract total real time (wrench + transit friction)
    realistic_hours <- sum(scheduled$repair_time_mins + scheduled$friction_mins) / 60
    
    results_list[[i]] <- data.frame(
      iteration = i,
      Ideal = ideal_hours,
      Realistic = realistic_hours
    )
  }
}

paired_data <- do.call(rbind, results_list)

# Calculate the mean friction tax for the subtitle
mean_ideal <- mean(paired_data$Ideal)
mean_real <- mean(paired_data$Realistic)
tax_pct <- ((mean_real - mean_ideal) / mean_ideal) * 100

# 2. Save plot data for Quarto
save_dir <- here::here("results", "transit_friction")
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
saveRDS(paired_data, file.path(save_dir, "friction_distribution_data.rds"))

# 3. Format Data for ggplot Density Plot
plot_data <- tidyr::pivot_longer(
  paired_data, 
  cols = c("Ideal", "Realistic"), 
  names_to = "Scenario", 
  values_to = "Total_Hours"
)

save_dir <- here::here("results", "transit_friction")
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
saveRDS(plot_data,file.path(save_dir, "friction_plot.rds"))

# 4. Generate Density Plot
p <- ggplot(plot_data, aes(x = Total_Hours, fill = Scenario, color = Scenario)) +
  geom_density(alpha = 0.6, linewidth = 1) +
  scale_fill_manual(values = c("Ideal" = "#4e79a7", "Realistic" = "#e15759"),
                    labels = c("Theoretical Wrench Time", "Realistic Time (with Friction)")) +
  scale_color_manual(values = c("Ideal" = "#2c4663", "Realistic" = "#8c2d2e"), guide = "none") +
  theme_minimal(base_size = 14) +
  labs(
    title = "The Operational Reality Gap",
    subtitle = sprintf("Transit friction adds an average %.1f%% 'tax' to total required labor hours.", tax_pct),
    x = "Total Labor Hours Required",
    y = "Density (Probability)"
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

print(p)