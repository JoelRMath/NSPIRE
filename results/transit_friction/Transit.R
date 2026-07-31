#' Transit Friction Tax
#'
#' Execution script: quantifies the hidden labor costs associated with physical
#' human movement and tool transitions during a property-wide maintenance.
#' By running 10,000 paired simulations, it directly compares theoretical "wrench time" 
#' (frictionless 'maintenance robots') against realistic human execution (includes 
#' micro-delays for intra-unit tool swaps and macro-delays for inter-unit travel and short breaks).
#' This data creates the "Operational Reality Gap" figure in the Quarto report.

# ==========================================
# Transit Friction
# ==========================================

devtools::load_all(".")
library(ggplot2)
library(tidyr)

# 1. Setup Environment
# Load the baseline configuration files specific to the transit friction study
dag_csv <- here::here("inst", "extdata", "tasks_config.csv")
fp_csv  <- here::here("inst", "extdata", "floorplans_config.csv")
yaml_path <- here::here("results","transit_friction","scenario1.yml")
score_csv <- here::here("inst", "extdata", "scoring_config.csv")
mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)

sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

cat("\nRunning 10000 paired simulations to quantify Transit Friction...\n")

# 2. Run Monte Carlo Loop for Paired Data
n_iterations <- 10000
results_list <- list()

set.seed(42) # Lock seed to guarantee reproducibility of the exact 34.2% tax shown in the report
for (i in 1:n_iterations) {
  # Generate a unique stochastic realization of building defects
  raw_defects <- run_simulation(sim_env, total_units = 50)
  
  # Route the repairs through the DAG to mathematically apply topological and spatial transit friction
  scheduled <- schedule_repairs(raw_defects, sim_env, t_prep_days = 30)
  
  if (nrow(scheduled) > 0) {
    # Ideal Scenario: Pure isolated "wrench time" without any human/physical delays
    ideal_hours <- sum(scheduled$repair_time_mins) / 60
    
    # Realistic Scenario: Wrench time + the context-aware stochastic transit delays
    realistic_hours <- sum(scheduled$repair_time_mins + scheduled$friction_mins) / 60
    
    # Store the paired data to calculate the exact comparative spread
    results_list[[i]] <- data.frame(
      iteration = i,
      Ideal = ideal_hours,
      Realistic = realistic_hours
    )
  }
}

# Bind all iterations into a single master dataframe
paired_data <- do.call(rbind, results_list)

# 3. Calculate Global Metrics
# Calculate the mean friction tax (the ~34% gap) to dynamically inject into the plot subtitle
mean_ideal <- mean(paired_data$Ideal)
mean_real <- mean(paired_data$Realistic)
tax_pct <- ((mean_real - mean_ideal) / mean_ideal) * 100

# 4. Save Base Data for Quarto
save_dir <- here::here("results", "transit_friction")
if(!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
saveRDS(paired_data, file.path(save_dir, "friction_distribution_data.rds"))

# 5. Format Data for ggplot Density Plot
# Melt the dataframe from wide to long format so ggplot can easily map the 'Scenario' column to fill colors
plot_data <- tidyr::pivot_longer(
  paired_data, 
  cols = c("Ideal", "Realistic"), 
  names_to = "Scenario", 
  values_to = "Total_Hours"
)

saveRDS(plot_data,file.path(save_dir, "friction_plot.rds"))

# 6. Generate Density Plot
# Renders the overlapping density distributions to visualize the operational reality gap
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