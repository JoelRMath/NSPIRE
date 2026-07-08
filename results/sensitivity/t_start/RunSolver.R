# ==========================================
# Prescriptive Analytics: Staffing Curve Solver
# ==========================================

devtools::load_all(".")
library(ggplot2)

# 1. Setup the Environment
dag_csv <- "inst/extdata/tasks_config.csv"
fp_csv  <- "inst/extdata/floorplans_config.csv"
yaml_path <- "inst/extdata/scenario1.yml"
score_csv <- "inst/extdata/scoring_config.csv"

mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

# 2. Run the Binary Search Solver
# We test every 2 days, from a luxurious 25-day notice down to a panic 5-day notice
t_start_test_values <- seq(25, 4, by = -1)

staffing_curve <- find_required_capacity(
  sim_env = sim_env,
  score_csv = score_csv,
  t_start_values = t_start_test_values,
  target_p_backlog = 0.001, # We want a 99.9% of the backlog
  max_techs = 30,       # Never borrow more than 30 techs
  total_units = 50,
  n_iterations = 1000    # 500 is a good balance of speed and MC stability
)

print(staffing_curve)
base_dir <- file.path("results", "sensitivity", "t_start")
saveRDS(staffing_curve, file = file.path(base_dir, "staffing_curve.rds"))

# 3. Plot the "Staffing Curve"
ggplot(staffing_curve, aes(x = t_start, y = required_techs)) +
  geom_step(color = "#8b0000", linewidth = 1.2, direction = "vh") +
  geom_point(shape = 21, fill = "white", color = "#8b0000", size = 3, stroke = 1.2) +
  scale_x_reverse() + # Reverse X-axis so time counts down to the right
  scale_y_continuous(breaks = seq(0, 30, by = 2)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Prescriptive Staffing Curve (99% Pass Guarantee)",
    subtitle = "Minimum required technicians as the preparation window shrinks",
    x = "Days of Notice (t_start)",
    y = "Required Technicians"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )