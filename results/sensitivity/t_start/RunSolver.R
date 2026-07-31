#' Prescriptive Analytics: Staffing Curve Solver
#'
#' Execution script: inverts the simulation framework to answer a 
#' prescriptive question: Given a shrinking inspection notice window 
#' (t_start), how many technicians are required to prevent a task backlog?
#' By wrapping a binary search algorithm around the Monte Carlo engine, the script
#' maps the non-linear (exponential-like) relationship between time compression and 
#' required labor capacity. 
#' 
#' Important: in order to have a monotonic function (required for binary search) the task
#' backlog is used. This is not the case when using a target score instead of backlog.
#' 
#' Note: this script uses find_required_capacity() which is implemented in Tools.R

# ==============================================
# Prescriptive Analytics: Staffing Curve Solver
# ==============================================

devtools::load_all(".")
library(ggplot2)

# 1. Setup the Environment
# Load the baseline physical and operational configurations
dag_csv <- "inst/extdata/tasks_config.csv"
fp_csv  <- "inst/extdata/floorplans_config.csv"
yaml_path <- "inst/extdata/scenario1.yml"
score_csv <- "inst/extdata/scoring_config.csv"

# Define the baseline demographic distribution of the property
mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)

# 2. Run the Binary Search Solver
# We sweep the timeline day-by-day, from a luxurious 25-day notice down to a panic 4-day notice.
t_start_test_values <- seq(25, 4, by = -1)

# Execute the binary search optimization.
# For each day in the sequence, the algorithm tests different crew sizes, 
# executing 1,000 Monte Carlo simulations per guess, to find the absolute 
# minimum headcount required to satisfy the backlog constraint.
staffing_curve <- find_required_capacity(
  sim_env = sim_env,
  score_csv = score_csv,
  t_start_values = t_start_test_values,
  target_p_backlog = 0.001, # Strict operational threshold: 99.9% probability of clearing all work
  max_techs = 30,           # Upper bound for the binary search (do not hire more than 30 techs)
  total_units = 50,
  n_iterations = 1000       # High resolution to ensure the tail-risk stability of the 99.9% target
)

print(staffing_curve)

# Save the mathematical curve to disk for downstream Quarto rendering
base_dir <- file.path("results", "sensitivity", "t_start")
saveRDS(staffing_curve, file = file.path(base_dir, "staffing_curve.rds"))

# 3. Plot the "Staffing Curve"
# Generates the visual proof of the "Non-Linear Staffing Surge" concept.
ggplot(staffing_curve, aes(x = t_start, y = required_techs)) +
  geom_step(color = "#8b0000", linewidth = 1.2, direction = "vh") +
  geom_point(shape = 21, fill = "white", color = "#8b0000", size = 3, stroke = 1.2) +
  scale_x_reverse() + # Reverse X-axis so time counts down to the right (timeline compressing)
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