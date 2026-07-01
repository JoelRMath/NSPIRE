# ==========================================
# 1. The Simulation Environment Class Definition
# ==========================================

#' InspectionSimulation Class
#'
#' An S4 class representing the complete environment for an NSPIRE Monte Carlo simulation.
#'
#' @slot dag An object of class \code{InspectionDAG} representing the task dependencies.
#' @slot floorplans A \code{data.frame} containing the physical item counts per unit archetype.
#' @slot scenario A \code{list} containing the parsed YAML operational strategy parameters.
#' @slot building_mix A \code{numeric} vector of normalized probabilities for unit archetypes.
#' @export
setClass(
  "InspectionSimulation",
  slots = list(
    dag          = "InspectionDAG", # The mathematical graph
    floorplans   = "data.frame",    # Unit spatial configurations
    scenario     = "list",          # Parsed YAML strategy parameters
    building_mix = "numeric",       # Normalized probabilities for unit archetypes
    topo_nodes   = "character",     # Cached topological sort
    long_floorplans = "data.frame"  # Cached long-format physical items
  )
)

# ==========================================
# 2. The Constructor Function
# ==========================================

#' Create a Simulation Environment
#'
#' Validates and constructs an \code{InspectionSimulation} object from raw configuration files.
#'
#' @param dag_csv Path to the tasks configuration CSV file.
#' @param floorplan_csv Path to the floorplans configuration CSV file.
#' @param yaml_path Path to the scenario configuration YAML file.
#' @param mix_weights A named numeric vector representing the distribution of unit types.
#' 
#' @return An object of class \code{InspectionSimulation}.
#' @importFrom methods new
#' @export
create_simulation_env <- function(dag_csv, floorplan_csv, yaml_path, mix_weights) {
  
  # 1. Instantiate the mathematical DAG
  my_dag <- create_inspection_dag(dag_csv)
  
  # 2. Load the physical floorplan item counts
  fp_df <- read.csv(floorplan_csv, stringsAsFactors = FALSE, strip.white = TRUE, check.names = FALSE)
  
  # Validate that the DAG and Floorplans match
  missing_tasks <- setdiff(my_dag@tasks_config$task_id, fp_df$task_id)
  if (length(missing_tasks) > 0) {
    stop("CRITICAL ERROR: Tasks in DAG are missing from floorplans_config.csv: ", 
         paste(missing_tasks, collapse = ", "))
  }
  
  # Pre-calculate Topological Sort
  topo_nodes <- names(igraph::topo_sort(my_dag@graph))
  
  # Pre-shape Floorplans to long format
  archetypes <- names(mix_weights)
  long_fp_list <- lapply(archetypes, function(arch) {
    data.frame(
      unit_type = arch,
      task_id = fp_df$task_id,
      item_count = fp_df[[arch]],
      stringsAsFactors = FALSE
    )
  })
  long_fp <- do.call(rbind, long_fp_list)
  long_fp <- long_fp[long_fp$item_count > 0, ]
  
  # 3. Load the operational strategy YAML
  if (!file.exists(yaml_path)) {
    stop("CRITICAL ERROR: YAML configuration file not found at ", yaml_path)
  }
  scenario_config <- yaml::read_yaml(yaml_path)
  
  # 4. Normalize the building mix probabilities (ensure they sum to 1.0)
  if (is.null(names(mix_weights))) {
    stop("CRITICAL ERROR: mix_weights must be a named numeric vector (e.g., c('Type_A'=0.4, 'Type_B'=0.6))")
  }
  mix_weights <- mix_weights / sum(mix_weights)
  
  # Return the fully validated S4 Object
  new("InspectionSimulation",
      dag          = my_dag,
      floorplans   = fp_df,
      scenario     = scenario_config,
      building_mix = mix_weights,
      topo_nodes   = topo_nodes,
      long_floorplans = long_fp)
}

# ==========================================
# 3. The Custom Show Method (Console Dashboard)
# ==========================================

#' Show method for InspectionSimulation
#' 
#' Prints a clean dashboard summary of the simulation environment parameters.
#' 
#' @param object An \code{InspectionSimulation} object.
#' @importFrom methods show
#' @exportMethod show
setMethod("show", "InspectionSimulation", function(object) {
  scen <- object@scenario
  
  cat("====================================================\n")
  cat("             NSPIRE Simulation Environment          \n")
  cat("====================================================\n")
  cat("Scenario    :", scen$scenario_name, "\n")
  cat("Description :", scen$description, "\n")
  cat("----------------------------------------------------\n")
  cat("Operational Strategy:\n")
  cat("  - Prep Window     :", scen$temporal_strategy$days_before_inspection, "Days Out\n")
  cat("  - Techs per Unit  :", scen$staffing$techs_per_unit, "\n")
  cat("  - Synergy Bonus   :", scen$staffing$efficiency_multiplier, "\n")
  cat("  - Audit Catch Rate:", sprintf("%.1f%%", scen$workflow$audit_catch_rate * 100), "\n")
  cat("----------------------------------------------------\n")
  cat("Financials & Constraints:\n")
  cat("  - Base Rate       : $", sprintf("%.2f", scen$staffing$hourly_rate_internal), "/hr\n", sep="")
  cat("  - Regular Limit   :", scen$temporal_strategy$regular_capacity_hours_per_unit, "hrs/unit\n")
  cat("  - Max Overtime    :", scen$staffing$max_overtime_hours_per_unit, "hrs/unit @", 
      scen$staffing$overtime_multiplier, "x Rate\n")
  cat("----------------------------------------------------\n")
  cat("Building Demographics:\n")
  cat("  - Historic Tickets:", scen$temporal_strategy$historical_tickets_lambda, "per year (Average)\n")
  cat("  - Unit Mix        : ")
  
  # Print the unit mix cleanly (e.g., Type_A: 40%, Type_B: 60%)
  mix_strings <- paste0(names(object@building_mix), ": ", 
                        round(object@building_mix * 100, 1), "%")
  cat(paste(mix_strings, collapse = ", "), "\n")
  cat("====================================================\n")
})