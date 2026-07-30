# ==========================================
# 1. Stochastic Helpers
# ==========================================

#' Beta-PERT Random Number Generator (Vectorized)
#'
#' Generates random execution times based on a 3-point estimation technique 
#' widely used in project management. The function is fully vectorized, allowing 
#' it to sample thousands of tasks simultaneously without slow `for` loops.
#'
#' @param n Integer. Number of random draws to generate.
#' @param a Numeric vector. The optimistic (minimum) time estimate.
#' @param m Numeric vector. The most likely (mode) time estimate.
#' @param b Numeric vector. The pessimistic (maximum) time estimate.
#' @return A numeric vector of simulated execution times.
#' @export
rpert <- function(n, a, m, b) {
  res <- numeric(n)
  
  # Handle deterministic edge cases where min == max (no variance)
  fixed_idx <- (a == b)
  if (any(fixed_idx)) res[fixed_idx] <- a[fixed_idx]
  
  # Calculate Beta shape parameters and draw for variable tasks
  var_idx <- !fixed_idx
  if (any(var_idx)) {
    a_v <- a[var_idx]; m_v <- m[var_idx]; b_v <- b[var_idx]
    alpha <- 1 + 4 * ((m_v - a_v) / (b_v - a_v))
    beta_param  <- 1 + 4 * ((b_v - m_v) / (b_v - a_v))
    res[var_idx] <- a_v + stats::rbeta(sum(var_idx), alpha, beta_param) * (b_v - a_v)
  }
  return(res)
}

#' Generate Building Demographics
#'
#' Generates a simulated building by sampling unit archetypes based on 
#' the normalized demographic probabilities defined in the scenario YAML.
#'
#' @param sim_env An \code{InspectionSimulation} object.
#' @param total_units Integer. Total number of apartments to generate.
#' @return A character vector of unit archetypes (e.g., c("1BR", "2BR", "1BR")).
#' @export
generate_building <- function(sim_env, total_units) {
  archetypes <- names(sim_env@building_mix)
  probs <- as.numeric(sim_env@building_mix)
  sample(x = archetypes, size = total_units, replace = TRUE, prob = probs)
}

# ==========================================
# 2. Core Physics & Defect Generation
# ==========================================

#' Run NSPIRE Inspection Simulation (Physics Engine)
#'
#' Generates the raw physical defects for a building. It applies spatial heterogeneity 
#' (the tenant "Lottery Effect") by creating clean and dirty units, ensuring that the 
#' overall building maintains a strict "Conservation of Defects" across iterations.
#'
#' @param sim_env An \code{InspectionSimulation} object.
#' @param total_units Integer. Number of units to simulate (default: 100).
#' @param seed Integer. Optional random seed for reproducibility.
#' @return A \code{data.frame} of instantiated physical defects requiring repair.
#' @export
run_simulation <- function(sim_env, total_units = 100, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  building_units <- generate_building(sim_env, total_units)
  scen <- sim_env@scenario
  
  # Establish spatial heterogeneity constraints
  clean_prop <- scen$tenant_factors$clean_proportion %||% 0.80
  
  # Use a multiplier ratio instead of hardcoded absolute means for flexibility
  severity_ratio <- scen$tenant_factors$dirty_severity_ratio %||% 5.0
  
  # Dynamically balance the means to enforce "Conservation of Defects".
  # If destructive units are extremely severe, standard units must become 
  # mathematically cleaner to keep the global expected severity fixed at 1.0.
  clean_mean <- 1.0 / (clean_prop + severity_ratio * (1 - clean_prop))
  dirty_mean <- clean_mean * severity_ratio
  
  # Assign units to binary latent states (Clean vs. Dirty)
  is_clean <- stats::runif(total_units) <= clean_prop
  tenant_factors <- numeric(total_units)
  
  # Scale Standard Deviations proportionally to prevent pmax() from biasing the global mean
  if(any(is_clean)) {
    tenant_factors[is_clean] <- pmax(0.01, stats::rnorm(sum(is_clean), mean = clean_mean, sd = 0.2 * clean_mean))
  }
  if(any(!is_clean)) {
    tenant_factors[!is_clean] <- pmax(0.01, stats::rnorm(sum(!is_clean), mean = dirty_mean, sd = 0.2 * dirty_mean))
  }
  
  # Build the base unit dataframe
  units_df <- data.frame(
    unit_id = paste0("Unit_", seq_along(building_units)),
    unit_type = building_units,
    tenant_factor = tenant_factors,
    stringsAsFactors = FALSE
  )
  
  # Merge units with the long-format floorplan to spawn physical items inside the units
  long_fp <- sim_env@long_floorplans
  master <- merge(units_df, long_fp, by = "unit_type", all.x = TRUE)
  # Expand the dataframe so every individual physical item gets its own row
  master <- master[rep(seq_len(nrow(master)), master$item_count), ]
  
  # Attach task probabilities, execution times, and costs from the DAG config
  tasks <- sim_env@dag@tasks_config[, c("task_id", "category", "severity", "p_defect", "p_miss_base", "t1_a", "t1_m", "t1_b", "cost1")]
  master <- merge(master, tasks, by = "task_id", all.x = TRUE)
  
  # Evaluate stochastic failure: Does the item break? (Cap probability at 1.0)
  adjusted_p_defect <- pmin(1.0, master$p_defect * master$tenant_factor)
  is_defective <- stats::runif(nrow(master)) < adjusted_p_defect
  
  # Filter down to only the broken items
  defects <- master[is_defective, ]
  
  # Edge case handling: Pristine building with zero defects
  if(nrow(defects) == 0) {
    return(data.frame(unit_id=character(), unit_type=character(), task_id=character(),
                      category=character(), severity=character(), tenant_factor=numeric(),
                      p_defect=numeric(), is_caught=logical(),
                      repair_time_mins=numeric(), material_cost=numeric()))
  }
  
  # Generate task-specific stochastic outcomes
  defects$is_caught <- stats::runif(nrow(defects)) >= defects$p_miss_base
  defects$repair_time_mins <- round(rpert(nrow(defects), defects$t1_a, defects$t1_m, defects$t1_b), 2)
  
  # Package the final raw defect payload
  results_df <- data.frame(
    unit_id = defects$unit_id, unit_type = defects$unit_type, task_id = defects$task_id,
    category = defects$category, severity = defects$severity, tenant_factor = defects$tenant_factor,
    p_defect = defects$p_defect, is_caught = defects$is_caught,
    repair_time_mins = defects$repair_time_mins, material_cost = defects$cost1,
    stringsAsFactors = FALSE
  )
  
  # Sort cleanly by Unit ID for downstream scheduling
  unit_nums <- as.numeric(gsub("Unit_", "", results_df$unit_id))
  results_df <- results_df[order(unit_nums, results_df$task_id), ]
  rownames(results_df) <- NULL
  
  return(results_df)
}

# ==========================================
# 3. Logistics & Execution Engine
# ==========================================

#' Schedule Repairs with Context-Aware Transit Friction
#'
#' Evaluates the raw defects, sorts them by spatial layout and DAG topology, 
#' and calculates the operational schedule. It natively injects stochastic transit 
#' friction based on whether a technician is transitioning between tasks in the same 
#' unit or packing up to move to a new apartment. 
#' 
#' @details This function is heavily vectorized to calculate complex financial straddles 
#' (e.g., a task starting in regular time but finishing in overtime) across thousands 
#' of tasks instantly.
#'
#' @param defects A data.frame of raw defects generated by run_simulation.
#' @param sim_env An \code{InspectionSimulation} environment object.
#' @param t_prep_days Numeric. Optional override for the timeline constraints in the YAML.
#' @return A data.frame enriched with scheduling cumulative times, friction penalties, and financial calculations.
#' @export
schedule_repairs <- function(defects, sim_env, t_prep_days = NULL) {
  if (nrow(defects) == 0) return(data.frame())
  
  # 1. Sort by topological order, grouped strictly by unit to minimize travel
  defects$topo_rank <- match(defects$task_id, sim_env@topo_nodes)
  defects <- defects[order(defects$unit_id, defects$topo_rank), ]
  
  # 2. Extract Scenario Constraints (with bulletproof fallbacks to prevent crashes)
  scen <- sim_env@scenario
  
  # Helper to safely extract values that might be NULL or length 0
  safe_extract <- function(x, default) {
    if (is.null(x) || length(x) == 0 || is.na(x)) default else x
  }
  
  reg_hours <- safe_extract(scen$temporal_strategy$regular_capacity_hours_per_unit, 8)
  ot_hours  <- safe_extract(scen$staffing$max_overtime_hours_per_unit, 4)
  techs     <- safe_extract(scen$staffing$techs_per_unit, 1)
  base_rate <- safe_extract(scen$staffing$hourly_rate_internal, 45) / 60
  ot_rate   <- base_rate * safe_extract(scen$staffing$overtime_multiplier, 1.5)
  
  daily_reg_mins <- reg_hours * 60
  daily_ot_mins  <- ot_hours * 60
  
  # Allow dynamic override for sensitivity sweeps, fallback to YAML
  days_available <- safe_extract(t_prep_days, safe_extract(scen$temporal_strategy$days_before_inspection, 30))
  
  total_reg_capacity <- daily_reg_mins * days_available * techs
  total_ot_capacity  <- daily_ot_mins * days_available * techs
  absolute_capacity  <- total_reg_capacity + total_ot_capacity
  
  # 3. Vectorized Context-Aware Transit Friction
  n_tasks <- nrow(defects)
  
  # Determine if the tech is moving to a new unit (TRUE) or staying in the same unit (FALSE)
  # The very first task of the simulation is always a "new unit" dispatch.
  is_new_unit <- c(TRUE, defects$unit_id[-1] != defects$unit_id[-n_tasks])
  
  # Extract friction distribution parameters from scenario with fallbacks
  f_intra <- scen$workflow$friction_intra
  f_inter <- scen$workflow$friction_inter
  
  # Generate stochastic delay distributions for the entire batch at once
  friction_intra <- rpert(
    n = n_tasks, 
    a = f_intra$min, 
    m = f_intra$mode, 
    b = f_intra$max
  )
  
  friction_inter <- rpert(
    n = n_tasks, 
    a = f_inter$min, 
    m = f_inter$mode, 
    b = f_inter$max
  )
  
  # Assign rigid micro-delays based on the technician's context (same unit vs new unit)
  defects$friction_mins <- ifelse(is_new_unit, friction_inter, friction_intra)
  
  # Calculate cumulative schedule instantly using vectorized cumsum
  task_total_time <- defects$friction_mins + defects$repair_time_mins
  defects$cumulative_time_mins <- cumsum(task_total_time)
  
  # 4. Vectorized Financial & Backlog Thresholds
  defects$is_overtime <- defects$cumulative_time_mins > total_reg_capacity
  defects$is_backlogged <- defects$cumulative_time_mins > absolute_capacity
  
  # 5. Vectorized Billing Calculations
  end_time <- defects$cumulative_time_mins
  start_time <- end_time - task_total_time
  
  # Pre-calculate pure scenarios
  cost_reg <- task_total_time * base_rate
  cost_ot <- task_total_time * ot_rate
  
  # Calculate straddle scenario (task starts in regular hours, ends in overtime)
  reg_time_straddle <- total_reg_capacity - start_time
  ot_time_straddle <- task_total_time - reg_time_straddle
  cost_straddle <- (reg_time_straddle * base_rate) + (ot_time_straddle * ot_rate)
  
  # Apply billing logic safely via nested ifelse
  defects$labor_cost <- ifelse(
    end_time <= total_reg_capacity, cost_reg,
    ifelse(start_time >= total_reg_capacity, cost_ot, cost_straddle)
  )
  
  # Isolate just the overtime premium/spend for reporting
  defects$overtime_cost <- ifelse(
    end_time <= total_reg_capacity, 0,
    ifelse(start_time >= total_reg_capacity, cost_ot, ot_time_straddle * ot_rate)
  )
  
  # Zero out costs for backlogged items (abandoned work incurs no labor expense)
  defects$labor_cost[defects$is_backlogged] <- 0
  defects$overtime_cost[defects$is_backlogged] <- 0
  
  return(defects)
}

# Fallback helper operator for null checking
`%||%` <- function(a, b) if (!is.null(a)) a else b