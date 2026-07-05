# ==========================================
# NSPIRE Monte Carlo Simulation Engine
# ==========================================

#' Beta-PERT Random Number Generator (Vectorized)
#' @param n Integer. Number of observations.
#' @param a Numeric vector. Minimum values.
#' @param m Numeric vector. Mode values.
#' @param b Numeric vector. Maximum values.
#' @return A numeric vector.
#' @importFrom stats rbeta
#' @export
rpert <- function(n, a, m, b) {
  res <- numeric(n)
  fixed_idx <- (a == b)
  if (any(fixed_idx)) res[fixed_idx] <- a[fixed_idx]
  
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
#' @param sim_env simulation environment
#' @param total_units total number of units in building
#' @export
generate_building <- function(sim_env, total_units) {
  archetypes <- names(sim_env@building_mix)
  probs <- as.numeric(sim_env@building_mix)
  sample(x = archetypes, size = total_units, replace = TRUE, prob = probs)
}

#' Run NSPIRE Inspection Simulation
#' @param sim_env simulation environment
#' @param total_units total number of units in building
#' @param seed RNG seed
#' @export
run_simulation <- function(sim_env, total_units = 100, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  building_units <- generate_building(sim_env, total_units)
  scen <- sim_env@scenario

  clean_prop <- scen$tenant_factors$clean_proportion %||% 0.80
  clean_mean <- scen$tenant_factors$clean_multiplier_mean %||% 1.0
  dirty_mean <- scen$tenant_factors$dirty_multiplier_mean %||% 3.0
  
  is_clean <- stats::runif(total_units) <= clean_prop
  tenant_factors <- numeric(total_units)
  
  if(any(is_clean)) tenant_factors[is_clean] <- pmax(0.1, stats::rnorm(sum(is_clean), mean = clean_mean, sd = 0.2))
  if(any(!is_clean)) tenant_factors[!is_clean] <- pmax(1.0, stats::rnorm(sum(!is_clean), mean = dirty_mean, sd = 0.5))
  
  units_df <- data.frame(
    unit_id = paste0("Unit_", seq_along(building_units)),
    unit_type = building_units,
    tenant_factor = tenant_factors,
    stringsAsFactors = FALSE
  )
  
  long_fp <- sim_env@long_floorplans
  master <- merge(units_df, long_fp, by = "unit_type", all.x = TRUE)
  master <- master[rep(seq_len(nrow(master)), master$item_count), ]
  
  tasks <- sim_env@dag@tasks_config[, c("task_id", "category", "severity",
                                        "p_defect", "p_miss_base", "t1_a", "t1_m", "t1_b", "cost1")]
  master <- merge(master, tasks, by = "task_id", all.x = TRUE)
  
  adjusted_p_defect <- pmin(1.0, master$p_defect * master$tenant_factor)
  is_defective <- stats::runif(nrow(master)) < adjusted_p_defect
  
  defects <- master[is_defective, ]
  
  if(nrow(defects) == 0) {
    return(data.frame(unit_id=character(), unit_type=character(), task_id=character(),
                      category=character(), severity=character(), tenant_factor=numeric(),
                      p_defect=numeric(), is_caught=logical(),
                      repair_time_mins=numeric(), material_cost=numeric()))
  }
  
  defects$is_caught <- stats::runif(nrow(defects)) >= defects$p_miss_base
  defects$repair_time_mins <- round(rpert(nrow(defects), defects$t1_a, defects$t1_m, defects$t1_b), 2)
  
  eff_mult <- scen$staffing$efficiency_multiplier %||% 1.0
  defects$repair_time_mins <- defects$repair_time_mins / eff_mult
  
  results_df <- data.frame(
    unit_id = defects$unit_id, unit_type = defects$unit_type, task_id = defects$task_id,
    category = defects$category, severity = defects$severity, tenant_factor = defects$tenant_factor,
    p_defect = defects$p_defect, is_caught = defects$is_caught,
    repair_time_mins = defects$repair_time_mins, material_cost = defects$cost1,
    stringsAsFactors = FALSE
  )
  
  unit_nums <- as.numeric(gsub("Unit_", "", results_df$unit_id))
  results_df <- results_df[order(unit_nums, results_df$task_id), ]
  rownames(results_df) <- NULL
  return(results_df)
}

#' Schedule Workforce Repairs (Sequential Unit-Block Assignment)
#' @param defects_df dataframe of defects
#' @param sim_env simulation environment
#' @param t_prep_days total available preparation days
#' @export
schedule_repairs <- function(defects_df, sim_env, t_prep_days = 30) {
  if (nrow(defects_df) == 0) return(defects_df)
  
  scen <- sim_env@scenario
  num_techs <- scen$staffing$num_techs %||% 5
  hours_per_day <- scen$staffing$hours_per_day %||% 8
  max_ot_per_tech <- scen$staffing$max_ot_hours_per_tech %||% 2
  base_rate   <- scen$staffing$hourly_rate_internal
  ot_rate     <- base_rate * scen$staffing$overtime_multiplier
  
  # 1. Total capacity per tech in the prep window
  
  total_reg_cap_hrs <- hours_per_day * t_prep_days
  total_ot_cap_hrs  <- max_ot_per_tech * t_prep_days
  total_tech_cap    <- total_reg_cap_hrs + total_ot_cap_hrs
  
  # 2. Aggregate time required per unit
  
  unit_summaries <- aggregate(repair_time_mins ~ unit_id, data = defects_df, sum)
  unit_summaries$repair_time_hrs <- unit_summaries$repair_time_mins / 60
  unit_summaries <- unit_summaries[order(unit_summaries$unit_id), ]
  
  # 3. Partition units across tech pool (Round-robin)
  
  n_units <- nrow(unit_summaries)
  unit_summaries$assigned_tech <- (seq_len(n_units) - 1) %% num_techs
  
  # 4. Process queues for each tech
  results_costs <- list()    # <-- Add this!
  results_defects <- list()
  
  results <- list()
  for (t_id in 0:(num_techs - 1)) {
    tech_queue <- unit_summaries[unit_summaries$assigned_tech == t_id, ]
    tech_queue$cum_time_hrs <- cumsum(tech_queue$repair_time_hrs)
    tech_queue$unit_start_hrs <- tech_queue$cum_time_hrs - tech_queue$repair_time_hrs
    
    # Vectorized Capacity Bucketing
    tech_queue$reg_hours <- pmin(tech_queue$repair_time_hrs, 
                                 pmax(0, total_reg_cap_hrs - tech_queue$unit_start_hrs))
    
    tech_queue$ot_hours <- pmin(tech_queue$repair_time_hrs - tech_queue$reg_hours,
                                pmax(0, total_ot_cap_hrs - pmax(0, tech_queue$unit_start_hrs - total_reg_cap_hrs)))
    
    # Calculate costs for this tech's queue
    tech_queue$labor_cost    <- round((tech_queue$reg_hours * base_rate) + (tech_queue$ot_hours * ot_rate), 2)
    tech_queue$overtime_cost <- round(tech_queue$ot_hours * ot_rate, 2)
    tech_queue$is_backlogged <- tech_queue$cum_time_hrs > total_tech_cap
    
    # Now map these results back to the individual defects within these units
    # We create a mapping subset for this tech
    tech_meta <- tech_queue[, c("unit_id", "labor_cost", "overtime_cost", "is_backlogged")]
    tech_defects <- defects_df[defects_df$unit_id %in% tech_queue$unit_id, ]
    
    results_costs[[t_id + 1]] <- tech_queue[, c("unit_id", "labor_cost", "overtime_cost", "is_backlogged")]
    results[[t_id + 1]] <- merge(tech_defects, tech_meta, by = "unit_id")
  }
  
  return(list(
    costs = do.call(rbind, results_costs),
    defects = do.call(rbind, results_defects)
  ))
}

# small helper
`%||%` <- function(a, b) if (!is.null(a)) a else b