# ==========================================
# NSPIRE Monte Carlo Simulation Engine
# ==========================================

#' Beta-PERT Random Number Generator (Vectorized)
#'
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
#' @export
generate_building <- function(sim_env, total_units) {
  archetypes <- names(sim_env@building_mix)
  probs <- as.numeric(sim_env@building_mix)
  sample(x = archetypes, size = total_units, replace = TRUE, prob = probs)
}

#' Run NSPIRE Inspection Simulation
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

#' Schedule Workforce Repairs
#' @export
schedule_repairs <- function(defects_df, sim_env, t_prep_days = 30) {
  if (nrow(defects_df) == 0) return(defects_df)
  
  topo_nodes <- sim_env@topo_nodes
  defects_df$task_factor <- factor(defects_df$task_id, levels = topo_nodes)
  defects_df <- defects_df[order(defects_df$unit_id, defects_df$task_factor), ]
  defects_df$task_factor <- NULL
  
  scen <- sim_env@scenario
  reg_cap_hrs <- scen$temporal_strategy$regular_capacity_hours_per_unit * t_prep_days
  max_ot_hrs  <- scen$staffing$max_overtime_hours_per_unit * t_prep_days
  base_rate   <- scen$staffing$hourly_rate_internal
  ot_rate     <- base_rate * scen$staffing$overtime_multiplier
  
  defects_df$cumulative_time_mins <- ave(defects_df$repair_time_mins, defects_df$unit_id, FUN = cumsum)
  task_time_hrs <- defects_df$repair_time_mins / 60
  cum_time_hrs <- defects_df$cumulative_time_mins / 60
  time_before_task <- cum_time_hrs - task_time_hrs
  
  reg_hours <- pmin(task_time_hrs, pmax(0, reg_cap_hrs - time_before_task))
  ot_hours <- task_time_hrs - reg_hours
  
  defects_df$labor_cost <- round((reg_hours * base_rate) + (ot_hours * ot_rate), 2)
  defects_df$overtime_cost <- round(ot_hours * ot_rate, 2)
  defects_df$is_overtime <- (ot_hours > 0)
  defects_df$is_backlogged <- (cum_time_hrs > (reg_cap_hrs + max_ot_hrs))
  return(defects_df)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b