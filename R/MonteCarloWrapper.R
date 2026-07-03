# ==========================================
# NSPIRE Monte Carlo Wrapper
# ==========================================

#' Run Full Monte Carlo Distribution
#'
#' Executes the complete simulation lifecycle (Physical generation, Scheduling, 
#' Audit, and Scoring) N times to generate statistical distributions of outcomes.
#'
#' @param n_iterations Integer. Number of simulations to run.
#' @param sim_env An \code{InspectionSimulation} object.
#' @param score_csv Path to the scoring rubric configuration CSV.
#' @param rubric Character. The scoring rubric to use (default "NSPIRE").
#' @param total_units Integer. Number of units to spawn per iteration.
#' @param audit_catch_rate Numeric. Probability that the internal team catches a defect.
#' @param t_start Integer. Days before inspection that prep work begins.
#' @param t_end Integer. Days before inspection that prep work ends.
#' @param seed Integer. Optional random seed for reproducible distributions.
#' @return A \code{data.frame} where each row represents the macro outcomes of a single simulation.
#' @export
run_monte_carlo <- function(n_iterations = 1000, sim_env, score_csv, rubric = "NSPIRE", 
                            total_units = 50, audit_catch_rate = 0.8, 
                            t_start = 30, t_end = 5, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Pre-allocate memory for speed
  scores           <- numeric(n_iterations)
  deductions       <- numeric(n_iterations)
  labor_hours      <- numeric(n_iterations)
  overtime_hours   <- numeric(n_iterations)
  material_costs   <- numeric(n_iterations)
  backlogged_count <- numeric(n_iterations)
  iteration_times  <- numeric(n_iterations) 
  
  start_time <- Sys.time()
  cat("Starting", n_iterations, "Monte Carlo iterations...\n")
  
  report_interval <- max(1, floor(n_iterations / 10))
  
  # Calculate available prep days (minimum 1 to avoid divide by zero errors)
  t_prep_days <- max(1, t_start - t_end)
  
  for (i in 1:n_iterations) {
    iter_start <- Sys.time() 
    
    # 1. Physical Environment Generation
    raw_defects <- run_simulation(sim_env, total_units = total_units)
    
    # 2. Scheduling & Financial Constraints
    # Pass t_prep_days to scale labor capacity
    scheduled <- schedule_repairs(raw_defects, sim_env, t_prep_days = t_prep_days)
    
    if (nrow(scheduled) > 0) {
      labor_hours[i] <- sum(scheduled$repair_time_mins) / 60
      material_costs[i] <- sum(scheduled$material_cost)
      
      # Tally overtime specifically
      ot_mins <- sum(scheduled$repair_time_mins[scheduled$is_overtime])
      overtime_hours[i] <- ot_mins / 60
      
      # Tally backlogged items
      backlogged_count[i] <- sum(scheduled$is_backlogged)
    } else {
      labor_hours[i] <- 0
      material_costs[i] <- 0
      overtime_hours[i] <- 0
      backlogged_count[i] <- 0
    }
    
    # 3. Internal Audit Walk (Pass t_end for decay math!)
    audited <- apply_audit_fixes(raw_defects, audit_catch_rate, t_end = t_end)
    
    # 4. Official HUD Score Calculation
    # Pass total_units_in_building for the random sampling logic
    score_report <- calculate_inspection_score(audited, score_csv, rubric = rubric, 
                                               total_units_in_building = total_units)
    
    scores[i] <- score_report$final_score
    deductions[i] <- score_report$total_deductions
    
    iter_end <- Sys.time()
    iteration_times[i] <- as.numeric(difftime(iter_end, iter_start, units = "secs"))
    
    if (i %% report_interval == 0) {
      cat(sprintf("... Completed %d / %d iterations\n", i, n_iterations))
    }
  }
  
  # Return the consolidated distribution dataset
  return(data.frame(
    iteration = 1:n_iterations,
    final_score = scores,
    total_deductions = deductions,
    labor_hours = round(labor_hours, 2),
    overtime_hours = round(overtime_hours, 2),
    material_cost = material_costs,
    backlogged_defects = backlogged_count,
    iteration_time_sec = round(iteration_times, 4)
  ))
}