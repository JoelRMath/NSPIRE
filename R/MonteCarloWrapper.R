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
#' @param seed Integer. Optional random seed for reproducible distributions.
#' @return A \code{data.frame} where each row represents the macro outcomes of a single simulation.
#' @export
run_monte_carlo <- function(n_iterations = 1000, sim_env, score_csv, rubric = "NSPIRE", 
                            total_units = 50, audit_catch_rate = 0.8, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Pre-allocate memory for speed
  scores         <- numeric(n_iterations)
  deductions     <- numeric(n_iterations)
  labor_hours    <- numeric(n_iterations)
  overtime_hours <- numeric(n_iterations)
  material_costs <- numeric(n_iterations)
  iteration_times<- numeric(n_iterations) # NEW: Track time per iteration
  
  # Start the performance timer
  start_time <- Sys.time()
  cat("Starting", n_iterations, "Monte Carlo iterations...\n")
  
  # Calculate reporting intervals (e.g., report every 10%)
  report_interval <- max(1, floor(n_iterations / 10))
  
  for (i in 1:n_iterations) {
    iter_start <- Sys.time() # Start iteration clock
    
    # 1. Physical Environment Generation
    raw_defects <- run_simulation(sim_env, total_units = total_units)
    
    # 2. Scheduling & Financial Constraints
    scheduled <- schedule_repairs(raw_defects, sim_env)
    
    if (nrow(scheduled) > 0) {
      labor_hours[i] <- sum(scheduled$repair_time_mins) / 60
      material_costs[i] <- sum(scheduled$material_cost)
      
      # Tally overtime specifically
      ot_mins <- sum(scheduled$repair_time_mins[scheduled$is_overtime])
      overtime_hours[i] <- ot_mins / 60
    } else {
      labor_hours[i] <- 0
      material_costs[i] <- 0
      overtime_hours[i] <- 0
    }
    
    # 3. Internal Audit Walk
    audited <- apply_audit_fixes(raw_defects, audit_catch_rate)
    
    # 4. Official HUD Score Calculation
    score_report <- calculate_inspection_score(audited, score_csv, rubric = rubric, 
                                               total_units_inspected = total_units)
    
    scores[i] <- score_report$final_score
    deductions[i] <- score_report$total_deductions
    
    # Stop iteration clock
    iter_end <- Sys.time()
    iteration_times[i] <- as.numeric(difftime(iter_end, iter_start, units = "secs"))
    
    # Lightweight progress reporting
    if (i %% report_interval == 0) {
      cat(sprintf("... Completed %d / %d iterations\n", i, n_iterations))
    }
  }
  
  # Stop total timer and calculate execution speed
  end_time <- Sys.time()
  exec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("========================================\n")
  cat("Completed in", round(exec_time, 2), "seconds.\n")
  cat("Average speed:", round(mean(iteration_times) * 1000, 2), "ms per iteration.\n")
  cat("========================================\n")
  
  # Return the consolidated distribution dataset
  return(data.frame(
    iteration = 1:n_iterations,
    final_score = scores,
    total_deductions = deductions,
    labor_hours = round(labor_hours, 2),
    overtime_hours = round(overtime_hours, 2),
    material_cost = material_costs,
    iteration_time_sec = round(iteration_times, 4)
  ))
}