#' Master Orchestrator for NSPIRE Monte Carlo Simulation
#'
#' Executes thousands of "virtual inspections" by repeatedly simulating property decay, 
#' scheduling maintenance, applying internal audits, and calculating HUD scores. 
#' This function acts as the central hub, passing data sequentially through the 
#' four functional pillars of the simulation engine.
#'
#' @param n_iterations Integer. The number of Monte Carlo simulations to execute (default: 1000).
#' @param sim_env An \code{InspectionSimulation} object. The validated environment containing the DAG, floorplans, and YAML parameters.
#' @param score_csv Character. File path to the CSV defining the regulatory scoring deduction weights.
#' @param rubric Character. The specific scoring rubric to use from the scoring configuration (default: "NSPIRE").
#' @param total_units Integer. The total number of apartments in the simulated building (default: 50).
#' @param audit_catch_rate Numeric. The probability [0, 1] that the internal team detects and fixes a defect (default: 0.8).
#' @param t_start Integer. The number of days of notice provided prior to the inspection (prep window start).
#' @param t_end Integer. The number of days before the inspection that maintenance ceases (prep window end).
#' @param seed Integer. Optional random seed for deterministic reproducibility across simulation runs.
#' @param save_raw Logical. If TRUE, saves the full raw defect data frame for every iteration. Use with caution as high \code{n_iterations} will consume massive memory.
#' 
#' @return A list containing two elements: \code{summary} (a data.frame of aggregated simulation statistics like score, labor cost, and backlog) and \code{raw} (a list of raw defect frames, if requested).
#' @export
run_monte_carlo <- function(n_iterations = 1000, sim_env, score_csv, rubric = "NSPIRE",
                            total_units = 50, audit_catch_rate = 0.8,
                            t_start = 30, t_end = 5, seed = NULL, save_raw = FALSE) {
  
  # Set the global seed once for the entire ensemble to guarantee reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  # Execute the core Monte Carlo loop across n_iterations using a highly optimized lapply
  all_iterations <- lapply(1:n_iterations, function(i) {
    
    # 1. Defect Generation: Roll the stochastic dice to create the raw physical baseline
    raw_defects <- run_simulation(sim_env, total_units = total_units)
    
    # 2. Schedule & Route: Apply physical constraints (DAG), transit friction, and labor limits
    # The available repair window is the difference between start notice and prep completion
    scheduled <- schedule_repairs(raw_defects, sim_env, t_prep_days = max(1, t_start - t_end))
    
    # 3. Audit & Fix: Apply quality control and simulate post-prep temporal decay
    # (BUG FIX: Ensure we use 'scheduled' so that backlogged items cannot magically be audited/fixed)
    audited <- apply_audit_fixes(scheduled, audit_catch_rate, t_end = t_end)
    
    # 4. Final Scoring: Run the HUD random sampling protocol and calculate the regulatory outcome
    score_report <- calculate_inspection_score(audited, score_csv, rubric = rubric, 
                                               total_units_in_building = total_units)
    
    # Package the iteration results into a standardized vector for fast matrix binding later
    return(list(
      summary = c(score = score_report$final_score,
                  labor = sum(scheduled$repair_time_mins) / 60,
                  total_labor_cost = sum(scheduled$labor_cost),
                  overtime_cost = sum(scheduled$overtime_cost),
                  backlog = sum(scheduled$is_backlogged)),
      raw_defects = if (save_raw) audited else NULL
    ))
    
  })
  
  # Aggregate summary matrix: Collapse the list of summary vectors into a single dataframe
  summary_mat <- do.call(rbind, lapply(all_iterations, `[[`, "summary"))
  df_results <- as.data.frame(summary_mat)
  
  # If forensic raw data was requested, extract and combine it into a list
  raw_list <- if (save_raw) lapply(all_iterations, `[[`, "raw_defects") else NULL
  
  return(list(summary = df_results, raw = raw_list))
}