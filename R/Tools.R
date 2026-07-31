
#' Prescriptive Analytics: Find Required Tech Headcount (Capacity Solver)
#'
#' Uses a high-speed binary search wrapped around the Monte Carlo engine to find 
#' the minimum number of technicians required to clear the known work queue.
#' It targets the probability of task backlog, decoupling labor limits from QA limits.
#'
#' @param sim_env The InspectionSimulation environment
#' @param score_csv Path to the scoring configuration CSV
#' @param t_start_values Numeric vector of prep days to evaluate (e.g., seq(5, 25, by=2))
#' @param target_p_backlog The maximum acceptable risk of incomplete work (e.g., 0.01 for 1%)
#' @param max_techs The upper bound for the binary search
#' @param total_units Number of units in the building to simulate
#' @param n_iterations Number of Monte Carlo iterations per step
#' @param audit_catch_rate Fixed audit efficacy to test under
#' 
#' @return A data.frame mapping t_start to required_techs and achieved backlog risk
#' @export
find_required_capacity <- function(sim_env, score_csv, t_start_values, 
                                   target_p_backlog = 0.01, max_techs = 30, 
                                   total_units = 50, n_iterations = 500, 
                                   audit_catch_rate = 0.85) {
  
  results_list <- list()
  
  cat(sprintf("\n=== Starting Prescriptive Capacity Solver ===\n"))
  cat(sprintf("Objective: Eliminate Labor Bottlenecks\n"))
  cat(sprintf("Target: P(Backlog > 0) <= %.1f%%\n", target_p_backlog * 100))
  
  for (t_start in t_start_values) {
    cat(sprintf("\nEvaluating t_start = %d days...\n", t_start))
    
    low <- 1
    high <- max_techs
    best_techs <- NA
    best_p_backlog <- NA
    
    # Fast Binary Search (Safe now because P(Backlog) monotonically decreases to 0)
    while (low <= high) {
      mid_techs <- floor((low + high) / 2)
      cat(sprintf("  Testing %d techs... ", mid_techs))
      
      # Temporarily override the tech capacity in a cloned environment
      temp_env <- sim_env
      temp_env@scenario$staffing$techs_per_unit <- mid_techs
      
      # Run Monte Carlo for this specific configuration
      mc_res <- run_monte_carlo(
        n_iterations = n_iterations,
        sim_env = temp_env,
        score_csv = score_csv,
        total_units = total_units,
        audit_catch_rate = audit_catch_rate,
        t_start = t_start,
        save_raw = FALSE # Keep memory usage light
      )
      
      # Calculate empirical probability that ANY tasks were left backlogged
      p_backlog <- mean(mc_res$summary$backlog > 0)
      cat(sprintf("Backlog Risk = %.1f%%\n", p_backlog * 100))
      
      if (p_backlog <= target_p_backlog) {
        # We achieved the capacity target! Can we do it with fewer techs?
        best_techs <- mid_techs
        best_p_backlog <- p_backlog
        high <- mid_techs - 1 # Search lower half
      } else {
        # We are leaving work unfinished. We need more techs.
        low <- mid_techs + 1 # Search upper half
      }
    }
    
    # Store the results
    if (is.na(best_techs)) {
      cat(sprintf("  [!] WARNING: Target unattainable even with %d techs.\n", max_techs))
    } else {
      cat(sprintf("  -> Optimal Capacity: %d techs\n", best_techs))
    }
    
    results_list[[length(results_list) + 1]] <- data.frame(
      t_start = t_start,
      required_techs = ifelse(is.na(best_techs), max_techs, best_techs),
      achieved_p_backlog = ifelse(is.na(best_p_backlog), 1.0, best_p_backlog)
    )
  }
  
  final_df <- do.call(rbind, results_list)
  cat("\n=== Solver Complete ===\n")
  return(final_df)
}