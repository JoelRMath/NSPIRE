# ==========================================
# NSPIRE Monte Carlo Wrapper
# ==========================================

#' @export
run_monte_carlo <- function(n_iterations = 1000, sim_env, score_csv, rubric = "NSPIRE", 
                            total_units = 50, audit_catch_rate = 0.8, 
                            t_start = 30, t_end = 5, seed = NULL, save_raw = FALSE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  all_iterations <- lapply(1:n_iterations, function(i) {
    raw_defects <- run_simulation(sim_env, total_units = total_units)
    
    # Capture the output as a list (costs and defects)
    scheduled_output <- schedule_repairs(raw_defects, sim_env, t_prep_days = max(1, t_start - t_end))
    # cat("DEBUG: Number of units in scheduled_output$costs: ", nrow(scheduled_output$costs), "\n")
    # cat("DEBUG: Mean labor_cost per unit: ", mean(scheduled_output$costs$labor_cost), "\n")
    # cat("DEBUG: Total sum of labor_cost: ", sum(scheduled_output$costs$labor_cost), "\n")
    
    audited <- apply_audit_fixes(raw_defects, audit_catch_rate, t_end = t_end)
    score_report <- calculate_inspection_score(audited, score_csv, rubric = rubric, 
                                               total_units_in_building = total_units)
    
    # CRITICAL: Sum costs using the unit-level dataframe, NOT the defect-level one.
    return(list(
      summary = c(score = score_report$final_score,
                  labor = sum(raw_defects$repair_time_mins) / 60,
                  total_labor_cost = sum(scheduled_output$costs$labor_cost),
                  overtime_cost = sum(scheduled_output$costs$overtime_cost),
                  backlog = sum(scheduled_output$costs$is_backlogged)),
      raw_defects = if (save_raw) audited else NULL
    ))
  })
  
  summary_mat <- do.call(rbind, lapply(all_iterations, `[[`, "summary"))
  df_results <- as.data.frame(summary_mat)
  raw_list <- if (save_raw) lapply(all_iterations, `[[`, "raw_defects") else NULL
  
  return(list(summary = df_results, raw = raw_list))
}