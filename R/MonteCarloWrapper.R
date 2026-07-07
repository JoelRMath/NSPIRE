#' @export
run_monte_carlo <- function(n_iterations = 1000, sim_env, score_csv, rubric = "NSPIRE",
                            total_units = 50, audit_catch_rate = 0.8,
                            t_start = 30, t_end = 5, seed = NULL, save_raw = FALSE) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Collect all results in a list
  
  all_iterations <- lapply(1:n_iterations, function(i) {
    raw_defects <- run_simulation(sim_env, total_units = total_units)
    
    # 1. Schedule repairs (Assigns backlogs and overtime)
    scheduled <- schedule_repairs(raw_defects, sim_env, t_prep_days = max(1, t_start - t_end))
    
    # 2. Audit & Fix (BUG FIX: Use 'scheduled', not 'raw_defects')
    audited <- apply_audit_fixes(scheduled, audit_catch_rate, t_end = t_end)
    
    # 3. Final Scoring
    score_report <- calculate_inspection_score(audited, score_csv, rubric = rubric, 
                                               total_units_in_building = total_units)
    
    # Return a structure containing both summary stats and the raw forensic data
    return(list(
      summary = c(score = score_report$final_score,
                  labor = sum(scheduled$repair_time_mins) / 60,
                  total_labor_cost = sum(scheduled$labor_cost),
                  overtime_cost = sum(scheduled$overtime_cost),
                  backlog = sum(scheduled$is_backlogged)),
      raw_defects = if (save_raw) audited else NULL
    ))
    
    
  })
  
  # Aggregate summary matrix
  
  summary_mat <- do.call(rbind, lapply(all_iterations, `[[`, "summary"))
  df_results <- as.data.frame(summary_mat)
  
  # If raw data requested, combine it into a list
  
  raw_list <- if (save_raw) lapply(all_iterations, `[[`, "raw_defects") else NULL
  
  return(list(summary = df_results, raw = raw_list))
}