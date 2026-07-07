#' Get NSPIRE Sample Size
#' @export
get_nspire_sample_size <- function(total_units) {
  if (total_units <= 5) return(total_units)
  if (total_units <= 7) return(6)
  if (total_units == 8) return(7)
  if (total_units <= 10) return(8)
  if (total_units <= 12) return(9)
  if (total_units <= 14) return(10)
  if (total_units <= 16) return(11)
  if (total_units <= 18) return(12)
  if (total_units <= 21) return(13)
  if (total_units <= 24) return(14)
  if (total_units <= 27) return(15)
  if (total_units <= 30) return(16)
  if (total_units <= 35) return(17)
  if (total_units <= 39) return(18)
  if (total_units <= 45) return(19)
  if (total_units <= 51) return(20)
  if (total_units <= 59) return(21)
  if (total_units <= 67) return(22)
  if (total_units <= 78) return(23)
  if (total_units <= 92) return(24)
  if (total_units <= 110) return(25)
  return(32)
}

#' Apply Internal Pre-Inspection Audit
#' @export
apply_audit_fixes <- function(defects_df, audit_catch_rate, t_end = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (nrow(defects_df) == 0) {
    defects_df$caught_in_audit <- logical(0)
    defects_df$rebroke_before_inspection <- logical(0)
    return(defects_df)
  }
  
  # Ensure we respect the workforce backlog limits
  
  is_backlogged <- defects_df$is_backlogged
  if (is.null(is_backlogged)) is_backlogged <- rep(FALSE, nrow(defects_df))
  
  # BUG FIX: A defect can only be "caught and fixed" if it is NOT backlogged.
  
  defects_df$caught_in_audit <- (stats::runif(nrow(defects_df)) <= audit_catch_rate) & !is_backlogged
  
  daily_fail_rate <- pmin(1.0, (defects_df$p_defect / 365) * defects_df$tenant_factor)
  p_rebreak <- 1 - (1 - daily_fail_rate)^t_end
  
  is_fixed <- defects_df$caught_in_audit
  rebroke <- rep(FALSE, nrow(defects_df))
  if (any(is_fixed)) {
    rebroke[is_fixed] <- stats::runif(sum(is_fixed)) < p_rebreak[is_fixed]
  }
  
  defects_df$rebroke_before_inspection <- rebroke
  return(defects_df)
}

#' Calculate Official Inspection Score
#' @export
calculate_inspection_score <- function(defects_df, scoring_csv, rubric = "NSPIRE", starting_score = 100, total_units_in_building = 50) {
  scoring_df <- read.csv(scoring_csv, stringsAsFactors = FALSE, strip.white = TRUE)
  rubric_df <- scoring_df[scoring_df$rubric_name == rubric, ]
  
  if (nrow(defects_df) == 0) return(list(final_score = starting_score, total_deductions = 0))
  
  sample_size <- get_nspire_sample_size(total_units_in_building)
  all_possible_units <- paste0("Unit_", 1:total_units_in_building)
  inspected_units <- sample(all_possible_units, size = sample_size, replace = FALSE)
  
  defects_df$caught_in_audit <- defects_df$caught_in_audit %||% FALSE
  defects_df$rebroke_before_inspection <- defects_df$rebroke_before_inspection %||% FALSE
  
  is_active_defect <- !defects_df$caught_in_audit | defects_df$rebroke_before_inspection
  in_sample <- defects_df$unit_id %in% inspected_units
  
  score_hits <- defects_df[is_active_defect & defects_df$is_caught & in_sample, ]
  
  if (nrow(score_hits) == 0) return(list(final_score = starting_score, total_deductions = 0))
  
  itemized <- merge(score_hits, rubric_df[, c("severity", "deduction")], by = "severity", all.x = TRUE)
  itemized$deduction[is.na(itemized$deduction)] <- 0
  
  total_deductions <- sum(itemized$deduction)
  final_score <- max(0, starting_score - (total_deductions / sample_size))
  
  return(list(final_score = final_score, total_deductions = total_deductions))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b