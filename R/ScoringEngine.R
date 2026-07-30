
# ==========================================
# 1. Regulatory Sampling
# ==========================================

#' Get NSPIRE Sample Size
#'
#' Retrieves the HUD-mandated number of units to inspect based on the total 
#' property size. This discrete step function drives the variance of the 
#' "Lottery Effect" in small to mid-sized properties.
#'
#' @param total_units Integer. The total number of apartments in the property.
#' @return Integer. The specific number of units the inspector will randomly sample.
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

# ==========================================
# 2. Quality Control & Asset Decay
# ==========================================

#' Apply Internal Pre-Inspection Audit
#'
#' Simulates the internal quality assurance process and the subsequent 
#' post-prep temporal decay. It determines if a defect is successfully caught 
#' and fixed by the team, and whether that fixed item randomly re-breaks 
#' before the HUD inspector arrives.
#'
#' @param defects_df A \code{data.frame} of scheduled defects.
#' @param audit_catch_rate Numeric [0, 1]. Probability that the internal team detects and fixes a defect.
#' @param t_end Integer. The number of days between the completion of repairs and the actual inspection date.
#' @param seed Integer. Optional random seed for reproducibility.
#' @return A \code{data.frame} with added logical flags: \code{caught_in_audit} and \code{rebroke_before_inspection}.
#' @export
apply_audit_fixes <- function(defects_df, audit_catch_rate, t_end = 0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (nrow(defects_df) == 0) {
    defects_df$caught_in_audit <- logical(0)
    defects_df$rebroke_before_inspection <- logical(0)
    return(defects_df)
  }
  
  # Ensure we respect the workforce backlog limits from the logistics engine
  is_backlogged <- defects_df$is_backlogged
  if (is.null(is_backlogged)) is_backlogged <- rep(FALSE, nrow(defects_df))
  
  # A defect can only be "caught and fixed" if it is NOT stuck in the backlog.
  # If capacity ran out, the item is mathematically abandoned.
  defects_df$caught_in_audit <- (stats::runif(nrow(defects_df)) <= audit_catch_rate) & !is_backlogged
  
  # Calculate Stochastic Decay: The probability that a fixed item re-breaks.
  # This scales the baseline annual probability down to a daily rate, amplified by tenant behavior.
  daily_fail_rate <- pmin(1.0, (defects_df$p_defect / 365) * defects_df$tenant_factor)
  p_rebreak <- 1 - (1 - daily_fail_rate)^t_end
  
  # Evaluate decay only for items that were actually fixed during the audit
  is_fixed <- defects_df$caught_in_audit
  rebroke <- rep(FALSE, nrow(defects_df))
  if (any(is_fixed)) {
    rebroke[is_fixed] <- stats::runif(sum(is_fixed)) < p_rebreak[is_fixed]
  }
  
  defects_df$rebroke_before_inspection <- rebroke
  return(defects_df)
}

# ==========================================
# 3. HUD Scoring Protocol
# ==========================================

#' Calculate Official Inspection Score
#'
#' Executes the regulatory scoring protocol. It randomly samples the building, 
#' identifies the active defects within that sample, merges them against the 
#' regulatory deduction weights, and outputs the final mathematical score.
#'
#' @param defects_df A \code{data.frame} of defects processed by the audit engine.
#' @param scoring_csv Character. Path to the configuration file containing defect weights.
#' @param rubric Character. The regulatory rubric to apply (default: "NSPIRE").
#' @param starting_score Numeric. The maximum possible score (default: 100).
#' @param total_units_in_building Integer. Used to calculate the denominator for sampling deductions.
#' @return A list containing \code{final_score} and \code{total_deductions}.
#' @export
calculate_inspection_score <- function(defects_df, scoring_csv, rubric = "NSPIRE", starting_score = 100, total_units_in_building = 50) {
  # Load and filter the deduction weights dictionary
  scoring_df <- read.csv(scoring_csv, stringsAsFactors = FALSE, strip.white = TRUE)
  rubric_df <- scoring_df[scoring_df$rubric_name == rubric, ]
  
  # Fast exit for a perfect building
  if (nrow(defects_df) == 0) return(list(final_score = starting_score, total_deductions = 0))
  
  # 1. Execute Random Spatial Sampling (The "Lottery")
  sample_size <- get_nspire_sample_size(total_units_in_building)
  all_possible_units <- paste0("Unit_", 1:total_units_in_building)
  inspected_units <- sample(all_possible_units, size = sample_size, replace = FALSE)
  
  # Safely handle missing flags just in case the audit step was bypassed
  defects_df$caught_in_audit <- defects_df$caught_in_audit %||% FALSE
  defects_df$rebroke_before_inspection <- defects_df$rebroke_before_inspection %||% FALSE
  
  # 2. Determine True Active State
  # An item is broken if it was NEVER caught, OR if it was caught but re-broke over time.
  is_active_defect <- !defects_df$caught_in_audit | defects_df$rebroke_before_inspection
  
  # Check if the defect falls inside the units randomly chosen by the inspector
  in_sample <- defects_df$unit_id %in% inspected_units
  
  # Filter down to the exact items the inspector actually writes up
  score_hits <- defects_df[is_active_defect & defects_df$is_caught & in_sample, ]
  
  if (nrow(score_hits) == 0) return(list(final_score = starting_score, total_deductions = 0))
  
  # 3. Calculate Deductions
  # Join the active defects with the HUD penalty weights based on severity
  itemized <- merge(score_hits, rubric_df[, c("severity", "deduction")], by = "severity", all.x = TRUE)
  itemized$deduction[is.na(itemized$deduction)] <- 0
  
  # Sum the raw penalties and divide by the sample size (per NSPIRE mathematical rules)
  total_deductions <- sum(itemized$deduction)
  final_score <- max(0, starting_score - (total_deductions / sample_size))
  
  return(list(final_score = final_score, total_deductions = total_deductions))
}

# Internal Helper: Null-coalescing operator for clean fallback assignments
`%||%` <- function(a, b) if (!is.null(a)) a else b