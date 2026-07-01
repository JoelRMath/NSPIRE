test_that("apply_audit_fixes adds tracking column and applies probability", {
  # Create dummy defects
  defects <- data.frame(
    unit_id = paste0("Unit_", 1:1000),
    task_id = "FS-01",
    severity = "LT",
    is_caught = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Run a 80% audit catch rate
  audited <- apply_audit_fixes(defects, audit_catch_rate = 0.8, seed = 123)
  
  expect_true("caught_in_audit" %in% names(audited))
  
  # Check that roughly 80% were caught (allow a small margin of error for randomness)
  catch_pct <- sum(audited$caught_in_audit) / nrow(audited)
  expect_true(catch_pct > 0.75 && catch_pct < 0.85)
})

test_that("calculate_inspection_score normalizes deductions by unit count", {
  score_csv <- system.file("extdata", "scoring_config.csv", package = "nspiresim")
  if (score_csv == "") score_csv <- testthat::test_path("../../inst/extdata/scoring_config.csv")
  
  # 3 defects: 1 LT (5 pts), 2 STD (1 pt each). Total deductions = 7.
  defects <- data.frame(
    unit_id = c("U1", "U2", "U3"),
    severity = c("LT", "STD", "STD"),
    is_caught = c(TRUE, TRUE, TRUE),
    caught_in_audit = c(FALSE, FALSE, FALSE), # Audit missed them all
    stringsAsFactors = FALSE
  )
  
  # Test 1: No normalization (Raw subtraction: 100 - 7 = 93)
  raw_score <- calculate_inspection_score(defects, score_csv, rubric = "NSPIRE")
  expect_equal(raw_score$final_score, 93)
  expect_equal(raw_score$total_deductions, 7)
  
  # Test 2: Normalized by 10 units (7 / 10 = 0.7 points lost. 100 - 0.7 = 99.3)
  norm_score <- calculate_inspection_score(defects, score_csv, rubric = "NSPIRE", total_units_inspected = 10)
  expect_equal(norm_score$final_score, 99.3)
  expect_equal(norm_score$points_lost, 0.7)
})

test_that("calculate_inspection_score ignores audit-caught and inspector-missed items", {
  score_csv <- system.file("extdata", "scoring_config.csv", package = "nspiresim")
  if (score_csv == "") score_csv <- testthat::test_path("../../inst/extdata/scoring_config.csv")
  
  defects <- data.frame(
    unit_id = c("U1", "U2", "U3"),
    severity = c("LT", "LT", "LT"), # 5 pts each
    is_caught = c(TRUE, FALSE, TRUE),        # Inspector misses U2
    caught_in_audit = c(TRUE, FALSE, FALSE), # Audit catches U1
    stringsAsFactors = FALSE
  )
  
  # Only U3 should hit the score! (U1 caught by team, U2 missed by inspector)
  # Total deductions should be 5. Score should be 95.
  score <- calculate_inspection_score(defects, score_csv, rubric = "NSPIRE")
  
  expect_equal(score$total_deductions, 5)
  expect_equal(score$final_score, 95)
  expect_equal(nrow(score$itemized), 1)
})

test_that("calculate_inspection_score handles unknown rubric gracefully", {
  score_csv <- system.file("extdata", "scoring_config.csv", package = "nspiresim")
  if (score_csv == "") score_csv <- testthat::test_path("../../inst/extdata/scoring_config.csv")
  
  defects <- data.frame(severity="LT", is_caught=TRUE, caught_in_audit=FALSE)
  
  expect_error(calculate_inspection_score(defects, score_csv, rubric = "FAKE_RUBRIC"), "not found in scoring config")
})