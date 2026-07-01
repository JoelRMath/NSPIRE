# Helper function to instantiate a valid sim_env for our test blocks
get_test_env <- function() {
  dag_csv <- system.file("extdata", "tasks_config.csv", package = "nspiresim")
  fp_csv <- system.file("extdata", "floorplans_config.csv", package = "nspiresim")
  yaml_path <- system.file("extdata", "scenario1.yml", package = "nspiresim")
  
  if (dag_csv == "") {
    dag_csv <- testthat::test_path("../../inst/extdata/tasks_config.csv")
    fp_csv <- testthat::test_path("../../inst/extdata/floorplans_config.csv")
    yaml_path <- testthat::test_path("../../inst/extdata/scenario1.yml")
  }
  
  mix_weights <- c("Studio" = 10, "1BR_1BA" = 40, "2BR_1BA" = 20, "2BR_2BA" = 20, "3BR_2BA" = 10)
  create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)
}

test_that("rpert calculates bounded random times and handles fixed times", {
  # Generate 1000 tasks that take between 5 and 20 minutes
  times <- rpert(1000, 5, 10, 20)
  
  expect_length(times, 1000)
  # Absolute bounds check
  expect_true(all(times >= 5))
  expect_true(all(times <= 20))
  
  # Check fixed time (e.g., a = m = b = 15)
  fixed_times <- rpert(10, 15, 15, 15)
  expect_equal(unique(fixed_times), 15)
  expect_length(fixed_times, 10)
})

test_that("generate_building scales correctly and uses requested archetypes", {
  sim_env <- get_test_env()
  
  # Spawn a 50-unit building
  bldg <- generate_building(sim_env, 50)
  
  expect_length(bldg, 50)
  expect_type(bldg, "character")
  # Ensure all spawned units are valid archetypes from our mix
  expect_true(all(bldg %in% names(sim_env@building_mix)))
})

test_that("run_simulation produces correct dataframe structure and respects seeds", {
  sim_env <- get_test_env()
  
  # Run two identical simulations with the same seed
  res1 <- run_simulation(sim_env, total_units = 25, seed = 42)
  res2 <- run_simulation(sim_env, total_units = 25, seed = 42)
  
  # Check reproducibility (they should be perfectly identical)
  expect_equal(res1, res2)
  
  # Check data frame structure
  expect_s3_class(res1, "data.frame")
  expected_cols <- c("unit_id", "unit_type", "task_id", "category", "severity", 
                     "is_caught", "repair_time_mins", "material_cost")
  expect_true(all(expected_cols %in% names(res1)))
})

test_that("schedule_repairs applies topological DAG sort and calculates labor", {
  sim_env <- get_test_env()
  raw_defects <- run_simulation(sim_env, total_units = 20, seed = 100)
  
  scheduled <- schedule_repairs(raw_defects, sim_env)
  
  # Check that new columns were added
  expect_true("labor_cost" %in% names(scheduled))
  expect_true("cumulative_time_mins" %in% names(scheduled))
  expect_true("is_overtime" %in% names(scheduled))
  expect_true("is_backlogged" %in% names(scheduled))
  
  # Check that time accurately moves forward within a specific unit
  if (nrow(scheduled) > 0) {
    # Isolate Unit 1
    unit_1_data <- scheduled[scheduled$unit_id == scheduled$unit_id[1], ]
    
    # If there's more than one task in the unit, the cumulative time MUST strictly increase
    if (nrow(unit_1_data) > 1) {
      expect_true(all(diff(unit_1_data$cumulative_time_mins) > 0))
    }
  }
})