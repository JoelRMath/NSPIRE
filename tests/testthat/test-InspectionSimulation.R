test_that("create_simulation_env constructs a valid S4 object", {
  # 1. Locate the configuration files
  dag_csv <- system.file("extdata", "tasks_config.csv", package = "nspiresim")
  fp_csv <- system.file("extdata", "floorplans_config.csv", package = "nspiresim")
  yaml_path <- system.file("extdata", "scenario1.yml", package = "nspiresim")
  
  # Fallback for interactive testing
  if (dag_csv == "") {
    dag_csv <- testthat::test_path("../../inst/extdata/tasks_config.csv")
    fp_csv <- testthat::test_path("../../inst/extdata/floorplans_config.csv")
    yaml_path <- testthat::test_path("../../inst/extdata/scenario1.yml")
  }
  
  # Define a valid mix of the 5 archetypes
  mix_weights <- c(
    "Studio" = 10,
    "1BR_1BA" = 40,
    "2BR_1BA" = 20,
    "2BR_2BA" = 20,
    "3BR_2BA" = 10
  )
  
  # 2. Instantiate the environment
  sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)
  
  # 3. Assertions
  expect_s4_class(sim_env, "InspectionSimulation")
  
  # Ensure the weights were mathematically normalized to exactly 1.0
  expect_equal(sum(sim_env@building_mix), 1.0)
  expect_equal(sim_env@building_mix["1BR_1BA"], c("1BR_1BA" = 0.4)) 
  
  # Ensure the floorplans matrix loaded correctly (34 tasks expected)
  expect_equal(nrow(sim_env@floorplans), 34)
  expect_true("3BR_2BA" %in% colnames(sim_env@floorplans))
})

test_that("create_simulation_env catches malformed inputs", {
  dag_csv <- system.file("extdata", "tasks_config.csv", package = "nspiresim")
  fp_csv <- system.file("extdata", "floorplans_config.csv", package = "nspiresim")
  yaml_path <- system.file("extdata", "scenario1.yml", package = "nspiresim")
  
  if (dag_csv == "") {
    dag_csv <- testthat::test_path("../../inst/extdata/tasks_config.csv")
    fp_csv <- testthat::test_path("../../inst/extdata/floorplans_config.csv")
    yaml_path <- testthat::test_path("../../inst/extdata/scenario1.yml")
  }
  
  # Test an unnamed mix vector (should trigger our stop() error)
  bad_mix <- c(0.2, 0.4, 0.4)
  
  expect_error(
    create_simulation_env(dag_csv, fp_csv, yaml_path, bad_mix),
    "must be a named numeric vector"
  )
})

test_that("show method for InspectionSimulation prints expected dashboard", {
  dag_csv <- system.file("extdata", "tasks_config.csv", package = "nspiresim")
  fp_csv <- system.file("extdata", "floorplans_config.csv", package = "nspiresim")
  yaml_path <- system.file("extdata", "scenario1.yml", package = "nspiresim")
  
  if (dag_csv == "") {
    dag_csv <- testthat::test_path("../../inst/extdata/tasks_config.csv")
    fp_csv <- testthat::test_path("../../inst/extdata/floorplans_config.csv")
    yaml_path <- testthat::test_path("../../inst/extdata/scenario1.yml")
  }
  
  mix_weights <- c("Studio" = 10, "1BR_1BA" = 90)
  sim_env <- create_simulation_env(dag_csv, fp_csv, yaml_path, mix_weights)
  
  # Check for the dashboard header and the dynamically calculated percentages
  expect_output(show(sim_env), "NSPIRE Simulation Environment")
  expect_output(show(sim_env), "Studio: 10%, 1BR_1BA: 90%")
})