test_that("create_inspection_dag constructs a valid S4 object", {
  # 1. Locate the configuration file
  csv_path <- system.file("extdata", "tasks_config.csv", package = "nspiresim")
  if (csv_path == "") {
    csv_path <- testthat::test_path("../../inst/extdata/tasks_config.csv")
  }
  
  # 2. Instantiate the DAG object
  dag_obj <- create_inspection_dag(csv_path)
  
  # 3. Test Object Class
  expect_s4_class(dag_obj, "InspectionDAG")
  
  # 4. Test Data Ingestion (Expecting exactly 34 tasks from our CSV)
  expect_equal(nrow(dag_obj@tasks_config), 34)
  expect_true("p_decay" %in% colnames(dag_obj@tasks_config))
  
  # 5. Test Graph Mathematics
  expect_true(igraph::is_dag(dag_obj@graph))
  expect_equal(igraph::vcount(dag_obj@graph), 34)
})

test_that("show method for InspectionDAG prints expected dashboard", {
  csv_path <- system.file("extdata", "tasks_config.csv", package = "nspiresim")
  if (csv_path == "") {
    csv_path <- testthat::test_path("../../inst/extdata/tasks_config.csv")
  }
  
  dag_obj <- create_inspection_dag(csv_path)
  
  # expect_output captures the cat() / print() output to the console 
  # We use regular expressions to match the expected values
  expect_output(show(dag_obj), "Inspection DAG Object")
  expect_output(show(dag_obj), "Total Tasks \\(Nodes\\)\\s+:\\s+34")
  expect_output(show(dag_obj), "Graph is Acyclic\\?\\s+:\\s+TRUE")
})