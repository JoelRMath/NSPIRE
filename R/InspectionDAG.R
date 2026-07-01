# ==========================================
# 1. DAG Class Definition
# ==========================================

#' InspectionDAG Class
#'
#' An S4 class that represents the mathematical directed acyclic graph (DAG) 
#' of inspection tasks and their dependencies.
#'
#' @slot tasks_config A \code{data.frame} containing task times, dependencies, and decay probabilities.
#' @slot adj_list A \code{list} providing a fast-lookup prerequisite adjacency list.
#' @slot graph An \code{igraph} object representing the network of task dependencies.
#' @export
setClass(
  "InspectionDAG",
  slots = list(
    tasks_config = "data.frame",  # Contains task times, dependencies, and p_decay
    adj_list     = "list",        # Fast-lookup prerequisite list
    graph        = "ANY"          # igraph object
  )
)

# ==========================================
# 2. The Constructor Function
# ==========================================

#' Create an Inspection DAG
#'
#' Reads a CSV configuration matrix, validates the workflow dependencies, 
#' and constructs an \code{InspectionDAG} object.
#'
#' @param csv_path A character string representing the file path to the tasks configuration CSV.
#' @return An object of class \code{InspectionDAG}.
#' @importFrom methods new
#' @export
create_inspection_dag <- function(csv_path) {
  # Read the CSV matrix
  df <- read.csv(csv_path, stringsAsFactors = FALSE, strip.white = TRUE, check.names = FALSE)
  
  # Ensure NAs are handled uniformly
  df$prerequisites[is.na(df$prerequisites) | df$prerequisites == "NA"] <- ""
  
  # Build Adjacency List
  adj <- lapply(df$prerequisites, function(p) {
    if (p == "") return(character(0))
    trimws(unlist(strsplit(p, "\\|")))
  })
  names(adj) <- df$task_id
  
  # Build igraph Object
  edges <- character(0)
  for (i in seq_along(adj)) {
    target_node <- names(adj)[i]
    for (prereq_node in adj[[i]]) {
      edges <- c(edges, prereq_node, target_node)
    }
  }
  
  g <- igraph::make_empty_graph(directed = TRUE) + igraph::vertices(df$task_id)
  if (length(edges) > 0) {
    g <- g + igraph::edges(edges)
  }
  
  # Validate Graph Mathematics
  if (!igraph::is_dag(g)) {
    stop("CRITICAL ERROR: Circular dependencies detected in the CSV. It is not a valid DAG.")
  }
  
  # Return S4 Object
  methods::new("InspectionDAG", 
      tasks_config = df, 
      adj_list = adj, 
      graph = g)
}

# ==========================================
# 3. The Custom Show Method
# ==========================================

#' Show method for InspectionDAG
#'
#' Prints a clean console dashboard summary of the DAG's mathematical properties, 
#' including root and terminal task counts.
#'
#' @param object An \code{InspectionDAG} object.
#' @importFrom methods show
#' @exportMethod show
setMethod("show", "InspectionDAG", function(object) {
  cat("========================================\n")
  cat("          Inspection DAG Object         \n")
  cat("========================================\n")
  cat("Total Tasks (Nodes)   :", nrow(object@tasks_config), "\n")
  cat("Dependencies (Edges)  :", igraph::ecount(object@graph), "\n")
  cat("Graph is Acyclic?     :", igraph::is_dag(object@graph), "\n\n")
  
  # Find "Root" tasks (no prerequisites, can be started immediately)
  in_degrees <- igraph::degree(object@graph, mode = "in")
  roots <- names(in_degrees[in_degrees == 0])
  cat("Independent Root Tasks:", length(roots), "\n")
  
  # Find "Terminal" tasks (no downstream dependencies)
  out_degrees <- igraph::degree(object@graph, mode = "out")
  terminals <- names(out_degrees[out_degrees == 0])
  cat("Terminal Tasks        :", length(terminals), "\n")
  cat("========================================\n")
})