library(igraph)

# ==========================================
# 1. The DAG Class Definition
# ==========================================
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
create_inspection_dag <- function(csv_path) {
  # Read the CSV matrix
  df <- read.csv(csv_path, stringsAsFactors = FALSE, strip.white = TRUE)
  
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
  
  g <- make_empty_graph(directed = TRUE) + vertices(df$task_id)
  if (length(edges) > 0) {
    g <- g + edges(edges)
  }
  
  # Validate Graph Mathematics
  if (!is_dag(g)) {
    stop("CRITICAL ERROR: Circular dependencies detected in the CSV. It is not a valid DAG.")
  }
  
  # Return S4 Object
  new("InspectionDAG", 
      tasks_config = df, 
      adj_list = adj, 
      graph = g)
}

# ==========================================
# 3. The Custom Show Method
# ==========================================
setMethod("show", "InspectionDAG", function(object) {
  cat("========================================\n")
  cat("          Inspection DAG Object         \n")
  cat("========================================\n")
  cat("Total Tasks (Nodes)   :", nrow(object@tasks_config), "\n")
  cat("Dependencies (Edges)  :", ecount(object@graph), "\n")
  cat("Graph is Acyclic?     :", is_dag(object@graph), "\n\n")
  
  # Find "Root" tasks (no prerequisites, can be started immediately)
  in_degrees <- degree(object@graph, mode = "in")
  roots <- names(in_degrees[in_degrees == 0])
  cat("Independent Root Tasks:", length(roots), "\n")
  
  # Find "Terminal" tasks (no downstream dependencies)
  out_degrees <- degree(object@graph, mode = "out")
  terminals <- names(out_degrees[out_degrees == 0])
  cat("Terminal Tasks        :", length(terminals), "\n")
  cat("========================================\n")
})