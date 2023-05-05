as_gmcp_graph <- function(graph) {
  if (requireNamespace("gMCP", quietly = TRUE)) {
    gMCP::matrix2graph(graph$transitions, graph$hypotheses)
  } else {
    stop("Please install.packages('gMCP') before converting to a gMCP graph")
  }
}

as_graph <- function(gmcp_graph) {
  create_graph(gmcp_graph@weights, gmcp_graph@m)
}
