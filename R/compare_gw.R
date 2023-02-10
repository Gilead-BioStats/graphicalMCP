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

# Sort the results of generate_weights() the way gMCP::generateWeights
# sorts them. Then set the row & columns names equal and compare
compare_gw <- function(graph) {
  gw <- generate_weights(graph)
  gw_gmcp <- gMCP::generateWeights(graph$transitions, graph$hypotheses)

  all.equal(unname(gw[nrow(gw):1, ]), unname(gw_gmcp))
}
