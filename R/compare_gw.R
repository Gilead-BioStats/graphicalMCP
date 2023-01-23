# Sort the results of generate_weights() the way gMCP::generateWeights sorts
# them. Then set the row & columns names equal and compare
as_gmcp_graph <- function(graph) {
  gMCP::matrix2graph(graph$transitions, graph$hypotheses)
}

as_graph <- function(gmcp_graph) {
  create_graph(gmcp_graph@weights, gmcp_graph@m)
}

compare_gw <- function(graph) {
  gw <- generate_weights(graph)
  gw_gmcp <- generateWeights(graph$transitions, graph$hypotheses)

  all.equal(unname(gw), unname(gw_gmcp))
}

compare_rpt <- function(graph) {

}
