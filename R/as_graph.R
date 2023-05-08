#' Convert between gMCP and graphicalMCP graph classes
#'
#' @param graph A graphicalMCP initial_graph
#' @param gmcp_graph A gMCP graph
#'
#' @return For `as_gmcp_graph()`, a gMCP graph object, and for `as_graph()`, a
#'   graphicalMCP graph object
#' @rdname as-graph
#' @export
#'
#' @examples
#' g1 <- random_graph(5)
#'
#' if (requireNamespace("gMCP", quietly = TRUE)) g2 <- as_gmcp_graph(g1)
#'
#' all.equal(g1, as_graph(g2))
as_gmcp_graph <- function(graph) {
  if (requireNamespace("gMCP", quietly = TRUE)) {
    gMCP::matrix2graph(graph$transitions, graph$hypotheses)
  } else {
    stop("Please install.packages('gMCP') before converting to a gMCP graph")
  }
}

#' @rdname as-graph
#' @export
as_graph <- function(gmcp_graph) {
  create_graph(gmcp_graph@weights, gmcp_graph@m)
}
