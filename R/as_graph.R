#' Convert between graphicalMCP, gMCP, and igraph graph classes
#'
#' Graphs are handled with a different object in graphicalMCP, gMCP, and igraph.
#' These functions convert minimally between the different classes.
#'
#' Note that igraph and gMCP can set various attributes for vertices, edges, or
#' a graph itself. These conversion functions only handle attributes related to
#' names and weights. Other attributes will be dropped when converting.
#'
#' @param graph An `initial_graph` object from graphicalMCP, a `graphMCP` object
#'   from gMCP, or an `igraph` object from igraph, depending on conversion type
#'
#' @return For `as_graphMCP()`, a gMCP graph object, for `as_igraph()`, an
#'   igraph object, and for `as_initial_graph()`, a graphicalMCP graph object
#' @rdname as-graph
#' @export
#'
#' @examples
#' g1 <- random_graph(5)
#'
#' if (requireNamespace("gMCP", quietly = TRUE)) {
#'   g2 <- as_graphMCP(g1)
#'
#'   all.equal(g1, as_initial_graph(g2))
#' }
#'
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g3 <- as_igraph(g1)
#'
#'   all.equal(g1, as_initial_graph(g3))
#' }
as_initial_graph <- function(graph) {
  UseMethod("as_initial_graph", graph)
}

#' @rdname as-graph
#' @export
as_initial_graph.graphMCP <- function(graph) {
  graph_create(graph@weights, graph@m)
}

#' @rdname as-graph
#' @export
as_initial_graph.igraph <- function(graph) {
  hypotheses <- igraph::vertex_attr(graph, "weight")
  names(hypotheses) <- igraph::vertex_attr(graph, "name")

  transitions <- matrix(0, length(graph), length(graph))
  dimnames(transitions) <- rep(list(igraph::vertex_attr(graph, "name")), 2)

  for (tail in seq_along(graph)) {
    transitions[tail, ] <- graph[tail]
  }

  graph_create(hypotheses, transitions)
}

#' @rdname as-graph
#' @export
as_graphMCP <- function(graph) {
  UseMethod("as_graphMCP", graph)
}

#' @rdname as-graph
#' @export
as_graphMCP.initial_graph <- function(graph) {
  if (!requireNamespace("gMCP", quietly = TRUE)) {
    stop("Please install.packages('gMCP') before converting to a gMCP graph")
  } else {
    gMCP::matrix2graph(graph$transitions, graph$hypotheses)
  }
}

#' @rdname as-graph
#' @export
as_igraph <- function(graph) {
  UseMethod("as_igraph", graph)
}

#' @rdname as-graph
#' @export
as_igraph.initial_graph <- function(graph) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Please install.packages('igraph') before converting to an igraph")
  } else {
    num_hyps <- length(graph$hypotheses)
    hyp_names <- names(graph$hypotheses)

    empty_igraph <- igraph::make_empty_graph()

    vertex_igraph <- igraph::add_vertices(
      empty_igraph,
      num_hyps,
      name = hyp_names,
      weight = graph$hypotheses
    )

    matrix_edge_tails <- matrix(rep(hyp_names, num_hyps), nrow = num_hyps)
    matrix_edge_heads <-
      matrix(rep(hyp_names, num_hyps), nrow = num_hyps, byrow = TRUE)

    edge_tails <- matrix_edge_tails[graph$transitions != 0]
    edge_heads <- matrix_edge_heads[graph$transitions != 0]

    vector_edges <- as.vector(rbind(edge_tails, edge_heads))

    complete_igraph <- igraph::add_edges(
      vertex_igraph,
      vector_edges,
      weight = graph$transitions[graph$transitions != 0]
    )

    complete_igraph
  }
}
