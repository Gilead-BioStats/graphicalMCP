#' Convert between gMCP and graphicalMCP graph classes
#'
#' @param graph An `initial_graph` object from graphicalMCP, a `graphMCP` object
#'   from gMCP, or an `igraph` object from igraph, depending on which direction
#'   you're converting
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
  cat("igraph method")
  invisible(graph)
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
    graph_size <- length(graph$hypotheses)
    graph_names <- names(graph$hypotheses)

    all_vert_cross <- rev(expand.grid(
      end = seq_along(graph$hypotheses),
      start = seq_along(graph$hypotheses)
    ))

    which_edge <- apply(
      all_vert_cross,
      1,
      function(row) graph$transitions[row[[1]], row[[2]]]
    ) != 0

    df_edges <- all_vert_cross[which_edge, ]

    graph_igraph <- igraph::make_directed_graph(t(df_edges))

    graph_igraph_vnm <- igraph::set_vertex_attr(
      graph_igraph,
      "name",
      value = graph_names
    )

    graph_igraph_vwgt <- igraph::set_vertex_attr(
      graph_igraph_vnm,
      "weight",
      value = graph$hypotheses
    )

    graph_igraph_ewgt <- igraph::set_edge_attr(
      graph_igraph_vwgt,
      "weight",
      value = t(graph$transitions)[t(graph$transitions) > 0]
    )

    graph_igraph_ewgt
  }
}
