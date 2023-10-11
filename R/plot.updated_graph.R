#' S3 plot method for the class `updated_graph`
#'
#' Plotting an updated graph is a *very* light wrapper around
#' [plot.initial_graph()], only changing the default vertex color to use gray
#' for deleted hypotheses.
#'
#' @param x An `updated_graph` object as returned by `graph_update()`
#' @inheritDotParams plot.initial_graph
#'
#' @return NULL, after plotting the graph
#' @export
#'
#' @examples
#' plot(
#'   graph_update(
#'     simple_successive_2(),
#'     c(TRUE, FALSE, TRUE, TRUE)
#'   ),
#'   layout = "grid"
#' )
plot.updated_graph <- function(x, ...) {
  v_colors <- rep("#6baed6", length(x$updated_graph$hypotheses))
  v_colors[x$deleted] <- "#cccccc"

  plot(x$updated_graph, vertex.color = v_colors, ...)
}
