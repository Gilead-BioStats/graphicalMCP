#' S3 plot method for the class `updated_graph`
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
  v_colors <- rep("#e8c2ff", length(x$updated_graph$hypotheses))
  v_colors[x$deleted] <- "grey80"

  plot(x$updated_graph, vertex.color = v_colors, ...)
}
