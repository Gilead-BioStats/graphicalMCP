#' S3 plot method for the class `udpated_graph`
#'
#' @inheritDotParams plot.initial_graph
#'
#' @return NULL, after plotting the graph
#' @export
#'
#' @examples
#' plot(graph_update(simple_successive_2(), c(T, F, T, T)), layout = "grid")
plot.updated_graph <- function(x, ...) {
  v_colors <- rep("#e8c2ff", length(x$updated_graph$hypotheses))
  v_colors[!x$kept_hypotheses] <- "grey80"

  plot(x$updated_graph, vertex.color = v_colors, ...)
}
