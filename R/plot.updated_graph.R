#' S3 plot method for the class `updated_graph`
#'
#' @description
#' Plotting an updated graph is a *very* light wrapper around
#' [plot.initial_graph()], only changing the default vertex color to use gray
#' for deleted hypotheses.
#'
#' @param x An object of class `updated_graph` to plot.
#' @inheritDotParams plot.initial_graph
#'
#' @return NULL, after plotting the updated graph.
#'
#' @family updated graphs
#'
#' @seealso
#'   [plot.initial_graph()] for the plot method for the initial graph.
#'
#' @rdname plot.updated_graph
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @references
#'  * \insertRef{bretz-2011-graphical}{graphicalMCP}
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 1 in \insertCite{bretz-2011-graphical;textual}{graphicalMCP}.
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#'
#' # Delete the second and third hypotheses in the "unordered mode"
#' plot(
#'   graph_update(
#'     g,
#'     c(FALSE, TRUE, TRUE, FALSE)
#'   ),
#'   layout = "grid"
#' )
plot.updated_graph <- function(x, ...) {
  v_colors <- rep("#6baed6", length(x$updated_graph$hypotheses))
  v_colors[x$deleted] <- "#cccccc"

  plot(x$updated_graph, vertex.color = v_colors, ...)
}
