#' Delete multiple hypotheses from a graph
#'
#' `update_graph_plain()` seeks to stay as simple as possible, and as similar as
#' possible to the algorithm laid out in the paper. Hypotheses and
#' transitions are directly updated using the formulas from the paper and
#' vectorized operations.
#'
#' @param graph An MCP graph as created by `initial_graph()`
#' @param keep A Boolean vector denoting which hypotheses to delete. An entry of
#'   `FALSE` corresponds to a deletion, and `TRUE` corresponds to keeping a
#'   hypothesis
#'
#' @return An object of class `updated_graph` with 3 elements
#'   * The initial graph object
#'   * The Boolean vector of keep/delete indicators
#'   * The updated graph object with appropriate hypotheses removed
#' @export
#'
#' @examples
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(c(0, 0, 1, 0),
#'                      c(0, 0, 0, 1),
#'                      c(0, 1, 0, 0),
#'                      c(1, 0, 0, 0))
#' g <- create_graph(hypotheses, transitions)
#'
#' # Delete the third hypothesis
#' update_graph(g, c(TRUE, TRUE, FALSE, TRUE))
#'
#' #
#'
update_graph_plain <- function(graph, keep) {
  stopifnot(
    "keep must be a logical or integer vector" =
      is.logical(keep) || is.integer(keep),
    "keep length must match number of hypotheses in graph" =
      length(graph$hypotheses) == length(keep)
  )

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  Jc <- seq_along(keep)[!keep]

  for (j in Jc) {
    hypotheses[-j] <- hypotheses[-j] + hypotheses[j] * transitions[j, -j]
    hypotheses[j] <- 0

    transitions[-j, -j] <- (
      transitions[-j, -j] + transitions[-j, j] %o% transitions[j, -j]
    ) / (
      1 - transitions[-j, -j] * t(transitions[-j, -j])
    )
  }

  hypotheses[Jc] <- 0
  transitions[Jc,] <- 0
  transitions[, Jc] <- 0
  diag(transitions) <- 0

  structure(
    list(
      initial_graph = initial_graph,
      kept_hypotheses = keep,
      updated_graph = structure(
        list(hypotheses = hypotheses, transitions = transitions),
        class = "initial_graph"
      )
    ),
    class = "updated_graph"
  )
}
