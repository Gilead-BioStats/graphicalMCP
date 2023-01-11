#' Delete multiple hypotheses from a graph
#'
#' @param graph An MCP graph as created by `graph()`
#' @param keep_hypotheses A Boolean vector denoting which hypotheses to delete.
#'   An entry of `FALSE` corresponds to a deletion, and `TRUE` corresponds to
#'   keeping a hypothesis
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
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0))
#' g <- create_graph(hypotheses, transitions)
#'
#' # Delete the third hypothesis
#' update_graph(g, c(TRUE, TRUE, FALSE, TRUE))
#'
#' #
#'
update_graph <- function(graph, keep) {
  stopifnot(
    "keep_hypotheses must be logical" = is.logical(keep),
    "keep_hypotheses length must match number of hypotheses in graph" =
      length(graph$hypotheses) == length(keep)
  )

  names(keep) <- names(graph$hypotheses)

  initial_graph <- graph

  cume_delete <- 0

  for (hyp_num in seq_along(keep)) {
    if (!keep[[hyp_num]]) {
      graph <- delete_node_fast(graph, hyp_num - cume_delete)
      cume_delete <- cume_delete + 1
    }
  }

  structure(
    list(
      initial_graph = initial_graph,
      kept_hypotheses = keep,
      updated_graph = graph
    ),
    class = "updated_graph"
  )
}


update_graph_single <- function(graph, keep) {
  stopifnot(
    "keep_hypotheses must be logical" = is.logical(keep),
    "keep_hypotheses length must match number of hypotheses in graph" =
      length(graph$hypotheses) == length(keep)
  )

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  names(keep) <- names(hypotheses)

  initial_graph <- graph

  for (del_hyp in hypotheses[!keep]) {
    if (!keep[[hyp_num]]) {
      delete_num <- hyp_num - cume_delete

      graph <- delete_node_fast(graph, hyp_num - cume_delete)
      cume_delete <- cume_delete + 1
    }
  }

  structure(
    list(
      initial_graph = initial_graph,
      kept_hypotheses = keep,
      updated_graph = graph
    ),
    class = "updated_graph"
  )
}
