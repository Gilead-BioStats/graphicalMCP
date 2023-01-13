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
update_graph <- function(graph, keep) {
  stopifnot(
    "keep must be a logical or integer vector" =
      is.logical(keep) || is.integer(keep),
    "keep length must match number of hypotheses in graph" =
      length(graph$hypotheses) == length(keep)
  )

  initial_graph <- graph
  names(keep) <- names(graph$hypotheses)

  # Iterate over the hypotheses you want to delete
  for (delete_num in which(!keep)) {
    # Save current state of the graph to use in calculations
    # Also make a copy of graph elements for storing new values in
    init_hypotheses <- hypotheses <- graph$hypotheses
    init_transitions <- transitions <- graph$transitions

    # Loop over hypotheses, calculating new weights based on initial hypothesis
    # weights and storing in `hypotheses`
    for (hyp_num in seq_along(init_hypotheses)) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[delete_num]] * init_transitions[[delete_num, hyp_num]]

      # In this loop, hyp_num is the starting node of the transition, and
      # trn_num is the ending node
      # Calculate new transition weights based on original transition weights,
      # and store in `transitions`
      for (trn_num in seq_along(graph$hypotheses)) {
        zero_condition <- any(
          hyp_num == trn_num,
          (init_transitions[[hyp_num, delete_num]] *
             init_transitions[[delete_num, hyp_num]]) >= 1
        )

        if (zero_condition) {
          0
        } else {
          transitions[[hyp_num, trn_num]] <- (
            init_transitions[[hyp_num, trn_num]] +
              init_transitions[[hyp_num, delete_num]] *
              init_transitions[[delete_num, trn_num]]
          ) / (
            1 - init_transitions[[hyp_num, trn_num]] *
              init_transitions[[trn_num, hyp_num]]
          )
        }
      }
    }

    # Make sure to zero out the deleted node's values
    hypotheses[delete_num] <- 0
    transitions[delete_num, ] <- 0
    transitions[, delete_num] <- 0

    # At this point, a single node has been removed from the graph
    # Assign the newly calculate hypotheses and transitions to `graph`, and loop
    # to the next node to delete
    graph <- structure(
      list(
        hypotheses = hypotheses,
        transitions = transitions
      ),
      class = "initial_graph"
    )
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
