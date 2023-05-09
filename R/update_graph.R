#' Delete hypotheses from a graph
#'
#' It is not always obvious what a graph's weights will look like after deleting
#' one or more hypotheses. While [generate_weights()] calculates all sub-graphs'
#' hypothesis weights, `update_graph()` gives a more detailed view for a single
#' set of deletions, including transition weights as well.
#'
#' @param graph An initial graph as returned by [create_graph()]
#' @param keep A logical or integer vector, denoting which hypotheses to
#'   keep/delete. An entry of `FALSE` (or 0) corresponds to a deletion, and
#'   `TRUE` (or 1) corresponds to keeping a hypothesis
#'
#' @return An object of class `updated_graph` with 3 elements
#'   * The initial graph object
#'   * The Boolean vector indicating which hypotheses are kept/deleted
#'   * The updated graph object with specific hypotheses deleted
#' @export
#'
#' @examples
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- create_graph(hypotheses, transitions)
#'
#' # Delete the second hypothesis
#' update_graph(g, c(TRUE, FALSE, TRUE, TRUE))
#' # Equivalent
#' # update_graph(g, c(1, 0, 1, 1))
#'
update_graph <- function(graph, keep) {
  stopifnot(
    "Please update an `initial_graph` object" = class(graph) == "initial_graph",
    "Hypothesis index must be a logical or integer vector" =
      is.logical(keep) || (is.numeric(keep) && all(as.integer(keep) == keep)),
    "Length of hypothesis index must match size of graph" =
      length(graph$hypotheses) == length(keep),
    "Hypothesis index must only contain 0, 1, TRUE, or FALSE" =
      all(keep %in% c(TRUE, FALSE, 0, 1))
  )

  keep <- as.logical(keep)
  initial_graph <- graph
  names(keep) <- names(graph$hypotheses)

  # Iterate over the hypotheses to delete
  for (delete_num in which(!keep)) {
    # Save current state of the graph to use in calculations
    # Also make a copy of graph elements for storing new values in
    init_hypotheses <- hypotheses <- graph$hypotheses
    init_transitions <- transitions <- graph$transitions

    hyp_nums <- seq_along(hypotheses)

    # Loop over hypotheses, calculating new weights based on initial hypothesis
    # weights and storing in `hypotheses`
    for (hyp_num in hyp_nums) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[delete_num]] * init_transitions[[delete_num, hyp_num]]

      denominator <- 1 - init_transitions[[hyp_num, delete_num]] *
        init_transitions[[delete_num, hyp_num]]

      # In this loop, hyp_num is the starting node of the transition, and
      # end_num is the ending node
      # Calculate new transition weights based on original transition weights,
      # and store in `transitions`
      for (end_num in hyp_nums) {
        if (hyp_num == end_num || denominator <= 0) {
          transitions[[hyp_num, end_num]] <- 0
        } else {
          transitions[[hyp_num, end_num]] <- (
            init_transitions[[hyp_num, end_num]] +
              init_transitions[[hyp_num, delete_num]] *
                init_transitions[[delete_num, end_num]]
          ) / denominator
        }
      }
    }

    # Make sure to zero out the deleted node's values
    hypotheses[delete_num] <- 0
    transitions[delete_num, ] <- 0
    transitions[, delete_num] <- 0

    # At this point, a single hypothesis has been removed from the graph.
    # Assign the newly calculated hypotheses and transitions to `graph`, and
    # loop to the next hypothesis to delete
    graph <- create_graph(hypotheses, transitions)
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
