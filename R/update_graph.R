#' `updated_graph` object
#'
#' Creates a list that represents an updated graph with specific hypotheses
#' deleted.
#'
#' @param graph An initial graph as created by `create_graph()`
#' @param keep A vector coercible to Boolean, denoting which hypotheses to
#'   keep/delete. An entry of `FALSE` (or 0) corresponds to a deletion, and
#'   `TRUE` (or any number other than 0) corresponds to keeping a hypothesis. We
#'   recommend using either a 1/0 vector or a TRUE/FALSE vector for clarity
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
#' # Equivalent call
#' # update_graph(g, c(1, 0, 1, 1))
#'
update_graph <- function(graph, keep) {
  stopifnot(
    "'keep' must be a logical or numeric vector" =
      (is.logical(keep) || is.numeric(keep)),
    "length of 'keep' must match number of hypotheses in 'graph'" =
      length(graph$hypotheses) == length(keep)
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

#' @export
update_graph2 <- function(graph, remove) {
  update_graph_cpp(graph, remove)
}
