#' Delete hypotheses from a graph
#'
#' It is not always obvious what a graph's weights will look like after deleting
#' one or more hypotheses. While [`graph_generate_weights()`] calculates all
#' sub-graphs' hypothesis weights, `graph_update()` gives a more detailed view
#' for a single set of deletions, including transition weights.
#'
#' @param graph An initial graph as returned by [graph_create()]
#' @param delete A logical or integer vector, denoting which hypotheses to
#'   keep/delete. A logical vector must match the size of the graph, with one
#'   entry per hypothesis, and it results in "unordered mode," where selected
#'   hypotheses are deleted in sequential order. An integer vector can be any
#'   length, but must only contain valid hypothesis numbers (greater than 0, and
#'   less than or equal to the size of the graph). This will trigger "ordered
#'   mode," where selected hypotheses are deleted in the order they appear in
#'   `delete`
#'
#' @return An object of class `updated_graph` with 4 elements
#'   * The initial graph object
#'   * The updated graph object with specified hypotheses deleted
#'   * A numeric vector indicating which hypotheses were deleted
#'   * When using ordered mode, a list of intermediate graphs for each deletion
#'   step
#'
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
#' g <- graph_create(hypotheses, transitions)
#'
#' # Delete the second and third hypotheses in any order
#' graph_update(g, c(FALSE, TRUE, TRUE, FALSE))
#'
#' # Equivalent in ordered mode
#' graph_update(g, 2:3)
graph_update <- function(graph, delete) {
  # Basic type checking
  stopifnot(
    "Please update an `initial_graph` object" = class(graph) == "initial_graph",
    "Hypothesis index must be a logical or integer vector" =
      is.logical(delete) || (all(as.integer(delete) == delete))
  )

  ordered <- !is.logical(delete)

  # Qualitative checking
  if (ordered) {
    stopifnot(
      "Ordered deletion index must contain only valid hypothesis numbers" =
        all(delete > 0 & delete <= length(graph$hypotheses)),
      "Ordered deletion index must have unique values" =
        length(delete) == length(unique(delete))
    )

    intermediate_graphs <- list(graph)
  } else {
    stopifnot(
      "Length of unordered deletion index must match size of graph" =
        length(graph$hypotheses) == length(delete),
      "Unordered deletion index must only contain TRUE or FALSE" =
        all(delete %in% c(TRUE, FALSE))
    )

    delete <- which(delete)
  }

  initial_graph <- graph
  cume_delete <- integer(0)

  # Iterate over the hypotheses to delete
  for (delete_num in delete) {
    cume_delete <- c(cume_delete, delete_num)

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
    graph <- structure(
      list(hypotheses = hypotheses, transitions = transitions),
      class = "initial_graph",
      title = "Updated graph",
      deleted = cume_delete
    )

    if (ordered) intermediate_graphs <- c(intermediate_graphs, list(graph))
  }

  structure(
    list(
      initial_graph = initial_graph,
      updated_graph = graph,
      deleted = delete,
      intermediate_graphs = if (ordered) intermediate_graphs
    ),
    class = "updated_graph"
  )
}
