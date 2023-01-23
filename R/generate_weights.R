#' Generate weights for each intersections hypothesis in the full closure tree
#' of an MCP graph
#'
#' @param graph An MCP graph as created by `create_graph()`
#'
#' @return A numeric matrix of all intersection hypothesis weights. Each row
#'   corresponds to a single intersection hypothesis. The first half of the
#'   columns indicate which hypotheses are included in the given intersection
#'   hypothesis, and the second half of columns are the weights
#'
#' @section Performance:

#' Much thought was given to the performance of this code, as the memory and
#' time usage can grow quickly as graph size grows. On the systems used for
#' testing, a size 10 graph had a median run time of 20-60 ms. Run time
#' increases at a rate of O(2 ^ n), so e.g. a size 5 graph takes approximately
#' twice as long to run as a size 4 graph
#'
#' @export
#'
#' @examples
#'
#' ex_graph <- create_graph(
#'   hypotheses = c(.5, .5, 0, 0),
#'   transitions = rbind(
#'     c(0, 0, 1, 0),
#'     c(0, 0, 0, 1),
#'     c(0, 1, 0, 0),
#'     c(1, 0, 0, 0)
#'   )
#' )
#'
#' generate_weights(ex_graph)
#'
generate_weights <- function(graph) {
  orig_names <- names(graph$hypotheses)
  names(graph$hypotheses) <- seq_along(graph$hypotheses)
  colnames(graph$transitions) <- names(graph$hypotheses)
  rownames(graph$transitions) <- names(graph$hypotheses)

  list_subgraphs <- delete_nodes_recursive(graph)

  wgts_mat <- structure(
    do.call(
      rbind,
      lapply(
        list_subgraphs,
        function(graph) graph$hypotheses[as.character(seq_along(orig_names))]
      )
    ),
    dimnames = list(1:(2^length(orig_names) - 1), orig_names)
  )

  wgts_mat_h <- !is.na(wgts_mat)
  wgts_mat[is.na(wgts_mat)] <- 0

  cbind(wgts_mat_h, wgts_mat)
}

#' Delete a single hypothesis from a graph
#'
#' @param graph An MCP graph as created by `create_graph()`, with integer names.
#'   The names are relied upon down into the recursion to remember where each
#'   node occurred in the original graph
#' @param last The numeric position of the last node deleted. All nodes larger
#'   than 'last' will be recursively deleted and resulting graphs returned
#'
#' @return A list of `initial_graph` objects
#' * If 'graph' is a single vertex, it will be returned as the only element of
#'   the list
#' * If 'last' is larger than the largest hypothesis __name__ in 'graph', then
#'   'graph' will again be returned as the only element of the list. These two
#'   make up the base case of the recursion
#' * If neither of the prior conditions is met, each node larger than 'last'
#'   will be deleted, and `delete_nodes_recursive()` will be called on each of
#'   the resulting sub-graphs. The original graph, as well as the results of the
#'   recursive calls, will be returned in a list
#'
#' @noRd
#'
#' @examples
#'
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' names <- 1:4
#' g <- create_graph(hypotheses, transitions, names)
#'
#' # Returns a list of all subgraphs of 'g'
#' delete_nodes_recursive(g)
#'
#' # Simulate the step right after deleting node 2 from `bonferroni_holm(4)`
#' delete_nodes_recursive(bonferroni_holm(3, names = c(1, 3:4)), last = 2)
delete_nodes_recursive <- function(graph, last = 0) {
  init_hypotheses <- hypotheses <- graph$hypotheses
  init_transitions <- transitions <- graph$transitions

  # base case
  int_hyp <- as.integer(names(hypotheses))

  is_single_node <- length(hypotheses) == 1
  last_is_bigger <- last > max(int_hyp)

  if (is_single_node || last_is_bigger) {
    return(list(graph))
  }

  # recursive step
  children <- list()

  for (orig_hyp_num in int_hyp[int_hyp > last]) {
    del_index <- match(orig_hyp_num, int_hyp)
    hyp_nums <- seq_along(hypotheses)[seq_along(hypotheses) != del_index]

    for (hyp_num in hyp_nums) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[del_index]] * init_transitions[[del_index, hyp_num]]

      denominator <- 1 - init_transitions[[hyp_num, del_index]] *
        init_transitions[[del_index, hyp_num]]

      for (end_num in hyp_nums) {
        if (hyp_num == end_num || denominator <= 0) {
          transitions[[hyp_num, end_num]] <- 0
        } else {
          transitions[[hyp_num, end_num]] <- (
            init_transitions[[hyp_num, end_num]] +
              init_transitions[[hyp_num, del_index]] *
              init_transitions[[del_index, end_num]]
          ) / denominator
        }
      }
    }

    smaller_graph <- structure(
      list(
        hypotheses = hypotheses[-del_index],
        transitions = as.matrix(transitions[-del_index, -del_index])
      ),
      class = "initial_graph"
    )

    children[[del_index]] <- delete_nodes_recursive(
      smaller_graph,
      orig_hyp_num
    )
  }

  c(
    unlist(children, recursive = FALSE),
    list(graph)
  )
}
