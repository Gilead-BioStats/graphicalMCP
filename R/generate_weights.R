#' Generate weights for the full closure tree of an MCP graph
#'
#' @param graph An MCP graph as created by `create_graph()`
#'
#' @return A numeric matrix of all subgraph weights. The first half of columns
#'   indicate which hypotheses are included in the given subgraph, and the
#'   second half of columns are the weights
#'
#' @section Performance:

#' An evaluation of a few different methods to generate the weights of all
#' subgraphs according to Bretz et al. 2011 can be found in the perf-tests
#' directory. On a Workbench session with sufficient RAM (Not sure how much
#' CPU), the `gMCP::generateWeights()` method runs significantly faster for the
#' size 2 case, but it grows quickly in both computing time and memory
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
    list(graph),
    unlist(children, recursive = FALSE)
  )
}
