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
#' generate_weights_recursive(ex_graph)
#'
generate_weights_recursive <- function(graph) {
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
  hypotheses <- graph$hypotheses

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
    smaller_graph <- delete_node_fast(graph, del_index)

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

# good for now - could convert to cpp at some point
# prior solution by Dong/Spencer is faster as well, but uses more memory?
delete_node_fast <- function(graph, delete_num) {
  init_hypotheses <- graph$hypotheses
  init_transitions <- graph$transitions

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  hyp_nums <- seq_along(hypotheses)[seq_along(hypotheses) != delete_num]

  for (hyp_num in hyp_nums) {
    hypotheses[[hyp_num]] <-
      init_hypotheses[[hyp_num]] +
      init_hypotheses[[delete_num]] * init_transitions[[delete_num, hyp_num]]

    denom <- 1 - init_transitions[[hyp_num, delete_num]] *
      init_transitions[[delete_num, hyp_num]]

    for (end_num in hyp_nums) {
      if (hyp_num == end_num || denom <= 0) {
        transitions[[hyp_num, end_num]] <- 0
      } else {
        transitions[[hyp_num, end_num]] <- (
          init_transitions[[hyp_num, end_num]] +
            init_transitions[[hyp_num, delete_num]] *
              init_transitions[[delete_num, end_num]]
        ) / denom
      }
    }
  }

  structure(
    list(
      hypotheses = hypotheses[-delete_num],
      transitions = as.matrix(transitions[-delete_num, -delete_num])
    ),
    class = "initial_graph"
  )
}
