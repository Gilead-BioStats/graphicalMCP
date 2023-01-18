# ------------------------------------------------------------------------------
generate_weights_recursive_vec <- function(graph) {
  orig_names <- names(graph$hypotheses)
  names(graph$hypotheses) <- seq_along(graph$hypotheses)
  colnames(graph$transitions) <- names(graph$hypotheses)
  rownames(graph$transitions) <- names(graph$hypotheses)

  list_subgraphs <- delete_nodes_recursive_vec(graph)

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

delete_nodes_recursive_vec <- function(graph, last = 0) {
  hypotheses <- graph$hypotheses

  # base case
  is_single_node <- length(hypotheses) == 1
  last_is_bigger <- last > max(as.integer(names(hypotheses)))

  if (is_single_node || last_is_bigger) {
    return(list(graph))
  }

  # recursive step
  children <- list()
  int_hyp <- as.integer(names(hypotheses))

  for (orig_hyp_num in int_hyp[int_hyp > last]) {
    del_index <- match(orig_hyp_num, names(hypotheses))
    smaller_graph <- delete_node_vec(graph, del_index)

    children[[del_index]] <- delete_nodes_recursive_vec(
      smaller_graph,
      orig_hyp_num
    )
  }

  c(
    list(graph),
    unlist(children, recursive = FALSE)
  )
}

# Vectorised and simplified version of delete_node_fast()
# Uses strategies from ./perf-tests/examples.R
# Maybe actually slower though? Seems to depend on the day
delete_node_vec <- function(graph, j) {
  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  hypotheses <- hypotheses[-j] + hypotheses[j] * transitions[j, -j]

  transitions <- (
    transitions[-j, -j] + transitions[-j, j] %o% transitions[j, -j]
  ) / (
    1 - transitions[-j, -j] * t(transitions[-j, -j])
  )

  diag(transitions) <- 0

  structure(
    list(
      hypotheses = hypotheses,
      transitions = transitions
    ),
    class = "initial_graph"
  )
}
