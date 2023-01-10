generate_weights_recursive <- function(graph, compact = TRUE) {
	orig_names <- names(graph$hypotheses)
	names(graph$hypotheses) <- seq_along(graph$hypotheses)
	colnames(graph$transitions) <- names(graph$hypotheses)
	rownames(graph$transitions) <- names(graph$hypotheses)

  list_subgraphs <- delete_nodes_recursive(graph)

  subgraphs_restore_names <- lapply(
    list_subgraphs,
    function(graph) {
      names <- orig_names[as.integer(names(graph$hypotheses))]
      names(graph$hypotheses) <- names
      colnames(graph$transitions) <- names
      rownames(graph$transitions) <- names

      graph
    }
  )

  if (!compact) {
    subgraphs_restore_names
  } else {
    subgraphs_restore_names
  }
}

delete_nodes_recursive <- function(graph, last = 0) {
  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

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
delete_node_fast <- function(graph, delete_num) {
  init_hypotheses <- graph$hypotheses
  init_transitions <- graph$transitions

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  for (hyp_num in seq_along(graph$hypotheses)) {
    hypotheses[[hyp_num]] <-
      init_hypotheses[[hyp_num]] +
      init_hypotheses[[delete_num]] * init_transitions[[delete_num, hyp_num]]

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

  updated_graph <- list(
    hypotheses = hypotheses[-delete_num],
    transitions = as.matrix(transitions[-delete_num, -delete_num])
  )
  class(updated_graph) <- "mcp_graph"
  updated_graph
}
