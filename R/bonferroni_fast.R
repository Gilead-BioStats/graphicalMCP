bonferroni_fast <- function(graph, p_values, alpha = .05) {
  initial_graph <- graph
  hyp_names <- names(graph$hypotheses)

  p_max <- 0
  adj_p_values <- vector("numeric", length(graph$hypotheses))

  for (i in seq_along(graph$hypotheses)) {
    adj_p_vec <- adjust_p(p_values, graph$hypotheses, "bonferroni")
    min_index <- which.min(adj_p_vec)
    adj_p_j <- max(
      p_values[[min_index]] / graph$hypotheses[[min_index]],
      p_max
    )

    adj_p_values[[min_index]] <- adj_p_j
    p_max <- adj_p_j
    graph <- zero_node_fast(graph, min_index)
  }

  reject_hyps <- adj_p_values <= alpha

  structure(
    list(
      initial_graph = initial_graph,
      p_values = structure(p_values, names = hyp_names),
      adj_p_values = structure(adj_p_values, names = hyp_names),
      alpha = alpha,
      test_used = "bonferroni",
      hypotheses_rejected = reject_hyps
    ),
    class = "graph_report2"
  )
}
