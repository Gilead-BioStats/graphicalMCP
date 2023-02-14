test_graph2 <- function(graph,
                        p_values,
                        alpha = .05,
                        groups = list(seq_along(graph$hypotheses)),
                        tests = c("bonferroni"),
                        corr = NULL,
                        verbose = TRUE) {
  g_size <- length(graph$hypotheses)
  hyp_names <- names(graph$hypotheses)

  subgraphs <- generate_weights(graph)
  subgraphs_h_vecs <- subgraphs[, seq_len(g_size), drop = FALSE]
  subgraphs_weights <- subgraphs[, seq_len(g_size) + g_size, drop = FALSE]

  test_results <- matrix(
    nrow = nrow(subgraphs_weights),
    ncol = ncol(subgraphs_weights)
  )

  for (row in seq_len(nrow(subgraphs_weights))) {
    weights <- subgraphs_weights[row, ]

    adj_p_values <- p_adjust(p_values, weights, groups, tests, corr)

    test_results[row, ] <- adj_p_values <= alpha
  }

  reject_intersection <- rowSums(test_results) > 0
  # Each hypothesis appears in half of the 2^n intersections hypotheses. Each
  # intersection a hypothesis is in must be rejected to reject the hypothesis
  # globally
  reject_hyps <- (reject_intersection %*% subgraphs_h_vecs) == 2^g_size / 2

  structure(
    list(
      initial_graph = graph,
      p_values = structure(p_values, names = hyp_names),
      alpha = alpha,
      test_used = tests,
      corr = corr,
      hypotheses_rejected = reject_hyps[1, ]
    ),
    class = "graph_report"
  )

}
