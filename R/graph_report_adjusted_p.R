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

  adj_p_results <- matrix(
    nrow = nrow(subgraphs_weights),
    ncol = ncol(subgraphs_weights)
  )

  adj_p_tilde <- vector("numeric", nrow(subgraphs_weights))

  for (row in seq_len(nrow(subgraphs_weights))) {
    # if (row == 3) browser()
    h <- as.logical(subgraphs_h_vecs[row, ])
    weights <- subgraphs_weights[row, ]

    # Need to remove indices from groups that aren't in this intersection
    # Furthermore, need to pass them in as names rather than positions to
    # account for some indices being removed
    groups_in <- lapply(
      groups,
      function(group) hyp_names[group][as.logical(h[group])]
    )

    # Only calculate adjusted p-values for hypotheses in this intersection
    # But add NA for hypotheses that are missing
    adj_p_values <- p_adjust(
      p_values[h],
      weights[h],
      groups_in,
      tests,
      corr[h, h]
    )[hyp_names]

    test_results[row, ] <- adj_p_values <= alpha

    adj_p_results[row, ] <- adj_p_values

    adj_p_tilde[[row]] <- min(1, adj_p_values, na.rm = TRUE)
  }

  adj_p_global1 <- apply(adj_p_results, 2, max, na.rm = TRUE)
  adj_p_global <- apply(adj_p_tilde * subgraphs_h_vecs, 2, max)

  reject_intersection <- rowSums(test_results, na.rm = TRUE) > 0
  # Each hypothesis appears in half of the 2^n intersections hypotheses. Each
  # intersection a hypothesis is in must be rejected to reject the hypothesis
  # globally
  reject_hyps <- (reject_intersection %*% subgraphs_h_vecs) == 2^g_size / 2

  if (verbose) {
    # Removes the "c *" columns from the detail dataframe when using only Simes
    # & Bonferroni
    # if (length(tests$parametric) == 0) test_details[, 3:4] <- NULL

    res_names <- c(
      hyp_names,
      paste(hyp_names, "wgt", sep = "_"),
      paste(hyp_names, "test", sep = "_"),
      "rej_Hj",
      paste(hyp_names, "padj", sep = "_"),
      "p_tilde"
    )

    weight_res_matrix <- structure(
      cbind(
        as.data.frame(subgraphs_h_vecs),
        as.data.frame(subgraphs_weights),
        as.data.frame(test_results),
        data.frame(rej_Hj = reject_intersection),
        as.data.frame(adj_p_results),
        data.frame(p_tilde = adj_p_tilde)
      ),
      names = res_names
    )

  }

  structure(
    list(
      initial_graph = graph,
      p_values = structure(p_values, names = hyp_names),
      adj_p_values = structure(adj_p_global, colnames = hyp_names),
      adj_p_values1 = adj_p_global1,
      alpha = alpha,
      test_used = tests,
      corr = corr,
      hypotheses_rejected = reject_hyps[1, ],
      test_results = if (verbose) weight_res_matrix
    ),
    class = "graph_report"
  )

}
