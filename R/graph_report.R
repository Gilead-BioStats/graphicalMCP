#' @export
test_graph <- function(graph,
                        p,
                        alpha = .05,
                        groups = list(seq_along(graph$hypotheses)),
                        tests = c("bonferroni"),
                        corr = NULL,
                        verbose = FALSE,
                        critical = FALSE) {
  partial_match_replacements <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  tests <- partial_match_replacements[tests]

  graph_size <- length(graph$hypotheses)
  gw_size <- 2 ^ graph_size - 1
  num_groups <- length(groups)

  hyp_names <- names(graph$hypotheses)
  names(p) <- hyp_names
  if (!is.null(corr)) dimnames(corr) <- list(hyp_names, hyp_names)

  intersections <- generate_weights(graph)
  inter_h_vecs <- intersections[, seq_len(graph_size), drop = FALSE]
  inter_small <- ifelse(
    inter_h_vecs,
    intersections[, seq_len(graph_size) + graph_size],
    NA_real_
  )

  p_adj <- matrix(
    NA_real_,
    nrow = gw_size,
    ncol = num_groups,
    dimnames = list(
      NULL,
      paste0("padj_", vapply(groups, paste, character(1), collapse = "-"))
    )
  )

  for (i in seq_len(gw_size * num_groups)) {
    row <- (i - 1) %/% num_groups + 1
    h <- inter_h_vecs[row, ]
    weights <- inter_small[row, ]

    # This index is periodic over the number of groups
    group_index <- (i - 1) %% num_groups + 1
    group <- groups[[group_index]]
    test <- tests[[group_index]]

    group_in_subgraph <- group[as.logical(h[group])]

    p_adj[row, group_index] <- do.call(
      paste0("p_adjust_", test),
      list(
        p_values = p[group_in_subgraph],
        weights = weights[group_in_subgraph],
        corr = corr[group_in_subgraph, group_in_subgraph]
      )
    )
  }

  p_adj_inter <- apply(p_adj, 1, min) # NA should never exist, so leave them in
  test_inter <- p_adj_inter <= alpha
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha

  structure(
    list(
      inputs = list(
        graph = graph,
        p = p,
        alpha = alpha,
        groups = groups,
        tests = tests,
        corr = corr
      ),
      outputs = list(
        p_adj = p_adj_global,
        rejected = test_global
      ),
      details = if (verbose) {
        list(
          results = cbind(
            inter_small,
            p_adj,
            p_adj_inter,
            res = test_inter
          )
        )
      },
      critical = if (critical) list()
    ),
    class = "graph_report"
  )
}
