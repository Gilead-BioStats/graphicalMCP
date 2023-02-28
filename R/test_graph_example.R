test_graph_example <- function(graph,
                               p,
                               alpha = .05,
                               groups = list(seq_along(graph$hypotheses)),
                               tests = "bonferroni",
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
  gw <- generate_weights(graph)
  gw_small <- ifelse(
    gw[, seq_len(graph_size)],
    gw[, seq_len(graph_size) + graph_size],
    NA_real_
  )
  hyps_res <- matrix(
    sample(c(T, F), nrow(gw) * graph_size, replace = TRUE),
    nrow = nrow(gw),
    dimnames = list(NULL, names(graph$hypotheses))
  )
  p_adj_grp <- matrix(
    sample(p, nrow(gw) * length(groups), replace = TRUE) /
      runif(nrow(gw) * length(groups)),
    nrow = nrow(gw),
    dimnames = list(
      NULL,
      paste0("padj_", vapply(groups, paste, character(1), collapse = "-"))
    )
  )

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
        p_adj = p / runif(graph_size),
        rejected = sample(c(T, F), graph_size, replace = TRUE)
      ),
      details = if (verbose) {
        list(
          results = cbind(
            gw_small,
            p_adj_grp = p_adj_grp,
            p_adj = sample(p, nrow(gw), replace = TRUE) / runif(nrow(gw)),
            res = sample(c(T, F), nrow(gw), replace = TRUE)
          )
        )
      },
      critical = if (critical) {
        list(
          results = cbind(
            structure(gw[, seq_len(graph_size)][!is.na(gw_small)], dim = NULL),
            sample(tests, sum(!is.na(gw_small)), replace = TRUE)
          )
        )
      }
    ),
    class = "graph_report"
  )
}
