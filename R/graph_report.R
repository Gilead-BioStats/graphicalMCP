#' Test a graph
#'
#' @param graph An initial graph as returned by `create_graph()`
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global level to test at
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param tests A character vector of tests to apply to the given groups
#' @param corr (Optional) A numeric matrix of correlations between hypotheses'
#'   test statistics
#' @param verbose A logical scalar specifying whether the results for each
#'   intersection hypothesis should be included
#' @param critical A logical scalar specifying whether hypothesis-level detail
#'   should be included in the results, including calculating critical values
#'   for parametric tests
#'
#' @return A `graph_report` object, specifying which null hypotheses can be
#'   rejected
#' @export
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
#'
#' g <- create_graph(hypotheses, transitions)
#' p <- c(.01, .005, .015, .022)
#'
#' corr <- matrix(nrow = 4, ncol = 4)
#' corr[3:4, 3:4] <- .9
#' diag(corr) <- 1
#'
#' corr2 <- matrix(.5, nrow = 4, ncol = 4)
#' diag(corr2) <- 1
#'
#' # The default is all Bonferroni with alpha = .05
#' test_graph(g, p)
#'
#' # But tests can be specified at the hypothesis-level
#' test_graph(
#'   graph = g,
#'   p = p,
#'   alpha = .025,
#'   groups = list(1, 2, 3:4),
#'   tests = c("bonferroni", "simes", "parametric"),
#'   corr = corr
#' )
test_graph <- function(graph,
                        p,
                        alpha = .05,
                        groups = list(seq_along(graph$hypotheses)),
                        tests = c("bonferroni"),
                        corr = NULL,
                        verbose = FALSE,
                        critical = FALSE) {
  # TODO: Input val ------------------------------------------------------------

  # Some useful values ---------------------------------------------------------
  partial_match_replacements <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  tests <- partial_match_replacements[tolower(tests)]

  graph_size <- length(graph$hypotheses)
  gw_size <- 2 ^ graph_size - 1
  num_groups <- length(groups)

  hyp_names <- names(graph$hypotheses)
  names(p) <- hyp_names
  if (!is.null(corr)) dimnames(corr) <- list(hyp_names, hyp_names)

  # Generate weights -----------------------------------------------------------
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

  critical_list <- if (critical) vector("list", gw_size * num_groups)

  # Calculate adjusted p-values ------------------------------------------------
  for (i in seq_len(gw_size * num_groups)) {
    # This index is periodic over the number of intersection hypotheses
    inter_index <- (i - 1) %/% num_groups + 1
    h <- inter_h_vecs[inter_index, ]
    weights <- inter_small[inter_index, ]

    # This index is periodic over the number of groups as i progresses
    group_index <- (i - 1) %% num_groups + 1
    group <- groups[[group_index]]
    test <- tests[[group_index]]

    # Hypotheses to test must be in both the current group and the current
    # intersection
    group_in_inter <- group[as.logical(h[group])]

    p_adj[inter_index, group_index] <- do.call(
      paste0("p_adjust_", test),
      list(
        p_values = p[group_in_inter],
        weights = weights[group_in_inter],
        corr = corr[group_in_inter, group_in_inter]
      )
    )

    # Calculate critical values
    # At this point, we have a group like 1-2-5-7,
    if (critical) {
      if (length(group_in_inter) == 0) {
        critical_list[[i]] <- NULL
      } else {
        df_critical <- do.call(
          paste0(test, "_test_vals"),
          list(
            p_values = p[group_in_inter],
            weights = weights[group_in_inter],
            alpha = alpha,
            corr = corr[group_in_inter, group_in_inter]
          )
        )
        df_critical$intersection <- inter_index

        critical_list[[i]] <- df_critical
      }
    }
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  test_inter <- p_adj_inter <= alpha
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha

  detail_results <- if (verbose) {
    list(results = cbind(inter_small, p_adj, p_adj_inter, res = test_inter))
  }

  critical_results <- if (critical) {
    df_crit_res <- do.call(rbind, critical_list)
    if (!any(tests == "parametric")) df_crit_res[6:7] <- NULL

    list(results = df_crit_res)
  }

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
      outputs = list(p_adj = p_adj_global, rejected = test_global),
      details = detail_results,
      critical = critical_results
    ),
    class = "graph_report"
  )
}
