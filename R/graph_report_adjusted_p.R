#' Test a graph using the adjusted p-values method
#'
#' @param graph An initial graph as returned by `create_graph()`
#' @param p_values A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global level to test at
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param tests A character vector of tests to apply to the given groups
#' @param corr (Optional) A numeric matrix of correlations between hypotheses'
#'   test statistics
#' @param verbose A logical scalar specifying how detailed the output should be
#'
#' @return A `graph_report2` object, specifying which null hypotheses can be
#'   rejected
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
#' corr[3:4, 3:4] <- .5
#' diag(corr) <- 1
#'
#' corr2 <- matrix(.5, nrow = 4, ncol = 4)
#' diag(corr2) <- 1
#'
#' # The default is all Bonferroni with alpha = .05
#' graphicalMCP:::test_graph2(g, p)
#'
#' # But tests can be specified at the hypothesis-level
#' graphicalMCP:::test_graph2(
#'   graph = g,
#'   p_values = p,
#'   alpha = .025,
#'   groups = list(1, 2, 3:4),
#'   tests = c("bonferroni", "simes", "parametric"),
#'   corr = corr
#' )
test_graph2 <- function(graph,
                        p_values,
                        alpha = .05,
                        groups = list(seq_along(graph$hypotheses)),
                        tests = c("bonferroni"),
                        corr = NULL,
                        verbose = FALSE) {
  g_size <- length(graph$hypotheses)
  hyp_names <- names(graph$hypotheses)

  subgraphs <- generate_weights(graph)
  subgraphs_h_vecs <- subgraphs[, seq_len(g_size), drop = FALSE]
  subgraphs_weights <- subgraphs[, seq_len(g_size) + g_size, drop = FALSE]

  adj_p_j <- vector("numeric", nrow(subgraphs_weights))

  for (row in seq_len(nrow(subgraphs_weights))) {
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
    # Returns one adjusted p-value per group
    adj_p_values <- p_adjust(
      p_values[h],
      weights[h],
      groups_in,
      tests,
      corr[h, h, drop = FALSE]
    )

    adj_p_j[[row]] <- min(1, adj_p_values, na.rm = TRUE)
  }

  adj_p_global <- apply(subgraphs_h_vecs * adj_p_j, 2, max, na.rm = TRUE)
  reject_hyps <- adj_p_global <= alpha

  if (verbose) {
    res_names <- c(
      hyp_names,
      paste(hyp_names, "wgt", sep = "_"),
      "rej_h_j",
      "adj_p_j"
    )

    weight_res_matrix <- structure(
      cbind(
        as.data.frame(subgraphs_h_vecs),
        as.data.frame(subgraphs_weights),
        data.frame(rej_h_j = adj_p_j <= alpha),
        data.frame(adj_p_j = adj_p_j)
      ),
      names = res_names
    )
  }

  structure(
    list(
      initial_graph = graph,
      p_values = structure(p_values, names = hyp_names),
      adj_p_values = structure(adj_p_global, colnames = hyp_names),
      alpha = alpha,
      test_used = list(
        bonferroni = groups[tests == "bonferroni"],
        parametric = groups[tests == "parametric"],
        simes = groups[tests == "simes"]
      ),
      corr = corr,
      hypotheses_rejected = reject_hyps,
      test_results = if (verbose) weight_res_matrix
    ),
    class = "graph_report2"
  )
}
