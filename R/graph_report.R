#' Apply weighted Bonferroni, parametric, and Simes tests
#'
#' @param graph An initial graph as created by `create_graph()`
#' @param p_values A numeric vector of p-values, one for each hypothesis in the
#'   graph
#' @param alpha A numeric vector of length 1 specifying the un-weighted alpha
#'   level at which to test each hypothesis
#' @param tests A list with three elements, `bonferroni`, `simes`, and
#'   `parametric`. Each element is a list of hypothesis groups to apply the
#'   given test to. Each hypothesis must be specified exactly once, so that the
#'   length of all elements of the list equals the number of hypotheses. The
#'   default is to apply the weighted Bonferroni test to all hypotheses
#' @param corr (Optional) A correlation matrix for the test statistics of
#'   `graph`. Diagonal entries should be 1. A known absence of correlation
#'   should be entered as 0, and unknown correlation should be entered as NA.
#'   This argument is only used for the parametric test. For each element of a
#'   parametric testing group, the correlation matrix for that group's sub-graph
#'   must be fully known if `use_cj` is FALSE. If `use_cj` is TRUE, the
#'   correlation matrix for the whole parametric testing list must be known
#' @param use_cj A logical vector of length one. If FALSE (the default), then
#'   the critical value `c` is re-calculated for each parametric testing group
#'   within each sub-graph. If TRUE, then `c` is calculated once for each
#'   intersection hypothesis
#'
#' @return A `graph_report` object, consisting of
#'   * The initial graph being tested,
#'   * p-values & alpha used for tests,
#'   * Which hypotheses can be rejected, and
#'   * Detailed test results matrix, including the results of
#'   `generate_weights()` & test results for each intersection hypothesis
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
#'
#' corr <- matrix(nrow = 4, ncol = 4)
#' corr[3:4, 3:4] <- .5
#' diag(corr) <- 1
#'
#' test_graph(
#'   g,
#'   p_values = c(.01, .02, .05, .1),
#'   alpha = .05,
#'   tests = list(
#'     bonferroni = 1,
#'     simes = list(c(2)),
#'     parametric = list(c(3, 4))
#'   ),
#'   corr = corr,
#'   use_cj = FALSE
#' )
test_graph <- function(graph,
                       p_values,
                       alpha = .05,
                       tests = list(
                         bonferroni = list(seq_along(graph$hypotheses)),
                         parametric = NULL,
                         simes = NULL
                       ),
                       corr = NULL,
                       use_cj = FALSE) {
  if (use_cj) {
    valid_corr <- !
      corr_has_missing(corr, unlist(tests$parametric))
  } else {
    valid_corr <- !any(
      vapply(
        tests$parametric,
        function(x) corr_has_missing(corr, x),
        logical(1)
      )
    )
  }

  test_names <- c("bonferroni", "parametric", "simes")
  stopifnot(
    "please choose exactly one test per hypothesis" =
      setequal(seq_along(graph$hypotheses), unlist(tests)),
    "'tests' can only be bonferroni, parametric, and simes" =
      setequal(union(names(tests), test_names), test_names),
    "correlation sub-matrix for parametric tests must be complete" = valid_corr
  )

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

    # Weighted Bonferroni test
    res_bonferroni <- lapply(
      tests$bonferroni,
      function(bonf_group) {
        bonferroni(p_values[bonf_group], weights[bonf_group], alpha)
      }
    )

    # Possibly correct at this point? Needs more testing
    cj <- if (use_cj) {
      cj <- solve_c(weights[unlist(tests$parametric)], corr, alpha)
    } else {
      cj <- NULL
    }

    res_parametric <- lapply(
      tests$parametric,
      function(para_group) {
        parametric(
          p_values[para_group],
          weights[para_group],
          alpha,
          corr[para_group, para_group],
          cj
        )
      }
    )

    # This is at a semi-reasonable point
    res_simes <- lapply(
      tests$simes,
      function(simes_group) {
        simes(p_values[simes_group], weights[simes_group], alpha)
      }
    )

    test_results[row, ] <- unlist(
      c(res_bonferroni, res_simes, res_parametric)
    )[hyp_names]
  }

  reject_intersection <- rowSums(test_results) > 0
  reject_hyps <- (reject_intersection %*% subgraphs_h_vecs) == 2^g_size / 2


  # This is kind of print-y stuff that may not belong here ---
  res_names <- c(
    hyp_names,
    paste(hyp_names, "wgt", sep = "_"),
    paste(hyp_names, "test", sep = "_"),
    "rej_Hj"
  )

  names_mat <- matrix(rep(colnames(subgraphs_h_vecs), nrow(subgraphs_h_vecs)),
    nrow = nrow(subgraphs_h_vecs),
    byrow = TRUE
  )

  format_names <- ifelse(subgraphs_h_vecs, names_mat, "")

  weight_res_matrix <- structure(
    cbind(
      as.data.frame(subgraphs_h_vecs),
      as.data.frame(subgraphs_weights),
      as.data.frame(test_results),
      data.frame(rej_Hj = reject_intersection)
    ),
    names = res_names
  )
  # ---

  structure(
    list(
      initial_graph = graph,
      p_values = structure(p_values, names = hyp_names),
      alpha = alpha,
      test_used = tests,
      corr = corr,
      hypotheses_rejected = reject_hyps[1, ],
      test_results = weight_res_matrix
    ),
    class = "graph_report"
  )
}
