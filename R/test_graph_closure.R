#' Report details of hypothesis rejections
#'
#' The slower graph testing functions have design choices made that favor ease
#' of interpreting results over speed. Results include hypothesis rejection
#' decisions, but also the test values that led to the final result.
#' The functions include options for reporting details using the adjusted
#' p-value method or critical value method.
#'
#' @param graph An initial graph as returned by [create_graph()]
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param test_types A character vector of tests to apply to the given groups
#' @param corr (Optional) A numeric matrix of correlations between hypotheses'
#'   test statistics
#' @param verbose A logical scalar specifying whether the results for each
#'   intersection hypothesis should be included
#' @param critical A logical scalar specifying whether hypothesis-level detail
#'   should be included in the results, including calculating critical values
#'   for parametric tests
#'
#' @return A `graph_report` object, a list of 4 elements: `inputs`, `outputs`,
#'   `verbose`, and `critical`
#'   * Inputs - A list of the input parameters used to run the test
#'   * Outputs - A list of global test results
#'   * Verbose - A matrix with detailed adjusted p-value results (graph deletion
#'     sequence for shortcut testing)
#'   * Critical - A data frame with hypothesis-level test details for each
#'   intersection (each step for shortcut testing)
#'
#' @rdname testing
#' @seealso [test_graph_fast()], [test_graph_shortcut_cpp()]
#' @export
#'
#' @template references
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
#' # The default is all Bonferroni with alpha = .025
#' test_graph_closure(g, p)
#'
#' # But tests can be specified at the hypothesis-level
#' test_graph_closure(
#'   graph = g,
#'   p = p,
#'   alpha = .025,
#'   groups = list(1, 2, 3:4),
#'   test_types = c("bonferroni", "simes", "parametric"),
#'   corr = corr
#' )
test_graph_closure <- function(graph,
                               p,
                               alpha = .025,
                               groups = list(seq_along(graph$hypotheses)),
                               test_types = c("bonferroni"),
                               corr = NULL,
                               verbose = FALSE,
                               critical = FALSE) {
  # Input validation & sanitization --------------------------------------------
  # Test types should be specified as full names or first initial,
  # case-insensitive. A single provided test type should be applied to all
  # groups.
  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  test_types <- test_opts[tolower(test_types)]
  if (length(test_types) == 1) test_types <- rep(test_types, length(groups))

  test_input_val(graph, p, alpha, groups, test_types, corr, verbose, critical)

  num_hyps <- length(graph$hypotheses)
  closure_rows <- 2^num_hyps - 1 # "- 1" for the null sub-graph
  num_groups <- length(groups)

  hyp_names <- names(graph$hypotheses)
  names(p) <- hyp_names
  if (!is.null(corr)) dimnames(corr) <- list(hyp_names, hyp_names)

  # Generate weights of the closure --------------------------------------------
  closure_standard <- generate_weights(graph)
  closure_presence <- closure_standard[, seq_len(num_hyps), drop = FALSE]

  # "Compact" representation shows hypothesis weights where a hypothesis is
  # present (even when that weight is 0), and NA where a hypothesis is missing.
  # This form represents the closure with only `num_hyps` columns
  closure_compact <- ifelse(
    closure_presence,
    closure_standard[, seq_len(num_hyps) + num_hyps, drop = FALSE],
    NA_real_
  )

  adjusted_p <- matrix(
    NA_real_,
    nrow = closure_rows,
    ncol = num_groups,
    dimnames = list(NULL, paste0("padj_grp", seq_along(groups)))
  )

  critical_index <- 1
  critical_list <- if (critical) vector("list", closure_rows * num_groups)

  # Calculate adjusted p-values ------------------------------------------------
  # Adjusted p-values are calculated for each group in each intersection of the
  # closure
  for (intersection_index in seq_len(closure_rows)) {
    hyp_presence <- closure_presence[intersection_index, ]
    weights <- closure_compact[intersection_index, ]

    for (group_index in seq_len(num_groups)) {
      group <- groups[[group_index]]
      test <- test_types[[group_index]]

      # Hypotheses to include in adjusted p-value calculations must be in both
      # the current group and the current intersection
      group_x_intersection <- group[as.logical(hyp_presence[group])]

      # Each `p_adjust_*` function expects a whole group as input and returns a
      # single value as output (adjusted p-value for the whole group)
      if (test == "bonferroni") {
        adjusted_p[[intersection_index, group_index]] <- p_adjust_bonferroni(
          p[group_x_intersection],
          weights[group_x_intersection]
        )
      } else if (test == "simes") {
        adjusted_p[[intersection_index, group_index]] <- p_adjust_simes(
          p[group_x_intersection],
          weights[group_x_intersection]
        )
      } else if (test == "parametric") {
        adjusted_p[[intersection_index, group_index]] <- p_adjust_parametric(
          p[group_x_intersection],
          weights[group_x_intersection],
          corr[group_x_intersection, group_x_intersection]
        )
      } else {
        stop(paste(test, "testing is not supported at this time"))
      }

      # Critical values, like adjusted p-values, must be calculated at both the
      # group and intersection level. Inputs are for a single group, and output
      # is a dataframe containing critical value test information at the
      # hypothesis/operand level.
      if (critical) {
        if (test == "bonferroni") {
          critical_list[[critical_index]] <- bonferroni_test_vals(
            p[group_x_intersection],
            weights[group_x_intersection],
            alpha,
            intersection_index
          )
        } else if (test == "simes") {
          critical_list[[critical_index]] <- simes_test_vals(
            p[group_x_intersection],
            weights[group_x_intersection],
            alpha,
            intersection_index
          )
        } else if (test == "parametric") {
          critical_list[[critical_index]] <- parametric_test_vals(
            p[group_x_intersection],
            weights[group_x_intersection],
            alpha,
            intersection_index,
            corr[group_x_intersection, group_x_intersection]
          )
        } else {
          stop(paste(test, "testing is not supported at this time"))
        }

        critical_index <- critical_index + 1
      }
    }
  }

  # Adjusted p-value summaries -------------------------------------------------
  # Adjusted p-values shouldn't exceed 1
  adjusted_p_cap <- ifelse(adjusted_p > 1, 1, adjusted_p)

  # The adjusted p-value for an *intersection* is the smallest adjusted p-value
  # for the groups it contains
  adjusted_p_intersection <- apply(adjusted_p_cap, 1, min)
  reject_intersection <- adjusted_p_intersection <= alpha

  # The adjusted p-value for a *hypothesis* is the largest adjusted p-value for
  # the intersections containing that hypothesis
  adjusted_p_global <- apply(adjusted_p_intersection * closure_presence, 2, max)
  reject_global <- adjusted_p_global <= alpha # Hypothesis test results

  # Adjusted p-value details ---------------------------------------------------
  detail_results <- if (verbose) {
    list(
      results = cbind(
        closure_compact,
        adjusted_p_cap,
        adjusted_p_intersection,
        rej = reject_intersection
      )
    )
  }

  # Critical value details -----------------------------------------------------
  critical_results <- if (critical) {
    df_critical_results <- do.call(rbind, critical_list)

    # "c" value is only used in parametric testing, so there's no need to
    # include this column when there are no parametric groups
    if (!any(test_types == "parametric")) {
      df_critical_results[c("c", "*")] <- NULL
    }

    list(results = df_critical_results)
  }

  structure(
    list(
      inputs = list(
        graph = graph,
        p = p,
        alpha = alpha,
        groups = groups,
        test_types = test_types,
        corr = corr
      ),
      outputs = list(
        adjusted_p = adjusted_p_global,
        rejected = reject_global,
        graph = update_graph(graph, !reject_global)$updated_graph
      ),
      details = detail_results,
      critical = critical_results
    ),
    class = "graph_report"
  )
}
