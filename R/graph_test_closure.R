#' Report details of hypothesis rejections
#'
#' The slower graph testing functions have design choices made that favor ease
#' of interpreting results over speed. Results include hypothesis rejection
#' decisions, but also the test values that led to the final result. The
#' functions include options for reporting details using the adjusted p-value
#' method or adjusted weight method.
#'
#' @param graph An initial graph as returned by [graph_create()]
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
#' @param test_values A logical scalar specifying whether hypothesis-level
#'   detail should be included in the results, including calculating adjusted
#'   weights for parametric tests
#'
#' @return A `graph_report` object, a list of 4 elements: `inputs`, `outputs`,
#'   `details`, and `test_values`
#'   * Inputs - A list of the input parameters used to run the test
#'   * Outputs - A list of global test results
#'   * Details - A matrix with detailed adjusted p-value results (graph deletion
#'   sequence for shortcut testing)
#'   * Test values - A data frame with hypothesis-level test details for each
#'   intersection (each step for shortcut testing)
#'
#' @rdname testing
#' @seealso [graph_test_closure_fast()], [graph_test_shortcut_fast()]
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
#' g <- graph_create(hypotheses, transitions)
#' p <- c(.01, .005, .015, .022)
#'
#' corr <- list(NA, matrix(c(1, .5, .5, 1), nrow = 2, byrow = TRUE))
#'
#' # The default is all Bonferroni with alpha = .025
#' graph_test_closure(g, p)
#'
#' # But tests can be specified at the hypothesis-level
#' graph_test_closure(
#'   graph = g,
#'   p = p,
#'   alpha = .025,
#'   groups = list(1:2, 3:4),
#'   test_types = c("bonferroni", "parametric"),
#'   corr = corr
#' )
graph_test_closure <- function(graph,
                               p,
                               alpha = .025,
                               groups = list(seq_along(graph$hypotheses)),
                               test_types = c("bonferroni"),
                               corr = rep(list(NA), length(test_types)),
                               verbose = FALSE,
                               test_values = FALSE) {
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

  test_input_val(
    graph,
    p,
    alpha,
    groups,
    test_types,
    corr,
    verbose,
    test_values
  )

  num_hyps <- length(graph$hypotheses)
  num_groups <- length(groups)

  hyp_names <- names(graph$hypotheses)
  names(p) <- hyp_names

  # Correlation matrix input is easier for end users to input as a list, but
  # it's easier to work with internally as a full matrix, potentially with
  # missing values. This puts all the correlation pieces into one matrix
  new_corr <- matrix(NA, num_hyps, num_hyps)

  for (group_num in seq_along(groups)) {
    new_corr[groups[[group_num]], groups[[group_num]]] <- corr[[group_num]]
  }
  diag(new_corr) <- 1
  corr <- if (any(test_types == "parametric")) new_corr else NULL

  if (!is.null(corr)) dimnames(corr) <- list(hyp_names, hyp_names)

  # Generate weights of the closure --------------------------------------------
  weighting_strategy <- graph_generate_weights(graph)
  matrix_intersections <- weighting_strategy[, seq_len(num_hyps), drop = FALSE]

  # "Compact" representation shows hypothesis weights where a hypothesis is
  # present (even when that weight is 0), and NA where a hypothesis is missing.
  # This form represents the closure with only `num_hyps` columns
  weighting_strategy_compact <- ifelse(
    matrix_intersections,
    weighting_strategy[, seq_len(num_hyps) + num_hyps, drop = FALSE],
    NA_real_
  )

  num_intersections <- nrow(matrix_intersections)

  adjusted_p <- matrix(
    NA_real_,
    nrow = num_intersections,
    ncol = num_groups,
    dimnames = list(NULL, paste0("adj_p_grp", seq_along(groups)))
  )

  # Calculate adjusted p-values ------------------------------------------------
  # Adjusted p-values are calculated for each group in each intersection of the
  # closure
  for (intersection_index in seq_len(num_intersections)) {
    vec_intersection <- matrix_intersections[intersection_index, , drop = TRUE]
    vec_weights <-
      weighting_strategy_compact[intersection_index, , drop = TRUE]

    for (group_index in seq_len(num_groups)) {
      group <- groups[[group_index]]
      test <- test_types[[group_index]]

      # Hypotheses to include in adjusted p-value calculations must be in both
      # the current group and the current intersection
      group_by_intersection <- group[as.logical(vec_intersection[group])]

      # The adjusted p-value for a *group* has varying rules depending on the
      # test type. Each `adjust_p_*` function expects a whole group as input and
      # returns a single value as output (adjusted p-value for the whole group)
      if (test == "bonferroni") {
        adjusted_p[[intersection_index, group_index]] <- adjust_p_bonferroni(
          p[group_by_intersection],
          vec_weights[group_by_intersection]
        )
      } else if (test == "simes") {
        adjusted_p[[intersection_index, group_index]] <- adjust_p_simes(
          p[group_by_intersection],
          vec_weights[group_by_intersection]
        )
      } else if (test == "parametric") {
        adjusted_p[[intersection_index, group_index]] <- adjust_p_parametric(
          p[group_by_intersection],
          vec_weights[group_by_intersection],
          corr[group_by_intersection, group_by_intersection, drop = FALSE]
        )
      } else {
        stop(paste(test, "testing is not supported at this time"))
      }
    }
  }

  # Adjusted p-value summaries -------------------------------------------------
  # The adjusted p-value for an *intersection* is the smallest adjusted p-value
  # for the groups it contains
  adjusted_p_intersection <- apply(adjusted_p, 1, min)
  reject_intersection <-
    adjusted_p_intersection <= (alpha + .Machine$double.eps)

  # The adjusted p-value for a *hypothesis* is the largest adjusted p-value for
  # the intersections containing that hypothesis
  adjusted_p_hypothesis <-
    apply(adjusted_p_intersection * matrix_intersections, 2, max)
  reject_hypothesis <- adjusted_p_hypothesis <= alpha # Hypothesis test results

  # Adjusted p-value details ---------------------------------------------------
  # Reported adjusted p-values shouldn't exceed 1
  detail_results <- list(
    results = cbind(
      weighting_strategy_compact,
      pmin(adjusted_p, 1 + 1e-14),
      adj_p_inter = pmin(adjusted_p_intersection, 1 + 1e-14),
      reject_intersection = reject_intersection
    )
  )

  # Adjusted weight details ----------------------------------------------------
  if (test_values) {
    # Adjusted weights are recorded in a dataframe, which doesn't store in a
    # matrix. So for the test values loops, each group's adjusted weight
    # dataframe is stored in a list. These are the initialized list and counter
    # for indexing into it.
    test_values_index <- 1
    test_values_list <- vector("list", num_intersections * num_groups)

    # adjusted weights are calculated for each group in each intersection of the
    # closure
    for (intersection_index in seq_len(num_intersections)) {
      vec_intersection <-
        matrix_intersections[intersection_index, , drop = TRUE]
      vec_weights <-
        weighting_strategy_compact[intersection_index, , drop = TRUE]

      for (group_index in seq_len(num_groups)) {
        group <- groups[[group_index]]
        test <- test_types[[group_index]]

        # Hypotheses to include in adjusted weight calculations must be in both
        # the current group and the current intersection
        group_by_intersection <- group[as.logical(vec_intersection[group])]

        # adjusted weights, like adjusted p-values, must be calculated at both
        # the group and intersection level. Inputs are for a single group, and
        # output is a dataframe containing adjusted weight test information at
        # the hypothesis/operand level.
        if (test == "bonferroni") {
          test_values_list[[test_values_index]] <- test_values_bonferroni(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            intersection_index
          )
        } else if (test == "simes") {
          test_values_list[[test_values_index]] <- test_values_simes(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            intersection_index
          )
        } else if (test == "parametric") {
          test_values_list[[test_values_index]] <- test_values_parametric(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            intersection_index,
            corr[group_by_intersection, group_by_intersection, drop = FALSE]
          )
        } else {
          stop(paste(test, "testing is not supported at this time"))
        }

        test_values_index <- test_values_index + 1
      }
    }

    df_test_values <- do.call(rbind, test_values_list)

    # "c" value is only used in parametric testing, so there's no need to
    # include this column when there are no parametric groups
    if (!any(test_types == "parametric")) {
      df_test_values[c("c_value", "*")] <- NULL
    }
  }

  # Build the report -----------------------------------------------------------
  # The core output of a test report is the adjusted p-values, rejection
  # decisions, and resulting graph after deleting all rejected hypotheses.
  # Inputs are recorded as well. Details about adjusted p-values and test
  # values are optionally available.
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
        adjusted_p = pmin(adjusted_p_hypothesis, 1 + 1e-14), # Cap reported at 1
        rejected = reject_hypothesis,
        graph = graph_update(graph, !reject_hypothesis)$updated_graph
      ),
      details = if (verbose) detail_results,
      test_values = if (test_values) list(results = df_test_values)
    ),
    class = "graph_report"
  )
}

