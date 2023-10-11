#' Report details of hypothesis rejections
#'
#' The graph testing functions apply a specified test strategy to a graph and a
#' set of p-values, giving rich information on which hypotheses are significant,
#' and why. Results include hypothesis rejection decisions, but also the test
#' values that led to the final result. The functions include options for
#' reporting details using the adjusted p-value method or adjusted significance
#' method.
#'
#' The test specification (`test_groups`, `test_types`, and `test_corr`) can be
#' specified either named or unnamed. If unnamed, it's assumed that all 3 are
#' ordered the same way, i.e. the nth elements of `test_types` and `test_corr`
#' apply to the nth group in `test_groups`. Naming each element with consistent
#' names across the three vectors will mean that they do not have to retain the
#' same order, and also may be a more robust way to keep track of testing
#' details.
#'
#' @param graph An initial graph as returned by [graph_create()]
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the significance level for testing
#' @param test_groups A list of numeric vectors specifying hypotheses to test
#'   together. Part 1 of the test specification.
#' @param test_types A character vector of tests to apply to the test groups.
#'   Part 2 of the test specification.
#' @param test_corr (Optional) A list of numeric matrices. Part 3 of the test
#'   specification. Each test group must have exactly one entry in the list:
#'   Bonferroni and Simes test groups should have entries of NULL, and
#'   parametric test groups should have numeric matrices specifying the (known
#'   or estimated) pairwise correlations between the test statistics of all
#'   hypotheses in the group.
#' @param verbose A logical scalar specifying whether the details of the
#'   adjusted p-value calculations should be included in results
#' @param test_values A logical scalar specifying whether details of the
#'   adjusted significance calculations should be included in results
#'
#' @return A `graph_report` object, a list of 4 elements: `inputs`, `outputs`,
#'   `details`, and `test_values`
#'   * Inputs - A list of the input parameters used to run the test
#'   * Outputs - A list of global test results
#'   * Details - A list of detailed adjusted p-value calculations (graph
#'   deletion sequence for shortcut testing)
#'   * Test values - A list with hypothesis-level test details for each
#'   intersection using the adjusted significance method (Details about each
#'   step taken for shortcut testing)
#'
#' @rdname testing
#'
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
#' corr_list <- list(NA, matrix(c(1, .5, .5, 1), nrow = 2, byrow = TRUE))
#'
#' # The default is all Bonferroni with alpha = .025
#' graph_test_closure(g, p)
#'
#' # But tests can be specified at the hypothesis-level
#' graph_test_closure(
#'   graph = g,
#'   p = p,
#'   alpha = .025,
#'   test_groups = list(1:2, 3:4),
#'   test_types = c("bonferroni", "parametric"),
#'   test_corr = corr_list
#' )
graph_test_closure <- function(graph,
                               p,
                               alpha = .025,
                               test_groups = list(seq_along(graph$hypotheses)),
                               test_types = c("bonferroni"),
                               test_corr = rep(list(NA), length(test_types)),
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
  if (length(test_types) == 1) {
    test_types <- rep(test_types, length(test_groups))
  }

  test_input_val(
    graph,
    p,
    alpha,
    test_groups,
    test_types,
    test_corr,
    verbose,
    test_values
  )

  # The test specification arguments can be named or not. However, if
  # `test_groups` is named, all of them must be named. The other two are
  # re-ordered to match `test_groups`
  if (!is.null(names(test_groups))) {
    if (!all(names(c(test_types, test_corr)) %in% names(test_groups))) {
      stop("If `test_groups` is named, `test_types` and `test_corr` must use the
           same names")
    } else {
      test_types <- test_types[names(test_groups)]
      test_corr <- test_corr[names(test_groups)]
    }
  } else {
    names(test_groups) <-
      names(test_types) <-
      names(test_corr) <-
      paste0("grp", seq_along(test_groups))
  }

  num_hyps <- length(graph$hypotheses)
  num_groups <- length(test_groups)

  hyp_names <- names(graph$hypotheses)
  names(p) <- hyp_names

  # Correlation matrix input is easier for end users to input as a list, but
  # it's easier to work with internally as a full matrix, potentially with
  # missing values. This puts all the correlation pieces into one matrix
  new_corr <- matrix(NA, num_hyps, num_hyps)

  for (group_num in seq_along(test_groups)) {
    new_corr[test_groups[[group_num]], test_groups[[group_num]]] <-
      test_corr[[group_num]]
  }
  diag(new_corr) <- 1
  test_corr <- if (any(test_types == "parametric")) new_corr else NULL

  if (!is.null(test_corr)) dimnames(test_corr) <- list(hyp_names, hyp_names)

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
    dimnames = list(NULL, paste0("adj_p_grp", seq_along(test_groups)))
  )

  # Calculate adjusted p-values ------------------------------------------------
  # Adjusted p-values are calculated for each group in each intersection of the
  # closure
  for (intersection_index in seq_len(num_intersections)) {
    vec_intersection <- matrix_intersections[intersection_index, , drop = TRUE]
    vec_weights <-
      weighting_strategy_compact[intersection_index, , drop = TRUE]

    for (group_index in seq_len(num_groups)) {
      group <- test_groups[[group_index]]
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
          test_corr[group_by_intersection, group_by_intersection, drop = FALSE]
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
    apply(adjusted_p_intersection * matrix_intersections, 2, max, na.rm = TRUE)
  reject_hypothesis <- adjusted_p_hypothesis <= alpha # Hypothesis test results

  # Adjusted p-value details ---------------------------------------------------
  # Reported adjusted p-values shouldn't exceed 1
  intersections <- apply(matrix_intersections, 1, paste, collapse = "")

  detail_results <- list(
    results = cbind(
      data.frame(Intersection = intersections),
      weighting_strategy_compact,
      pmin(adjusted_p, 1 + 1e-14),
      data.frame(adj_p_inter = pmin(adjusted_p_intersection, 1 + 1e-14)),
      data.frame(reject_intersection = reject_intersection)
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

    # Adjusted weights are calculated for each group in each intersection of the
    # closure
    for (intersection_index in seq_len(num_intersections)) {
      vec_intersection <-
        matrix_intersections[intersection_index, , drop = TRUE]
      vec_weights <-
        weighting_strategy_compact[intersection_index, , drop = TRUE]

      str_intersection <- paste(vec_intersection, collapse = "")

      for (group_index in seq_len(num_groups)) {
        group <- test_groups[[group_index]]
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
            str_intersection
          )
        } else if (test == "simes") {
          test_values_list[[test_values_index]] <- test_values_simes(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            str_intersection
          )
        } else if (test == "parametric") {
          test_values_list[[test_values_index]] <- test_values_parametric(
            p[group_by_intersection],
            vec_weights[group_by_intersection],
            alpha,
            str_intersection,
            test_corr[group_by_intersection,
              group_by_intersection,
              drop = FALSE
            ]
          )
        } else {
          stop(paste(test, "testing is not supported at this time"))
        }

        test_values_index <- test_values_index + 1
      }
    }

    df_test_values <- do.call(rbind, test_values_list)
    rownames(df_test_values) <- NULL



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
        test_groups = test_groups,
        test_types = test_types,
        test_corr = test_corr
      ),
      outputs = list(
        adjusted_p = pmin(adjusted_p_hypothesis, 1 + 1e-14), # Cap reported at 1
        rejected = reject_hypothesis,
        graph = graph_update(graph, reject_hypothesis)$updated_graph
      ),
      details = if (verbose) detail_results,
      test_values = if (test_values) list(results = df_test_values)
    ),
    class = "graph_report"
  )
}
