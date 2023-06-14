#' Report details of hypothesis rejections
#'
#' The slower graph testing functions have design choices made that favor ease
#' of interpreting results over speed. Results include hypothesis rejection
#' decisions of course, but also the test values that led to the final result.
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
#'   * Verbose - A matrix with detailed adjusted p-value results
#'   * Critical - A data frame with hypothesis-level test details for each
#'   intersection
#'
#' @rdname testing
#' @seealso [test_graph_fast()], [bonferroni_sequential_cpp()]
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
#' # The default is all Bonferroni with alpha = .05
#' test_graph(g, p)
#'
#' # But tests can be specified at the hypothesis-level
#' test_graph(
#'   graph = g,
#'   p = p,
#'   alpha = .025,
#'   groups = list(1, 2, 3:4),
#'   test_types = c("bonferroni", "simes", "parametric"),
#'   corr = corr
#' )
test_graph <- function(graph,
                       p,
                       alpha = .05,
                       groups = list(seq_along(graph$hypotheses)),
                       test_types = c("bonferroni"),
                       corr = NULL,
                       verbose = FALSE,
                       critical = FALSE) {
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

  # Some useful values ---------------------------------------------------------
  graph_size <- length(graph$hypotheses)
  gw_size <- 2^graph_size - 1
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
    dimnames = list(NULL, paste0("padj_grp", seq_along(groups)))
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
    test <- test_types[[group_index]]

    # Hypotheses to test must be in both the current group and the current
    # intersection
    group_in_inter <- group[as.logical(h[group])]

    # Choose which function to use to adjust p-values - could probably just be
    # an if/else - but this way is robust to adding more tests
    p_adjust_fun <- paste0("p_adjust_", test)
    p_adjust_args <- list(
      p = p[group_in_inter],
      weights = weights[group_in_inter],
      corr = corr[group_in_inter, group_in_inter]
    )[methods::formalArgs(p_adjust_fun)]

    # Then call the chosen p_adjust_ function
    p_adj[inter_index, group_index] <- do.call(p_adjust_fun, p_adjust_args)

    # Calculate critical values
    if (critical) {
      if (length(group_in_inter) == 0) {
        critical_list[[i]] <- NULL
      } else {
        # Same as p_adjust_ above, this section constructs a critical values
        # call with the appropriate arguments based on test type...
        critical_fun <- paste0(test, "_test_vals")
        critical_args <- list(
          p = p[group_in_inter],
          weights = weights[group_in_inter],
          alpha = alpha,
          corr = corr[group_in_inter, group_in_inter]
        )[methods::formalArgs(critical_fun)]

        # ...then executes the call
        df_critical <- do.call(
          critical_fun,
          critical_args
        )
        df_critical$intersection <- inter_index

        critical_list[[i]] <- df_critical
      }
    }
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_cap <- ifelse(p_adj > 1, 1, p_adj) # Adjusted p-values shouldn't exceed 1
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj_cap)) # Min adj-p by intersection
  test_inter <- p_adj_inter <= alpha # Intersection test results

  # The intersection-level adjusted p-values need to be spread out on the
  # hypotheses that are in each intersection. Then take the max for each
  # hypothesis
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha # Hypothesis test results

  detail_results <- if (verbose) {
    list(results = cbind(inter_small, p_adj_cap, p_adj_inter, res = test_inter))
  }

  critical_results <- if (critical) {
    df_crit_res <- do.call(rbind, critical_list)
    if (!any(test_types == "parametric")) df_crit_res[6:7] <- NULL

    list(results = df_crit_res)
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
      outputs = list(p_adj = p_adj_global, rejected = test_global),
      details = detail_results,
      critical = critical_results
    ),
    class = "graph_report"
  )
}
