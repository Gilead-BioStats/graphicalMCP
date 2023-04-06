#' Test a graph
#'
#' @param graph An initial graph as returned by `create_graph()`
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global level to test at
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
#'   test_types = c("bonferroni", "simes", "parametric"),
#'   corr = corr
#' )

# basic testing function for export
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

    p_adjust_fun <- paste0("p_adjust_", test)
    p_adjust_args <- list(
      p_values = p[group_in_inter],
      weights = weights[group_in_inter],
      corr = corr[group_in_inter, group_in_inter]
    )[formalArgs(p_adjust_fun)]

    p_adj[inter_index, group_index] <- do.call(p_adjust_fun, p_adjust_args)

    # Calculate critical values
    if (critical) {
      if (length(group_in_inter) == 0) {
        critical_list[[i]] <- NULL
      } else {
        critical_fun <- paste0(test, "_test_vals")
        critical_args <- list(
          p_values = p[group_in_inter],
          weights = weights[group_in_inter],
          alpha = alpha,
          corr = corr[group_in_inter, group_in_inter]
        )[formalArgs(critical_fun)]

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
  p_adj_cap <- ifelse(p_adj > 1, 1, p_adj)
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj_cap))
  test_inter <- p_adj_inter <= alpha
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha

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

#' @rdname testing
#' @export

# slightly faster all-R version of testing; does not validate inputs, generate
# weights, or return any extra details (just hypothesis rejections)
test_graph_fast <- function(graph,
                       p,
                       alpha = .05,
                       groups = list(seq_along(graph$hypotheses)),
                       test_types = c("bonferroni"),
                       corr = NULL,
                       intersections = generate_weights(graph),
                       graph_size = length(graph$hypotheses),
                       gw_size = 2 ^ graph_size - 1,
                       num_groups = length(groups)) {
  inter_h_vecs <- intersections[, seq_len(graph_size), drop = FALSE]
  inter_small <- intersections[, seq_len(graph_size) + graph_size]

  p_adj <- matrix(
    NA_real_,
    nrow = gw_size,
    ncol = num_groups,
    dimnames = list(NULL, paste0("padj_grp", seq_along(groups)))
  )

  # Calculate adjusted p-values ------------------------------------------------
  for (i in seq_len(gw_size * num_groups)) {
    # This index is periodic over the number of intersection hypotheses
    inter_index <- (i - 1) %/% num_groups + 1
    h <- inter_h_vecs[inter_index, ]
    weights <- inter_small[inter_index, ]

    # This index is periodic over the number of groups as i progresses
    group_index <- (i - 1) %% num_groups + 1
    group <- groups[[group_index]]

    # Hypotheses to test must be in both the current group and the current
    # intersection
    group_in_inter <- group[as.logical(h[group])]

    p_adjust_fun <- paste0("p_adjust_", test_types[[group_index]])
    p_adjust_args <- list(
      p_values = p[group_in_inter],
      weights = weights[group_in_inter],
      corr = corr[group_in_inter, group_in_inter]
    )[formalArgs(p_adjust_fun)]

    p_adj[inter_index, group_index] <- do.call(p_adjust_fun, p_adjust_args)
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha

  test_global
}

#' @rdname testing
#' @export

# testing optimized for parametric; does not validate
# inputs, generate weights, or return any extra details (just hypothesis
# rejections) - also assumes pre-calculated critical values
test_graph_fast_parametric <- function(graph,
                            p,
                            alpha = .05,
                            groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            corr = NULL,
                            intersections = generate_weights(graph),
                            inter_list = add_critical(intersections, corr, alpha, groups),
                            graph_size = length(graph$hypotheses),
                            gw_size = 2 ^ graph_size - 1,
                            num_groups = length(groups)) {

  test_res <- vector("list", length(groups))

  # Calculate adjusted p-values ------------------------------------------------
  for (group_num in seq_along(groups)) {
    group <- groups[[group_num]]
    group_size <- length(group)

    test_res[[group_num]] <- apply(
      inter_list[[group_num]],
      1,
      function(inter_row) {
        h <- inter_row[seq_len(group_size)]
        w <- inter_row[seq_len(group_size) + group_size]
        c <- inter_row[2 * group_size + 1]

        parametric_test_fast(p[group], c, w, alpha)
      }
    )
  }

  # results come out transposed from how inputs were...because apply??
  rej_inter <- colSums(do.call(rbind, test_res)) > 0

  rej_local <- colSums(intersections[, seq_along(graph$hypotheses)] * rej_inter)

  rej_local == 2 ^ (graph_size - 1)
}

#' @rdname testing
#' @export

# uses a C++ version of the adjusted p-value calculation
test_graph_fast_simes <- function(graph,
                            p,
                            alpha = .05,
                            groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            corr = NULL,
                            intersections = generate_weights(graph),
                            graph_size = length(graph$hypotheses),
                            gw_size = 2 ^ graph_size - 1,
                            num_groups = length(groups)) {
  # Generate weights -----------------------------------------------------------
  inter_h_vecs <- intersections[, seq_len(graph_size), drop = FALSE]
  inter_small <- intersections[, seq_len(graph_size) + graph_size]

  p_adj <- matrix(
    NA_real_,
    nrow = gw_size,
    ncol = num_groups,
    dimnames = list(NULL, paste0("padj_grp", seq_along(groups)))
  )

  # Calculate adjusted p-values ------------------------------------------------
  for (i in seq_len(gw_size * num_groups)) {
    # This index is periodic over the number of intersection hypotheses
    inter_index <- (i - 1) %/% num_groups + 1
    h <- inter_h_vecs[inter_index, ]
    weights <- inter_small[inter_index, ]

    # This index is periodic over the number of groups as i progresses
    group_index <- (i - 1) %% num_groups + 1
    group <- groups[[group_index]]

    # Hypotheses to test must be in both the current group and the current
    # intersection
    group_in_inter <- group[as.logical(h[group])]

    # p_adjust_fun <- paste0("p_adjust_", test_types[[group_index]], "_cpp")
    # p_adjust_args <- list(
    #   p_values = p[group_in_inter],
    #   weights = weights[group_in_inter],
    #   corr = corr[group_in_inter, group_in_inter]
    # )[formalArgs(p_adjust_fun)]

    p_adj[inter_index, group_index] <- p_adjust_simes_cpp(p[group_in_inter], weights[group_in_inter])
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha

  test_global
}

#' @rdname testing
#' @export

# uses a C++ version of the adjusted p-value calculation which expects weights
# and p-values to be ordered already
test_graph_fast_simes_ordered_cpp <- function(graph,
                                  p,
                                  alpha = .05,
                                  groups = list(seq_along(graph$hypotheses)),
                                  test_types = c("bonferroni"),
                                  corr = NULL,
                                  intersections = generate_weights(graph),
                                  graph_size = length(graph$hypotheses),
                                  gw_size = 2 ^ graph_size - 1,
                                  num_groups = length(groups)) {
  # Some useful values ---------------------------------------------------------
  # graph_size <- length(graph$hypotheses)
  # gw_size <- 2^graph_size - 1
  # num_groups <- length(groups)

  # hyp_names <- names(graph$hypotheses)
  # names(p) <- hyp_names
  # if (!is.null(corr)) dimnames(corr) <- list(hyp_names, hyp_names)

  # Generate weights -----------------------------------------------------------
  simes_ord <- order(p)

  p <- p[simes_ord]
  inter_h_vecs <- intersections[, seq_len(graph_size), drop = FALSE]
  inter_small <- intersections[, simes_ord + graph_size, drop = FALSE]

  p_adj <- matrix(
    NA_real_,
    nrow = gw_size,
    ncol = num_groups,
    dimnames = list(NULL, paste0("padj_grp", seq_along(groups)))
  )

  # Calculate adjusted p-values ------------------------------------------------
  for (i in seq_len(gw_size * num_groups)) {
    # This index is periodic over the number of intersection hypotheses
    inter_index <- (i - 1) %/% num_groups + 1
    h <- inter_h_vecs[inter_index, ]
    weights <- inter_small[inter_index, ]

    # This index is periodic over the number of groups as i progresses
    group_index <- (i - 1) %% num_groups + 1
    group <- groups[[group_index]]
    group_ord <- simes_ord[group]

    # Hypotheses to test must be in both the current group and the current
    # intersection
    group_in_inter <- as.logical(h[group_ord])

    # p_adjust_fun <- paste0("p_adjust_", test_types[[group_index]], "_cpp")
    # p_adjust_args <- list(
    #   p_values = p[group_in_inter],
    #   weights = weights[group_in_inter],
    #   corr = corr[group_in_inter, group_in_inter]
    # )[formalArgs(p_adjust_fun)]

    p_adj[inter_index, group_index] <- p_adjust_simes_ord_simple_cpp(weights[group_in_inter], p[group_in_inter])
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha

  test_global
}

#' @rdname testing
#' @export

# uses an R version of the adjusted p-value calculation which expects weights
# and p-values to be ordered already
test_graph_fast_simes_ordered_r <- function(graph,
                                   p,
                                   alpha = .05,
                                   groups = list(seq_along(graph$hypotheses)),
                                   test_types = c("bonferroni"),
                                   corr = NULL,
                                   intersections = generate_weights(graph),
                                   graph_size = length(graph$hypotheses),
                                   gw_size = 2 ^ graph_size - 1,
                                   num_groups = length(groups)) {
  simes_ord <- order(p)

  p <- p[simes_ord]
  inter_h_vecs <- intersections[, seq_len(graph_size), drop = FALSE]
  inter_small <- intersections[, simes_ord + graph_size, drop = FALSE]

  p_adj <- matrix(
    NA_real_,
    nrow = gw_size,
    ncol = num_groups,
    dimnames = list(NULL, paste0("padj_grp", seq_along(groups)))
  )

  # Calculate adjusted p-values ------------------------------------------------
  for (i in seq_len(gw_size * num_groups)) {
    # This index is periodic over the number of intersection hypotheses
    inter_index <- (i - 1) %/% num_groups + 1
    h <- inter_h_vecs[inter_index, ]
    weights <- inter_small[inter_index, ]

    # This index is periodic over the number of groups as i progresses
    group_index <- (i - 1) %% num_groups + 1
    group <- groups[[group_index]]
    group_ord <- simes_ord[group]

    # Hypotheses to test must be in both the current group and the current
    # intersection
    group_in_inter <- as.logical(h[group_ord])

    # p_adjust_fun <- paste0("p_adjust_", test_types[[group_index]], "_cpp")
    # p_adjust_args <- list(
    #   p_values = p[group_in_inter],
    #   weights = weights[group_in_inter],
    #   corr = corr[group_in_inter, group_in_inter]
    # )[formalArgs(p_adjust_fun)]

    p_adj[inter_index, group_index] <- p_adjust_simes_ordered(p[group_in_inter], weights[group_in_inter])
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_global <- apply(p_adj_inter * inter_h_vecs, 2, max)
  test_global <- p_adj_global <= alpha

  test_global
}
