#' Test a graph efficiently
#'
#' For insight and nice reporting, prefer `test_graph()` with all of its
#' options. It is reasonably fast for interactive use. However in order to
#' minimize power run time, more efficient testing functions have been written.
#' The power simulation can be segmented so that certain parts, like generating
#' weights and calculating some critical values, can be done only a single time.
#' The actual testing function has been stripped down to just a few vectorized
#' lines for efficiency
#'
#' @param graph An initial graph as returned by `create_graph()`
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global level to test at
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param test_types A character vector of tests to apply to the given groups
#' @param corr Optional if no `test_types` are parametric. A numeric matrix of
#'   correlations between hypotheses' test statistics
#' @param intersections A numeric matrix of h-vectors and weights as created by
#'   `generate_weights()`. For `test_graph_fast_v()` and
#'   `test_graph_fast_vms()`, the compact representation of the same
#' @param inter_list A list of intersection weights with critical values, as
#'   created by `add_critical()`
#' @param graph_size Number of hypotheses in `graph`
#' @param gw_size Number of rows in `intersections`
#' @param num_groups Number of groups in `groups`
#'
#' @return A logical vector of results indicating whether each hypothesis can be
#'   accepted or rejected globally
#'
#' @rdname testing-fast
#' @export
#'
#' @examples
#' par_gate <- simple_successive_1()
#' graph_size <- length(par_gate$hypotheses)
#'
#' p <- c(.001, .02, .002, .03)
#'
#' weights <- generate_weights(par_gate)
#' compact_weights <- ifelse(
#'   weights[, seq_len(graph_size), drop = FALSE],
#'   weights[, seq_len(graph_size) + graph_size, drop = FALSE],
#'   NA
#' )
#'
#' test_graph_fast_vms(p, .025, compact_weights)
test_graph_fast <- function(graph,
                            p,
                            alpha = .05,
                            groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            corr = NULL,
                            intersections = generate_weights(graph),
                            graph_size = length(graph$hypotheses),
                            gw_size = 2^graph_size - 1,
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
    )[methods::formalArgs(p_adjust_fun)]

    p_adj[inter_index, group_index] <- do.call(p_adjust_fun, p_adjust_args)
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_inter[p_adj_inter == Inf] <- NA
  p_adj_global <- apply(
    p_adj_inter * inter_h_vecs[, unlist(groups), drop = FALSE],
    2,
    max,
    na.rm = TRUE
  )
  test_global <- p_adj_global <= alpha

  test_global
}

#' @rdname testing-fast
#' @export
test_graph_fast_v <- function(p,
                              alpha,
                              intersections) {
  graph_size <- ncol(intersections)
  inter_h <- !is.na(intersections) # extract h-matrix
  intersections[is.na(intersections)] <- 0 # replace missing weights with 0

  # Calculate test results -----------------------------------------------------
  rej_hyps <- t(p <= alpha * t(intersections))

  colSums(inter_h * do.call(pmax, as.data.frame(rej_hyps))) ==
    2^(graph_size - 1)
}

#' @rdname testing-fast
#' @export
test_graph_fast_vms <- function(p,
                                alpha,
                                intersections) {
  graph_size <- ncol(intersections)
  inter_h <- !is.na(intersections) # extract h-matrix
  intersections[is.na(intersections)] <- 0 # replace missing weights with 0

  # Calculate test results -----------------------------------------------------
  rej_hyps <- t(p <= alpha * t(intersections))

  # could also be
  # ...xStats::colCounts(...      matrixStats::rowAnys(rej_hyps)      ==
  #   slightly slower but less memory allocated
  matrixStats::colSums2(inter_h * matrixStats::rowMaxs(rej_hyps + 0)) ==
    2^(graph_size - 1)
}

#' @rdname testing-fast
#' @export
test_graph_fast_para <- function(graph,
                                 p,
                                 alpha = .05,
                                 groups = list(seq_along(graph$hypotheses)),
                                 test_types = c("bonferroni"),
                                 corr = NULL,
                                 intersections = generate_weights(graph),
                                 inter_list = add_critical(
                                   intersections, corr,
                                   alpha, groups
                                 ),
                                 graph_size = length(graph$hypotheses),
                                 gw_size = 2^graph_size - 1,
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
        w <- inter_row[seq_len(group_size) + group_size]
        c <- inter_row[2 * group_size + 1]

        ifelse(
          p[group] == 0 & w == 0,
          NA,
          p[group] <= c * w * alpha
        )
      }
    )
  }

  # results come out transposed from how inputs were...because apply??
  rej_inter <- colSums(do.call(rbind, test_res)) > 0

  rej_local <- colSums(intersections[, unlist(groups)] * rej_inter)

  rej_local == 2^(graph_size - 1)
}

#' @rdname testing-fast
#' @export
test_graph_fast_simes <- function(graph,
                                  p,
                                  alpha = .05,
                                  groups = list(seq_along(graph$hypotheses)),
                                  test_types = c("bonferroni"),
                                  corr = NULL,
                                  intersections = generate_weights(graph),
                                  graph_size = length(graph$hypotheses),
                                  gw_size = 2^graph_size - 1,
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

    p_adj[inter_index, group_index] <-
      p_adjust_simes_cpp(p[group_in_inter], weights[group_in_inter])
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_inter[p_adj_inter == Inf] <- NA
  p_adj_global <- apply(
    p_adj_inter * inter_h_vecs[, unlist(groups), drop = FALSE],
    2,
    max,
    na.rm = TRUE
  )
  test_global <- p_adj_global <= alpha

  test_global
}

#' @rdname testing-fast
#' @export
tg_fast_simes_ord_cpp <- function(graph,
                                  p,
                                  alpha = .05,
                                  groups = list(seq_along(graph$hypotheses)),
                                  test_types = c("bonferroni"),
                                  corr = NULL,
                                  intersections = generate_weights(graph),
                                  graph_size = length(graph$hypotheses),
                                  gw_size = 2^graph_size - 1,
                                  num_groups = length(groups)) {
  # Parse generated weights ----------------------------------------------------
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

    p_adj[inter_index, group_index] <-
      p_adjust_simes_ord_simple_cpp(weights[group_in_inter], p[group_in_inter])
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_inter[p_adj_inter == Inf] <- NA
  p_adj_global <- apply(
    p_adj_inter * inter_h_vecs[, unlist(groups)],
    2,
    max,
    na.rm = TRUE
  )
  test_global <- p_adj_global <= alpha

  test_global
}

#' @rdname testing-fast
#' @export
tg_fast_simes_ord_r <- function(graph,
                                p,
                                alpha = .05,
                                groups = list(seq_along(graph$hypotheses)),
                                test_types = c("bonferroni"),
                                corr = NULL,
                                intersections = generate_weights(graph),
                                graph_size = length(graph$hypotheses),
                                gw_size = 2^graph_size - 1,
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

    p_adj[inter_index, group_index] <-
      p_adjust_simes_ordered(p[group_in_inter], weights[group_in_inter])
  }

  # Adjusted p-values at higher levels -----------------------------------------
  p_adj_inter <- do.call(pmin, as.data.frame(p_adj))
  p_adj_inter[p_adj_inter == Inf] <- NA
  p_adj_global <- apply(
    p_adj_inter * inter_h_vecs[, unlist(groups), drop = FALSE],
    2,
    max,
    na.rm = TRUE
  )
  test_global <- p_adj_global <= alpha

  test_global
}
