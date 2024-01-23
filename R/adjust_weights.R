#' Calculate updated hypothesis weights for the closure of a graph
#'
#' The weights created by [graph_generate_weights()] work immediately for
#' Bonferroni testing, but parametric and Simes testing require additional
#' calculations. The `adjust_weights_*()` functions apply parametric or Simes
#' weight increases to get updated weights for testing. They also subset the
#' weights columns by the appropriate groups
#'
#' @param matrix_weights The second half of columns from
#'   [graph_generate_weights()] output, indicating the weights of each
#'   intersection
#' @param matrix_intersections The first half of columns from
#'   [graph_generate_weights()] output, indicating which hypotheses are
#'   contained in each intersection
#' @param test_corr A numeric matrix of correlations between hypotheses' test
#'   statistics
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param test_groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param p A numeric vector of p-values
#' @param hypotheses A numeric vector of hypothesis weights
#' @param x The root to solve for with [stats::uniroot()]
#'
#' @return Outputs:
#' * For `adjust_weights_*()`, a matrix with the same shape as
#'   `weighting_strategy`, where the weights have been adjusted according to the
#'   specified adjustment method
#' * For `c_value_function()`, the \eqn{c_{J_h}} value for the given group,
#'   according to Formula 6 of Xi et al. (2017).
#'
#' @rdname adjusted-weights
#'
#' @keywords internal
#'
#' @template references
#'
#' @examples
#' p <- 1:6 / 200
#'
#' g <- bonferroni_holm(6)
#' gw_large <- graph_generate_weights(g)
#'
#' gw_0 <- gw_large[, 7:12]
#' gw <- ifelse(gw_large[, 1:6], gw_0, NA)
#'
#' graphicalMCP:::adjust_weights_parametric(
#'   gw_0,
#'   gw_large[, 1:6],
#'   diag(6),
#'   .05,
#'   list(1:3)
#' )
#'
#' graphicalMCP:::adjust_weights_simes(gw_0, p, list(4:6))
adjust_weights_parametric <- function(matrix_weights,
                                      matrix_intersections,
                                      test_corr,
                                      alpha,
                                      test_groups) {
  c_values <- matrix(
    nrow = nrow(matrix_weights),
    ncol = ncol(matrix_weights),
    dimnames = dimnames(matrix_weights)
  )

  for (group in test_groups) {
    for (row in seq_len(nrow(matrix_weights))) {
      group_by_intersection <-
        group[as.logical(matrix_intersections[row, , drop = TRUE][group])]

      group_c_value <- solve_c_parametric(
        matrix_weights[row, group_by_intersection, drop = TRUE],
        test_corr[group_by_intersection, group_by_intersection, drop = FALSE],
        alpha
      )

      c_values[row, group] <-
        group_c_value * matrix_intersections[row, group, drop = TRUE]
    }
  }

  adjusted_weights <- c_values * matrix_weights

  adjusted_weights[, unlist(test_groups), drop = FALSE]
}

#' @rdname adjusted-weights
adjust_weights_simes <- function(matrix_weights, p, groups) {
  ordered_p <- order(p)

  matrix_weights <- matrix_weights[, ordered_p, drop = FALSE]

  group_adjusted_weights <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    group_adjusted_weights[[i]] <- matrixStats::rowCumsums(
      matrix_weights[, ordered_p %in% groups[[i]], drop = FALSE],
      useNames = TRUE
    )
  }

  do.call(cbind, group_adjusted_weights)
}

#' @rdname adjusted-weights
c_value_function <- function(x, hypotheses, test_corr, alpha) {
  hyps_nonzero <- which(hypotheses > 0)
  z <- stats::qnorm(x * hypotheses[hyps_nonzero] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = test_corr[hyps_nonzero, hyps_nonzero, drop = FALSE],
      algorithm = mvtnorm::GenzBretz(maxpts = 25000, abseps = 1e-6, releps = 0)
    )[[1]]
  )

  y - alpha * sum(hypotheses)
}

#' @rdname adjusted-weights
solve_c_parametric <- function(hypotheses, test_corr, alpha) {
  num_hyps <- seq_along(hypotheses)
  c_value <- ifelse(
    length(num_hyps) == 1 || sum(hypotheses) == 0,
    1,
    stats::uniroot(
      c_value_function,
      lower = 0, # Why is this not -Inf? Ohhhh because c_value >= 1
      # upper > 40 errors when w[i] ~= 1 && w[j] = epsilon
      # upper = 2 errors when w = c(.5, .5) && all(test_corr == 1)
      # furthermore, even under perfect correlation & with balanced weights, the
      # c_function_to_solve does not exceed `length(hypotheses)`
      upper = length(hypotheses) + 1,
      hypotheses = hypotheses,
      test_corr = test_corr,
      alpha = alpha
    )$root
  )

  # Occasionally has floating point differences
  round(c_value, 10)
}
