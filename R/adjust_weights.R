#' Calculate adjusted hypothesis weights
#'
#' @description
#' An intersection hypothesis can be rejected if its p-values are less than or
#' equal to their adjusted significance levels, which are their adjusted
#' hypothesis weights times \eqn{\alpha}. For Bonferroni tests, their adjusted
#' hypothesis weights are their hypothesis weights of the intersection
#' hypothesis. Additional adjustment is needed for parametric and Simes tests:
#' * Parametric tests \insertCite{xi-2017-unified}{graphicalMCP} for
#'   `graphicalMCP:::adjust_weights_parametric()`,
#'     - Note that one-sided tests are required for parametric tests.
#' * Simes tests \insertCite{lu-2016-graphical}{graphicalMCP} for
#'   `graphicalMCP:::adjust_weights_simes()`.
#'
#' @param matrix_weights A matrix of hypothesis weights of all intersection
#'   hypotheses. This can be obtained as the second half of columns from the
#'   output of [graph_generate_weights()].
#' @param matrix_intersections A matrix of hypothesis indicators of all
#'   intersection hypotheses. This can be obtained as the first half of columns
#'   from the output of [graph_generate_weights()].
#' @inheritParams graph_test_closure
#' @inheritParams graph_create
#' @param x The root to solve for with [stats::uniroot()].
#'
#' @return
#' * `adjust_weights_parametric()` returns a matrix with the same dimensions
#'   as `weighting_strategy`, whose hypothesis weights have been adjusted
#'   according to parametric tests.
#' * `adjust_weights_simes()` returns a matrix with the same dimensions
#'   as `weighting_strategy`, whose hypothesis weights have been adjusted
#'   according to Simes tests.
#' * `c_value_function()` returns the difference between \eqn{\alpha} and the
#'   Type I error of the parametric test with the c value of `x`, adjusted for
#'   the correlation between test statistics using parametric tests based on
#'   equation (6) of \insertCite{xi-2017-unified}{graphicalMCP}.
#' * `solve_c_parametric()` returns the c value adjusted for the correlation
#'   between test statistics using parametric tests based on equation (6) of
#'   \insertCite{xi-2017-unified}{graphicalMCP}.
#'
#' @family graphical tests
#'
#' @rdname adjusted_weights
#'
#' @importFrom Rdpack reprompt
#'
#' @keywords internal
#'
#' @references
#'  * \insertRef{lu-2016-graphical}{graphicalMCP}
#'  * \insertRef{xi-2017-unified}{graphicalMCP}
#'
#' @examples
#' set.seed(1234)
#' alpha <- 0.025
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' num_hyps <- length(p)
#' g <- bonferroni_holm(rep(1 / 4, 4))
#' weighting_strategy <- graph_generate_weights(g)
#' matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
#' matrix_weights <- weighting_strategy[, -seq_len(num_hyps)]
#'
#' set.seed(1234)
#' graphicalMCP:::adjust_weights_parametric(
#'   matrix_weights = matrix_weights,
#'   matrix_intersections = matrix_intersections,
#'   test_corr = diag(4),
#'   alpha = alpha,
#'   test_groups = list(1:4)
#' )
#'
#' graphicalMCP:::adjust_weights_simes(
#'   matrix_weights = matrix_weights,
#'   p = p,
#'   test_groups = list(1:4)
#' )
#'
#' graphicalMCP:::solve_c_parametric(
#'   hypotheses = matrix_weights[1, ],
#'   test_corr = diag(4),
#'   alpha = alpha
#' )
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

#' @rdname adjusted_weights
adjust_weights_simes <- function(matrix_weights, p, test_groups) {
  ordered_p <- order(p)

  matrix_weights <- matrix_weights[, ordered_p, drop = FALSE]

  group_adjusted_weights <- vector("list", length(test_groups))
  for (i in seq_along(test_groups)) {
    group_adjusted_weights[[i]] <- matrixStats::rowCumsums(
      matrix_weights[, ordered_p %in% test_groups[[i]], drop = FALSE],
      useNames = TRUE
    )
  }

  do.call(cbind, group_adjusted_weights)
}

#' @rdname adjusted_weights
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

#' @rdname adjusted_weights
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
