#' Calculate adjusted hypothesis weights
#'
#' @description
#' An intersection hypothesis can be rejected if its p-values are less than or
#' equal to their adjusted significance levels, which are their adjusted
#' hypothesis weights times \eqn{\alpha}. For Bonferroni tests, their adjusted
#' hypothesis weights are their hypothesis weights of the intersection
#' hypothesis. Additional adjustment is needed for parametric and Simes tests:
#' * Parametric tests for `graphicalMCP:::adjust_weights_parametric()`,
#'     - Note that one-sided tests are required for parametric tests.
#' * Simes tests for `graphicalMCP:::adjust_weights_simes()`.
#'
#' @param matrix_weights (Optional) A matrix of hypothesis weights of all
#'   intersection hypotheses. This can be obtained as the second half of columns
#'   from the output of [graph_generate_weights()].
#' @param matrix_intersections (Optional) A matrix of hypothesis indicators of
#'   all intersection hypotheses. This can be obtained as the first half of
#'   columns from the output of [graph_generate_weights()].
#' @param x The root to solve for with `stats::uniroot()`.
#' @param alpha (Optional) A numeric value of the overall significance level,
#'   which should be between 0 & 1. The default is 0.025 for one-sided
#'   hypothesis testing problems; another common choice is 0.05 for two-sided
#'   hypothesis testing problems. Note when parametric tests are used, only
#'   one-sided tests are supported.
#' @param p (Optional) A numeric vector of p-values (unadjusted, raw), whose
#'   values should be between 0 & 1. The length should match the length of
#'   `hypotheses`.
#' @param hypotheses (Optional) A numeric vector of hypothesis weights. Must be
#'   a vector of values between 0 & 1 (inclusive). The length should match the
#'   length of `p`. The sum of hypothesis weights should not exceed 1.
#' @param test_corr (Optional) A numeric matrix of correlations between test
#'   statistics, which is needed to perform parametric tests using
#'   `graphicalMCP:::adjust_p_parametric()`. The number of rows and columns of
#'   this correlation matrix should match the length of `p`.
#' @param test_groups (Optional) A list of numeric vectors specifying hypotheses
#'   to test together. Grouping is needed to correctly perform Simes and
#'   parametric tests.
#' @param maxpts (Optional) An integer scalar for the maximum number of function
#'   values, which is needed to perform parametric tests using the
#'   `mvtnorm::GenzBretz` algorithm. The default is 25000.
#' @param abseps (Optional) A numeric scalar for the absolute error tolerance,
#'   which is needed to perform parametric tests using the `mvtnorm::GenzBretz`
#'   algorithm. The default is 1e-6.
#' @param releps (Optional) A numeric scalar for the relative error tolerance
#'   as double, which is needed to perform parametric tests using the
#'   `mvtnorm::GenzBretz` algorithm. The default is 0.
#'
#' @return
#' * `graphicalMCP:::adjust_weights_parametric()` returns a matrix with the same
#'   dimensions as `matrix_weights`, whose hypothesis weights have been
#'   adjusted according to parametric tests.
#' * `graphicalMCP:::adjust_weights_simes()` returns a matrix with the same
#'   dimensions as `matrix_weights`, whose hypothesis weights have been
#'   adjusted according to Simes tests.
#' * `graphicalMCP:::c_value_function()` returns the difference between
#'   \eqn{\alpha} and the Type I error of the parametric test with the $c$ value
#'   of `x`, adjusted for the correlation between test statistics using
#'   parametric tests based on equation (6) of Xi et al. (2017).
#' * `graphicalMCP:::solve_c_parametric()` returns the c value adjusted for the
#'   correlation between test statistics using parametric tests based on
#'   equation (6) of Xi et al. (2017).
#'
#' @seealso
#'   `graphicalMCP:::adjust_p_parametric()` for adjusted p-values using
#'   parametric tests, `graphicalMCP:::adjust_p_simes()` for adjusted p-values
#'   using Simes tests.
#'
#' @rdname adjust_weights
#'
#' @keywords internal
#'
#' @references
#'   Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
#'   Simes tests. \emph{Statistics in Medicine}, 35(22), 4041-4055.
#'
#'   Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
#'   for weighted parametric multiple test procedures.
#'   \emph{Biometrical Journal}, 59(5), 918-931.
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
                                      test_groups,
                                      maxpts = 25000,
                                      abseps = 1e-6,
                                      releps = 0) {
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
        alpha,
        maxpts,
        abseps,
        releps
      )

      c_values[row, group] <-
        group_c_value * matrix_intersections[row, group, drop = TRUE]
    }
  }

  adjusted_weights <- c_values * matrix_weights

  adjusted_weights[, unlist(test_groups), drop = FALSE]
}

#' @rdname adjust_weights
#' @keywords internal
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

#' @rdname adjust_weights
#' @keywords internal
c_value_function <- function(x,
                             hypotheses,
                             test_corr,
                             alpha,
                             maxpts = 25000,
                             abseps = 1e-6,
                             releps = 0) {
  hyps_nonzero <- which(hypotheses > 0)
  z <- stats::qnorm(x * hypotheses[hyps_nonzero] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = test_corr[hyps_nonzero, hyps_nonzero, drop = FALSE],
      algorithm = mvtnorm::GenzBretz(
        maxpts = maxpts,
        abseps = abseps,
        releps = releps
      )
    )[[1]]
  )

  y - alpha * sum(hypotheses)
}

#' @rdname adjust_weights
#' @keywords internal
solve_c_parametric <- function(hypotheses,
                               test_corr,
                               alpha,
                               maxpts = 25000,
                               abseps = 1e-6,
                               releps = 0) {
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
      alpha = alpha,
      maxpts = maxpts,
      abseps = abseps,
      releps = releps
    )$root
  )

  # Occasionally has floating point differences
  round(c_value, 10)
}
