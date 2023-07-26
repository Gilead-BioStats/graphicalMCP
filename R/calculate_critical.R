#' Calculate updated hypothesis weights for the closure of a graph
#'
#' The weights created by [graph_generate_weights()] work immediately for
#' Bonferroni testing, but parametric and Simes testing require additional
#' calculations. The `calculate_critical_*()` functions apply parametric or
#' Simes weight increases to get updated weights for testing. They also subset
#' the weights columns by the appropriate groups
#'
#' @param weighting_strategy For parametric, a compact representation of
#'   [graph_generate_weights()] output, where missing hypotheses get a missing
#'   value for weights, and h-vectors are dropped. For Simes, just the weights
#'   from [graph_generate_weights()] output
#' @param corr A numeric matrix of correlations between hypotheses' test
#'   statistics
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param p A numeric vector of p-values
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param hypotheses A numeric vector of hypothesis weights
#' @param x The root to solve for with [stats::uniroot()]
#'
#' @return Outputs:
#' * For `calculate_critical_*()`, a matrix with the same shape as
#'   `weighting_strategy`, where the weights have been adjusted according to the
#'   specified adjustment method
#' * For `c_value_function()`, the critical value for the given group
#'
#' @rdname critical-vals
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
#' graphicalMCP:::calculate_critical_parametric(gw, diag(6), .05, list(1:3))
#' graphicalMCP:::calculate_critical_simes(gw_0, p, list(4:6))
calculate_critical_parametric <- function(weighting_strategy,
                                          corr,
                                          alpha,
                                          groups) {
  matrix_intersections <- !is.na(weighting_strategy)

  c_values <- matrix(
    nrow = nrow(weighting_strategy),
    ncol = ncol(weighting_strategy),
    dimnames = dimnames(weighting_strategy)
  )

  for (group in groups) {
    for (row in seq_len(nrow(weighting_strategy))) {
      group_by_intersection <-
        group[as.logical(matrix_intersections[row, , drop = TRUE][group])]

      group_c_value <- solve_c_parametric(
        weighting_strategy[row, group_by_intersection, drop = TRUE],
        corr[group_by_intersection, group_by_intersection, drop = FALSE],
        alpha
      )

      c_values[row, group] <-
        group_c_value * matrix_intersections[row, group, drop = TRUE]
    }
  }

  critical_values <- c_values * weighting_strategy

  critical_values[, unlist(groups), drop = FALSE]
}

#' @rdname critical-vals
calculate_critical_simes <- function(weighting_strategy, p, groups) {
  ordered_p <- order(p)

  weighting_strategy <- weighting_strategy[, ordered_p, drop = FALSE]

  group_critical_values <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    group_critical_values[[i]] <- matrixStats::rowCumsums(
      weighting_strategy[, ordered_p %in% groups[[i]], drop = FALSE],
      useNames = TRUE
    )
  }

  do.call(cbind, group_critical_values)
}

#' @rdname critical-vals
c_value_function <- function(x, hypotheses, corr, alpha) {
  hyps_nonzero <- which(hypotheses > 0)
  z <- stats::qnorm(x * hypotheses[hyps_nonzero] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = corr[hyps_nonzero, hyps_nonzero, drop = FALSE],
      algorithm = mvtnorm::GenzBretz(maxpts = 25000, abseps = 1e-6, releps = 0)
    )[[1]]
  )

  y - alpha * sum(hypotheses)
}

#' @rdname critical-vals
solve_c_parametric <- function(hypotheses, corr, alpha) {
  num_hyps <- seq_along(hypotheses)
  c_value <- ifelse(
    length(num_hyps) == 1 || sum(hypotheses) == 0,
    1,
    stats::uniroot(
      c_value_function,
      lower = 0, # Why is this not -Inf? Ohhhh because c_value >= 1
      # upper > 40 errors when w[i] ~= 1 && w[j] = epsilon
      # upper = 2 errors when w = c(.5, .5) && all(corr == 1)
      # furthermore, even under perfect correlation & with balanced weights, the
      # c_function_to_solve does not exceed `length(hypotheses)`
      upper = length(hypotheses) + 1,
      hypotheses = hypotheses,
      corr = corr,
      alpha = alpha
    )$root
  )

  # Occasionally has floating point differences
  round(c_value, 10)
}
