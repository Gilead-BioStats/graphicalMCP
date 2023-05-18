#' @rdname critical-vals
c_function <- function(x, weights, corr, alpha) {
  w_nonzero <- which(weights > 0)
  z <- stats::qnorm(x * weights[w_nonzero] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = corr[w_nonzero, w_nonzero]
    )[[1]]
  )

  y - alpha * sum(weights)
}

#' @rdname critical-vals
solve_c <- function(weights, corr, alpha) {
  n_hyps <- seq_along(weights)
  c <- ifelse(
    length(n_hyps) == 1 || sum(weights) == 0,
    1,
    stats::uniroot(
      c_function,
      lower = 0, # Why is this not -Inf? Ohhhh because c >= 1
      # upper > 40 errors when w[i] ~= 1 && w[j] = epsilon
      # upper = 2 errors when w = c(.5, .5) && all(corr == 1)
      # furthermore, even under perfect correlation & with balanced weights, the
      # c_function does not exceed `length(w)`
      upper = length(weights) + 1,
      weights = weights,
      corr = corr,
      alpha = alpha
    )$root
  )

  c
}

#' Calculate updated hypothesis weights for the closure of a graph
#'
#' The weights created by [generate_weights()] work immediately for Bonferroni
#' testing, but parametric and Simes testing require additional calculations.
#' The `calculate_critical_*()` functions apply parametric or Simes weight
#' increases to get updated weights for testing. They also subset the weights
#' columns by the appropriate groups
#'
#' @param intersections For parametric, a compact representation of
#'   [generate_weights()] output, where missing hypotheses get a missing value
#'   for weights, and h-vectors are dropped. For Simes, just the weights from
#'   [generate_weights()] output
#' @param corr A numeric matrix of correlations between hypotheses' test
#'   statistics
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param p A numeric vector of p-values
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param weights A numeric vector of hypothesis weights
#' @param x The root to solve for with [stats::uniroot()]
#'
#' @return Outputs:
#' * For `calculate_critical_*()`, a matrix with the same shape as
#'   `intersections`, where the weights have been adjusted according to the
#'   specified adjustment method
#' * For `c_function()`, the critical value for the given group
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
#' gw_large <- generate_weights(g)
#'
#' gw_0 <- gw_large[, 7:12]
#' gw <- ifelse(gw_large[, 1:6], gw_0, NA)
#'
#' graphicalMCP:::calculate_critical_parametric(gw, diag(6), .05, list(1:3))
#' graphicalMCP:::calculate_critical_simes(gw_0, p, list(4:6))
calculate_critical_parametric <- function(intersections, corr, alpha, groups) {
  h_vecs <- !is.na(intersections)

  c_mat <- intersections # placeholder

  for (group in groups) {
    for (row in seq_len(nrow(intersections))) {
      group_in_inter <- group[as.logical(h_vecs[row, ][group])]

      c_val <- solve_c(
        intersections[row, group_in_inter],
        corr[group_in_inter, group_in_inter],
        alpha
      )

      c_mat[row, group] <- c_val * h_vecs[row, group]
    }
  }

  (c_mat * intersections)[, unlist(groups), drop = FALSE]
}

#' @rdname critical-vals
calculate_critical_simes <- function(intersections, p, groups) {
  p_ord <- order(p)

  # re-order by p
  intersections <- intersections[, p_ord]

  list_w_new <- vector("list", length(groups))

  for (i in seq_along(groups)) {
    list_w_new[[i]] <- matrixStats::rowCumsums(
      intersections[, p_ord %in% groups[[i]], drop = FALSE],
      useNames = TRUE
    )
  }

  do.call(cbind, list_w_new)
}
