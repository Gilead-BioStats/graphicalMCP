#' Calculate adjusted p-values
#'
#' @param p A named numeric vector of p-values
#' @param weights A named numeric vector of hypothesis weights
#' @param corr (Optional) A numeric matrix of correlations between hypotheses'
#'   test statistics
#'
#' @return A single adjusted p-value for the given group
#'
#' @rdname p_adjust
#'
#' @keywords internal
#'
#' @template references
#'
#' @examples
#' set.seed(22723)
#'
#' w <- c("H1" = .75, "H2" = .25, "H3" = 0)
#' p <- c("H1" = .019, "H2" = .025, "H3" = .05)
#'
#' graphicalMCP:::p_adjust_bonferroni(p, w)
#' graphicalMCP:::p_adjust_simes(p, w)
#'
#' corr1 <- diag(3)
#' corr2 <- corr1
#' corr2[lower.tri(corr2)] <- corr2[upper.tri(corr2)] <- runif(3, -1, 1)
#'
#' # No correlation
#' graphicalMCP:::p_adjust_parametric(p, w, corr1)
#'
#' # Uniform random pairwise correlations
#' graphicalMCP:::p_adjust_parametric(p, w, corr2)
p_adjust_bonferroni <- function(p, weights) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  # We need na.rm = TRUE to handle the 0 / 0 case. This may be too blunt a way
  # to handle it, but I suspect it's the fastest. Another option is to reduce p
  # and weights by keeping only indices where `!(p == 0 & weights == 0)`.
  # Considering that p-values are validated in the test function, this should be
  # safe
  min(p / weights, na.rm = TRUE)
}

#' @rdname p_adjust
p_adjust_parametric <- function(p, weights, corr = NULL) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  w_nonzero <- weights > 0
  q <- min(p[w_nonzero] / weights[w_nonzero])
  q <- q * weights[w_nonzero]
  z <- stats::qnorm(q, lower.tail = FALSE)
  prob_lt_z <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = corr[w_nonzero, w_nonzero, drop = FALSE],
      algorithm = mvtnorm::GenzBretz(maxpts = 25000, abseps = 1e-6, releps = 0)
    )[[1]]
  )

  # Occasionally off by floating point differences
  round(1 / sum(weights) * prob_lt_z, 10)
}

#' @rdname p_adjust
p_adjust_simes <- function(p, weights) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  adjusted_p <- Inf
  for (i in seq_along(weights)) {
    # See Bonferroni for na.rm reasoning
    adjusted_p <- min(
      adjusted_p,
      p[[i]] / sum(weights[p <= p[[i]]]),
      na.rm = TRUE
    )
  }
  adjusted_p
}
