#' Calculate adjusted p-values
#'
#' @param p A named numeric vector of p-values
#' @param hypotheses A named numeric vector of hypothesis weights
#' @param corr (Optional) A numeric matrix of correlations between hypotheses'
#'   test statistics
#'
#' @return A single adjusted p-value for the given group
#'
#' @rdname adjust_p
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
#' graphicalMCP:::adjust_p_bonferroni(p, w)
#' graphicalMCP:::adjust_p_simes(p, w)
#'
#' corr1 <- diag(3)
#' corr2 <- corr1
#' corr2[lower.tri(corr2)] <- corr2[upper.tri(corr2)] <- runif(3, -1, 1)
#'
#' # No correlation
#' graphicalMCP:::adjust_p_parametric(p, w, corr1)
#'
#' # Uniform random pairwise correlations
#' graphicalMCP:::adjust_p_parametric(p, w, corr2)
adjust_p_bonferroni <- function(p, hypotheses) {
  if (sum(hypotheses) == 0) {
    return(Inf)
  }

  # We need na.rm = TRUE to handle the 0 / 0 case. This may be too blunt a way
  # to handle it, but I suspect it's the fastest. Another option is to reduce p
  # and weights by keeping only indices where `!(p == 0 & weights == 0)`.
  # Considering that p-values are validated in the test function, this should be
  # safe
  round(min(p / hypotheses, na.rm = TRUE), 10)
}

#' @rdname adjust_p
adjust_p_parametric <- function(p, hypotheses, corr = NULL) {
  if (sum(hypotheses) == 0) {
    return(Inf)
  }

  w_nonzero <- hypotheses > 0
  q <- min(p[w_nonzero] / hypotheses[w_nonzero])
  q <- q * hypotheses[w_nonzero]
  z <- stats::qnorm(q, lower.tail = FALSE)
  prob_less_than_z <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = corr[w_nonzero, w_nonzero, drop = FALSE],
      algorithm = mvtnorm::GenzBretz(maxpts = 25000, abseps = 1e-6, releps = 0)
    )[[1]]
  )

  # Occasionally off by floating point differences, so round at some high detail
  # This level of detail should always remove floating point differences
  # appropriately, as they're typically 10^(<=-15). Conversely, this
  round(1 / sum(hypotheses) * prob_less_than_z, 10)
}

#' @rdname adjust_p
adjust_p_simes <- function(p, hypotheses) {
  if (sum(hypotheses) == 0) {
    return(Inf)
  }

  adjusted_p <- Inf
  for (i in seq_along(hypotheses)) {
    # This demonstrates a different and slightly more accurate way of
    # calculating Simes adjusted weights/adjusted p-values compared to the
    # method used in [adjust_weights_simes()]. In this function (and
    # [test_values_simes()]), we add all hypothesis weights for hypotheses with
    # a smaller p-value than hypothesis_j, for all j in J. In the case that two
    # p-values are identical, the corresponding hypotheses will get identical
    # adjusted weights/adjusted p-values. [adjust_weights_simes()], on the other
    # hand, uses an alternate method that's faster: First order hypotheses
    # according to their p-values in ascending order, then take the cumulative
    # sum. In the case that two p-values are identical, they will be sorted
    # sequentially, and the hypothesis that happens to come first will get a
    # smaller, incorrect adjusted weight (larger, incorrect adjusted p-value).
    # The hypothesis that comes second will be correct. [adjust_weights_simes()]
    # is only used in power calculations where it should not be possible to have
    # identical p-values, since they are sampled randomly (unless `all(corr ==
    # 1)`). Furthermore, even when there are incorrect adjusted weights, it
    # cannot affect the hypothesis rejections. See Bonferroni function above for
    # na.rm reasoning
    adjusted_p <- min(
      adjusted_p,
      p[[i]] / sum(hypotheses[p <= p[[i]]]),
      na.rm = TRUE
    )
  }

  round(adjusted_p, 10)
}
