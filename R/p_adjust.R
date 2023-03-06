#' Calculate adjusted p-values
#'
#' @param p_values A named numeric vector of p-values to adjust
#' @param weights A named numeric vector of weights to adjust the p-values by
#' @param corr (Optional) A numeric matrix indicating the correlation between
#'   the test statistics which generated the p-values. For parametric testing,
#'   `corr` must be a square matrix with side length equal to the length of `p`
#'   and `weights`. Ignored for Simes and Bonferroni testing
#'
#' @return A single adjusted p-value for the given group
#'
#' @rdname p_adjust
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
p_adjust_bonferroni <- function(p_values, weights, corr = NULL) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  min(p_values / weights)
}

#' @rdname p_adjust
p_adjust_parametric <- function(p_values, weights, corr = NULL) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  w_nonzero <- weights > 0
  q <- min(p_values[w_nonzero] / weights[w_nonzero])
  q <- q * weights[w_nonzero]
  z <- stats::qnorm(q, lower.tail = FALSE)
  prob_lt_z <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = corr[w_nonzero, w_nonzero, drop = FALSE]
    )[[1]]
  )

  1 / sum(weights) * prob_lt_z
}

#' @rdname p_adjust
p_adjust_simes <- function(p_values, weights, corr = NULL) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  adj_p <- Inf
  for (i in seq_along(weights)) {
    adj_p <- min(
      adj_p,
      p_values[[i]] / sum(weights[p_values <= p_values[[i]]])
    )
  }
  adj_p
}
