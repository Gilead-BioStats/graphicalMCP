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
#' Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
#' approach to sequentially rejective multiple test procedures. Statistics in
#' Medicine, 28(4), 586–604. https://doi.org/10.1002/sim.3495
#'
#' Bretz, F., Maurer, W., and Hommel, G. (2011). Test and power considerations
#' for multiple endpoint analyses using sequentially rejective graphical
#' procedures. Statistics in Medicine, 30(13), 1489–1501.
#' https://doi.org/10.1002/sim.3988
#'
#' Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and Rohmeyer,
#' K. (2011). Graphical approaches for multiple comparison procedures using
#' weighted Bonferroni, Simes, or parametric tests. Biometrical Journal, 53(6),
#' 894–913. https://doi.org/10.1002/bimj.201000239
#'
#' Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
#' Simes tests. Statistics in Medicine, 35(22), 4041–4055.
#' https://doi.org/10.1002/sim.6985
#'
#' Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework for
#' weighted parametric multiple test procedures. Biometrical Journal, 59(5),
#' 918–931. https://doi.org/10.1002/bimj.201600233
#'
#' Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted tests,
#' with application to the Hochberg procedure. Statistics in Medicine, 38(27),
#' 5268–5282. https://doi.org/10.1002/sim.8375
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
      corr = corr[w_nonzero, w_nonzero, drop = FALSE]
    )[[1]]
  )

  1 / sum(weights) * prob_lt_z
}

#' @rdname p_adjust
p_adjust_simes <- function(p, weights) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  adj_p <- Inf
  for (i in seq_along(weights)) {
    # See Bonferroni for na.rm reasoning
    adj_p <- min(
      adj_p,
      p[[i]] / sum(weights[p <= p[[i]]]),
      na.rm = TRUE
    )
  }
  adj_p
}
