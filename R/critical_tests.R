#' Test hypotheses with the critical values method
#'
#' @param p A numeric vector of p-values
#' @param weights A numeric vector of hypothesis weights
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param corr A numeric matrix of correlations between hypotheses' test
#'   statistics
#'
#' @return A data frame with columns specifying the values used to calculate
#'   each hypothesis test
#'
#' @rdname calc-test_vals
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
#' w <- c(H1 = .5, H2 = .5, H3 = 0, H4 = 0)
#'
#' p <- c(.024, .01, .026, .027)
#'
#' graphicalMCP:::bonferroni_test_vals(p, w, .05)
#' graphicalMCP:::parametric_test_vals(p, w, .05, diag(4))
#' graphicalMCP:::simes_test_vals(p, w, .05)
bonferroni_test_vals <- function(p, weights, alpha) {
  data.frame(
    intersection = NA,
    hypothesis = names(weights),
    test = "bonferroni",
    p = p,
    "<=" = "<=",
    c = "",
    "*" = "",
    w = weights,
    "*" = "*",
    alpha = alpha,
    res = ifelse(
      p == 0 & weights == 0,
      NA,
      p <= weights * alpha
    ),
    check.names = FALSE
  )
}

#' @rdname calc-test_vals
parametric_test_vals <- function(p, weights, alpha, corr) {
  c <- solve_c(weights, corr, alpha)

  data.frame(
    intersection = NA,
    hypothesis = names(weights),
    test = "parametric",
    p = p,
    "<=" = "<=",
    c = c,
    "*" = "*",
    w = weights,
    "*" = "*",
    alpha = alpha,
    res = ifelse(
      p == 0 & weights == 0,
      NA,
      p <= c * weights * alpha
    ),
    check.names = FALSE
  )
}

#' @rdname calc-test_vals
simes_test_vals <- function(p, weights, alpha) {
  vec_res <- vector(length = length(weights))
  w_sum <- vector("numeric", length = length(weights))

  for (i in seq_along(weights)) {
    w_sum[[i]] <- sum(weights[p <= p[[i]]])
    vec_res[[i]] <- p[[i]] <= alpha * w_sum[[i]]
  }

  data.frame(
    intersection = NA,
    hypothesis = names(weights),
    test = "simes",
    p = p,
    "<=" = "<=",
    c = "",
    "*" = "",
    w = w_sum,
    "*" = "*",
    alpha = alpha,
    res = ifelse(
      p == 0 & w_sum == 0,
      NA,
      vec_res
    ),
    check.names = FALSE
  )
}
