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
#' @examples
#' w <- c(.5, .5, 0, 0)
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
