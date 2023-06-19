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
#' @template references
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
    Intersection = rep(NA, length(p)),
    Hypothesis = names(weights),
    Test = rep("bonferroni", length(p)),
    p = p,
    "<=" = rep("<=", length(p)),
    c = rep("", length(p)),
    "*" = rep("", length(p)),
    Critical = weights,
    "*" = rep("*", length(p)),
    Alpha = rep(alpha, length(p)),
    Reject = ifelse(
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
    Intersection = NA,
    Hypothesis = names(weights),
    Test = "parametric",
    p = p,
    "<=" = "<=",
    c = c,
    "*" = "*",
    Critical = weights,
    "*" = "*",
    Alpha = alpha,
    Reject = ifelse(
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
    Intersection = NA,
    Hypothesis = names(weights),
    Test = "simes",
    p = p,
    "<=" = "<=",
    c = "",
    "*" = "",
    Critical = w_sum,
    "*" = "*",
    Alpha = alpha,
    Reject = ifelse(
      p == 0 & w_sum == 0,
      NA,
      vec_res
    ),
    check.names = FALSE
  )
}
