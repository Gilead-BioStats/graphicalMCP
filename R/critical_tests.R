#' Test hypotheses with the `p <= (c * ) w * a` method
#'
#' @param p_values A numeric vector of p-values
#' @param weights A numeric vector of hypothesis weights
#' @param alpha A numeric scalar specifying the level to test weighted p-values
#'   against
#' @param corr (Optional) A numeric matrix indicating the correlation between
#'   the test statistics which generated the p-values. Must be a square matrix
#'   with side length equal to the length of `p` and `weights`
#'
#' @return A data frame with columns specifying the values used to calculate
#'   each hypothesis test
#'
#' @rdname calc-test_vals
bonferroni_test_vals <- function(p_values, weights, alpha) {
  data.frame(
    intersection = NA,
    hypothesis = names(weights),
    test = "bonferroni",
    p = p_values,
    "<=" = "<=",
    c = "",
    "*" = "",
    w = weights,
    "*" = "*",
    alpha = alpha,
    res = ifelse(
      p_values == 0 & weights == 0,
      NA,
      p_values <= weights * alpha
    ),
    check.names = FALSE
  )
}

#' @rdname calc-test_vals
parametric_test_vals <- function(p_values, weights, alpha, corr = NULL) {
  c <- solve_c(weights, corr, alpha)

  data.frame(
    intersection = NA,
    hypothesis = names(weights),
    test = "parametric",
    p = p_values,
    "<=" = "<=",
    c = c,
    "*" = "*",
    w = weights,
    "*" = "*",
    alpha = alpha,
    res = ifelse(
      p_values == 0 & weights == 0,
      NA,
      p_values <= c * weights * alpha
    ),
    check.names = FALSE
  )
}

#' @rdname calc-test_vals
parametric_test_fast <- function(p_values, critical, weights, alpha) {
  ifelse(
    p_values == 0 & weights == 0,
    NA,
    p_values <= critical * weights * alpha
  )
}

#' @rdname calc-test_vals
simes_test_vals <- function(p_values, weights, alpha) {
  vec_res <- vector(length = length(weights))
  w_sum <- vector("numeric", length = length(weights))

  for (i in seq_along(weights)) {
    w_sum[[i]] <- sum(weights[p_values <= p_values[[i]]])
    vec_res[[i]] <- p_values[[i]] <= alpha * w_sum[[i]]
  }

  data.frame(
    intersection = NA,
    hypothesis = names(weights),
    test = "simes",
    p = p_values,
    "<=" = "<=",
    c = "",
    "*" = "",
    w = w_sum,
    "*" = "*",
    alpha = alpha,
    res = ifelse(
      p_values == 0 & w_sum == 0,
      NA,
      vec_res
    ),
    check.names = FALSE
  )
}
