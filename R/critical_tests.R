# Solve for x to get critical value c
c_function <- function(x, w, corr, alpha) {
  w_nonzero <- which(w > 0)
  z <- stats::qnorm(x * w[w_nonzero] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = corr[w_nonzero, w_nonzero]
    )[[1]]
  )

  y - alpha * sum(w)
}

# Function to find the separate critical value c's for all subsets
# in an intersection hypothesis
# w is a vector of weights, possibly including 0s
# corr is a correlation matrix
# alpha is overall alpha
# Everything coming in here must be for a single parametric group, with no
# missing values in corr
# solve_c(w[group], corr[group, group], alpha)
solve_c <- function(w, corr, alpha) {
  n_hyps <- seq_along(w)
  c <- ifelse(
    length(n_hyps) == 1 || sum(w) == 0,
    1,
    stats::uniroot( # This seems to use a bit of randomness, making the results change by .001
      c_function,
      lower = 0.9, # Why is this not -Inf? Ohhhh because c >= 1
      upper = 1 / min(w[w > 0]),
      w = w,
      corr = corr,
      alpha = alpha
    )$root
  )

  c
}

#' Test hypotheses with the `p <= (c * ) w * a` method
#'
#' @param p_values A numeric vector of p-values
#' @param weights A numeric vector of hypothesis weights
#' @param alpha A numeric scalar specifying the level to test weighted p-values
#'   against
#' @param verbose A logical scalar specifying how detailed the results should be
#'
#' @return A vector of test results if `verbose` is FALSE, otherwise a data
#'   frame with detailed test values and results
#' @export
#'
#' @rdname calc-test_vals
#' @examples
bonferroni_test_vals <- function(p_values, weights, alpha, corr = NULL) {
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
    res = p_values <= weights * alpha,
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
    res = p_values <= c * weights * alpha,
    check.names = FALSE
  )
}

#' @rdname calc-test_vals
simes_test_vals <- function(p_values, weights, alpha, corr = NULL) {
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
    res = vec_res,
    check.names = FALSE
  )
}
