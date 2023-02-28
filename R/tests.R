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
      lower = 0.9, # Why is this not -Inf?
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
#' @rdname test-critical
#' @examples
test_bonferroni <- function(p_values, weights, alpha, verbose = TRUE) {
  if (verbose) {
    res <- data.frame(
      intersection = NA,
      hypothesis = NA,
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
  } else {
    res <- p_values <= weights * alpha
  }

  res
}

#' @rdname test-critical
test_parametric <- function(p_values, weights, alpha, corr, verbose = TRUE) {
  c <- solve_c(weights, corr, alpha)

  if (verbose) {
    res <- data.frame(
      intersection = NA,
      hypothesis = NA,
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
  } else {
    res <- p_values <= c * weights * alpha
  }

  res
}

#' @rdname test-critical
test_simes <- function(p_values, weights, alpha, verbose = TRUE) {
  vec_res <- vector(length = length(weights))
  w_sum <- vector(length = length(weights))

  for (i in seq_along(weights)) {
    w_sum[[i]] <- sum(weights[p_values <= p_values[[i]]])
    vec_res[[i]] <- p_values[[i]] <= alpha * w_sum[[i]]
  }

  if (verbose) {
    res <- data.frame(
      intersection = NA,
      hypothesis = NA,
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

    rownames(res) <- names(weights)
  } else {
    res <- vec_res

    names(res) <- names(weights)
  }

  res
}
