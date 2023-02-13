# Solve for x to get critical value c
# w cannot have 0s?
c_function <- function(x, w, cr, alpha) {
  I <- which(w > 0)
  z <- stats::qnorm(x * w[I] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE),
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = cr[I, I]
    )
  )

  y - alpha * sum(w)
}

# Function to find the separate critical value c's for all subsets
# in an intersection hypothesis
# w is a vector of weights, possibly including 0s
# cr is a correlation matrix
# alpha is overall alpha
# Everything coming in here must be for a single parametric group, with no
# missing values in cr
# solve_c(w[group], cr[group, group], alpha) ~==~
# separate_c_function(w, cr, alpha)[[group_index]]
solve_c <- function(w, cr, alpha) {
  n_hyps <- seq_along(w)
  c <- ifelse(
    length(n_hyps) == 1 || sum(w) == 0,
    1,
    stats::uniroot( # This seems to use a bit of randomness, making the results change by .001
      c_function,
      lower = 0.9,
      upper = 1 / min(w[w > 0]),
      w = w,
      cr = cr,
      alpha = alpha
    )$root
  )

  c
}

# Input must be appropriate vectors for a *single* Bonferroni test group
bonferroni <- function(p_values, weights, alpha, detailed = TRUE) {
  if (detailed) {
    res <- data.frame(
      p = p_values,
      "<=" = "<=",
      c = "",
      "*" = "",
      w = weights,
      "*" = "*",
      alpha = alpha,
      test = p_values <= weights * alpha,
      check.names = FALSE
    )
  } else {
    res <- p_values <= weights * alpha
  }

  res
}

# Input must be appropriate vectors for a *single* parametric test group
# Calculates the critical value for each parametric test group distinctly
parametric <- function(p_values, weights, alpha, corr, detailed = TRUE) {
  c <- solve_c(weights, corr, alpha)

  if (detailed) {
    res <- data.frame(
      p = p_values,
      "<=" = "<=",
      c = c,
      "*" = "*",
      w = weights,
      "*" = "*",
      alpha = alpha,
      test = p_values <= c * weights * alpha,
      check.names = FALSE
    )
  } else {
    res <- p_values <= c * weights * alpha
  }

  res
}

# Input must be appropriate vectors for a *single* Simes test group
simes <- function(p_values, weights, alpha, detailed = TRUE) {
  vec_res <- vector(length = length(weights))
  w_sum <- vector(length = length(weights))

  for (i in seq_along(weights)) {
    w_sum[[i]] <- sum(weights[p_values <= p_values[[i]]])
    vec_res[[i]] <- p_values[[i]] <= alpha * w_sum[[i]]
  }

  if (detailed) {
    res <- data.frame(
      p = p_values,
      "<=" = "<=",
      c = "",
      "*" = "",
      w = w_sum,
      "*" = "*",
      alpha = alpha,
      test = vec_res,
      check.names = FALSE
    )

    rownames(res) <- names(weights)
  } else {
    res <- vec_res

    names(res) <- names(weights)
  }

  res
}
