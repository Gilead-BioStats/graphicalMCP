myfct <- function(x, a, w, sig) {
  1 -
    a -
    mvtnorm::pmvnorm(lower = -Inf,
                     upper = qnorm(1 - x * w * a),
                     sigma = sig)
}

# Find blocks of non-missing values in a correlation matrix
# Behaves a little weird if a block has a mix of missing and non-missing
subset_function <- function(x) {
  subset <- !is.na(x)
  id <- !duplicated(subset)
  subsets <- vector("list", sum(id))
  for (i in 1:sum(id)) {
    if (!is.null(nrow(x))) {
      subsets[[i]] <- (1:nrow(x))[which(subset[which(id)[i],])]
    } else {
      subsets[[i]] <- 1
    }
  }

  subsets
}

# Solve for x to get critical value c
# w cannot have 0s?
c_function <- function(x, w, cr, alpha) {
  I <- which(w > 0)
  z <- qnorm(x * w[I] * alpha, lower.tail = FALSE)
  y <- ifelse(
    length(z) == 1,
    pnorm(z, lower.tail = FALSE),
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
    uniroot( # This seems to use a bit of randomness, making the results change by .001
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

separate_c_function <- function(w, cr, alpha) {

  I <- which(w > 0) # Is this good enough? Do we need to know about weight 0 H's that are present?
  subw <- w[I]
  subcr <- cr[I, I]
  subsets <- subset_function(subcr)
  nsubsets <- length(subsets)
  wP <- w
  c <- rep(0, nsubsets)
  for (i in 1:nsubsets) {
    ind <- subsets[[i]]
    c[i] <- ifelse(
      length(ind) == 1,
      1,
      uniroot(
        c_function,
        lower = 0.9,
        upper = 1 / min(subw),
        w = subw[ind],
        cr = subcr[ind, ind],
        alpha = alpha
      )$root
    )
    wP[I[ind]] <- subw[ind] * c[i]
  }

  c
}

# Input must be appropriate vectors for a *single* Bonferroni test group
bonferroni <- function(p_values, weights, alpha) {
  p_values <= weights * alpha
}

# Input must be appropriate vectors for a *single* parametric test group
# Calculates the critical value for each parametric test group distinctly
# To use a global c for a given intersection hypothesis
parametric <- function(p_values, weights, alpha, corr, cJ = NULL) {
  c <- cJ
  if (is.null(cJ)) c <- solve_c(weights, corr, alpha)

  p_values <= c * weights * alpha
}

parametric_ <- function(p_values, weights, alpha, corr) {
  sub_corr <- corr
  # Check out Dong's code for this
  cJ <- uniroot(
    myfct,
    lower = 1,
    upper = 9,
    a = alpha,
    w = weights,
    sig = sub_corr
  )$root

  p_values <= cJ * weights * alpha
}

# Input must be appropriate vectors for a *single* Simes test group
simes <- function(p_values, weights, alpha) {
  # browser()
  res <- vector(length = length(weights))

  for (i in seq_along(weights)) {
    w_sum <- sum(weights[p_values <= p_values[[i]]])
    res[[i]] <- p_values[[i]] <= alpha * w_sum
  }

  names(res) <- names(weights)
  res
}
