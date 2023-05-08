#' @rdname critical-vals
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

#' @rdname critical-vals
solve_c <- function(w, corr, alpha) {
  n_hyps <- seq_along(w)
  c <- ifelse(
    length(n_hyps) == 1 || sum(w) == 0,
    1,
    stats::uniroot(
      c_function,
      lower = 0.9, # Why is this not -Inf? Ohhhh because c >= 1
      # upper > 40 errors when w[i] ~= 1 && w[j] = epsilon
      # upper = 2 errors when w = c(.5, .5) && all(corr == 1)
      # furthermore, even under perfect correlation & with balanced weights, the
      # c_function does not seem to exceed `length(w)`
      upper = length(w) + 1,
      w = w,
      corr = corr,
      alpha = alpha
    )$root
  )

  c
}

#' Calculate testing critical values for the closure of a graph
#'
#' @param gw_small A compact representation of `generate_weights()` output,
#'   where missing hypotheses get a missing value for weights, and h-vectors are
#'   dropped
#' @param corr A numeric matrix specifying the correlation between the test
#'   statistics of hypotheses to be tested using parametric testing
#' @param p A numeric vector of hypothesis p-values
#' @param alpha A numeric scalar specifying the global significance level for
#'   parametric testing
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param w A numeric vector of graph weights
#' @param x The root to solve for with `uniroot()`
#'
#' @return Outputs:
#' * For `calculate_critical_parametric()`, a matrix with the same shape as
#'   gw_small, where the weights in the second half of columns have been
#'   multiplied by the parametric critical value for the group they are in
#' * For `c_function()`, the critical value for the given group
#'
#' @rdname critical-vals
#' @export
#'
#' @examples
#' p <- 1:6 / 200
#'
#' g <- bonferroni_holm(6)
#' gw_large <- generate_weights(g)
#'
#' gw <- ifelse(gw_large[, 1:6], gw_large[, 7:12], NA)
#'
#' para_critical <- calculate_critical_parametric(gw, diag(6), .05, list(1:3))
#' simes_critical <- calculate_critical_simes(gw, p, list(4:6))
calculate_critical_parametric <- function(gw_small, corr, alpha, groups) {
  h_vecs <- !is.na(gw_small)

  c_mat <- gw_small # placeholder

  for (group in groups) {
    for (row in seq_len(nrow(gw_small))) {
      group_in_inter <- group[as.logical(h_vecs[row, ][group])]

      c_val <- solve_c(
        gw_small[row, group_in_inter],
        corr[group_in_inter, group_in_inter],
        alpha
      )

      c_mat[row, group] <- c_val * h_vecs[row, group]
    }
  }

  (c_mat * gw_small)[, unlist(groups), drop = FALSE]
}

#' @rdname critical-vals
#' @export
calculate_critical_simes <- function(gw_small, p, groups) {
  missing_index <- is.na(gw_small)
  gw_small[missing_index] <- 0
  graph_names <- colnames(gw_small)[unlist(groups)]

  list_w_new <- vector("list", length(groups))
  i <- 1

  for (group in groups) {
    list_w_new[[i]] <- matrixStats::rowCumsums(
      gw_small[, group[order(p[group])], drop = FALSE],
      useNames = TRUE
    )

    i <- i + 1
  }

  res <- do.call(cbind, list_w_new)[, graph_names, drop = FALSE]
  res[missing_index[, unlist(groups), drop = FALSE]] <- NA

  res
}
