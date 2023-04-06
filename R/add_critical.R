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
      upper = 1 / min(w[w > 0]),
      w = w,
      corr = corr,
      alpha = alpha
    )$root
  )

  c
}

#' Calculate parametric testing critical values for the closure of a graph
#'
#' @param graph An object with class `initial_graph` as returned by
#'   `create_graph()`
#' @param corr A numeric matrix specifying the correlation between the test
#'   statistics of hypotheses to be tested using parametric testing
#' @param alpha A numeric scalar specifying the global significance level for
#'   parametric testing
#' @param groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param w A numeric vector of graph weights
#' @param x The root to solve for with `uniroot()`
#'
#' @return Outputs:
#' * For `add_critical()`, a list of matrices. Each list element is the
#'   output of `generate_weights()` for a single group with the critical value
#'   for each intersection appended as an additional column.
#' * For `c_function()`, the critical value for the given group
#'
#' @rdname critical-vals
#'
#' @examples
#' g <- bonferroni_holm(6)
#' gw <- generate_weights(g)
#'
#' add_critical(gw, diag(6), .05, list(1:3, 4:6))
#'
#' # Can handle groups only containing some hypotheses
#' add_critical(gw, diag(6), .05, list(1:2, c(4, 6)))
add_critical <- function(graph, corr, alpha, groups) {
  gw <- generate_weights(graph)

  h_vecs <- gw[, seq_len(ncol(gw) / 2)]
  w_vecs <- gw[, seq_len(ncol(gw) / 2) + (ncol(gw) / 2)]

  res_list <- lapply(
    groups,
    function(group) {
      matrix(
        nrow = nrow(gw),
        ncol = length(group) * 2 + 1,
        dimnames = list(rownames(gw), c(rep(colnames(gw)[group], 2), "critical"))
      )
    }
  )

  for (row in seq_len(nrow(gw))) {
    h <- h_vecs[row, ]
    w <- w_vecs[row, ]

    for (grp_index in seq_along(groups)) {
      group <- groups[[grp_index]]
      group_in_inter <- group[as.logical(h[group])]

      c_val <- solve_c(
        w[group_in_inter],
        corr[group_in_inter, group_in_inter],
        alpha
      )

      res_list[[grp_index]][row, ] <- c(h[group], w[group], c_val)
    }
  }

  res_list
}
