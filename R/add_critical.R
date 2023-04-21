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

#' Calculate testing critical values for the closure of a graph
#'
#' @param gw A numeric matrix as created by `generate_weights()`
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
#' * For `add_critical()`, a list of matrices. Each list element is the
#'   output of `generate_weights()` for a single group with the critical value
#'   for each intersection appended as an additional column.
#' * For `calculate_critical_parametric()`, a matrix with the same shape as gw,
#'   where the weights in the second half of columns have been multiplied by the
#'   parametric critical value for the group they are in
#' * For `c_function()`, the critical value for the given group
#'
#' @rdname critical-vals
#' @export
#'
#' @examples
#' g <- bonferroni_holm(6)
#' gw <- generate_weights(g)
#'
#' add_critical(gw, diag(6), .05, list(1:3, 4:6))
#'
#' # Can handle groups only containing some hypotheses
#' add_critical(gw, diag(6), .05, list(1:2, c(4, 6)))
add_critical <- function(gw, corr, alpha, groups) {
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

#' @rdname critical-vals
#' @export
calculate_critical_parametric <- function(gw, corr, alpha, groups) {
  h_vecs <- gw[, seq_len(ncol(gw) / 2)]
  w_vecs <- gw[, seq_len(ncol(gw) / 2) + (ncol(gw) / 2)]

  c_mat <- w_vecs # placeholder

  for (group in groups) {
    for (row in seq_len(nrow(w_vecs))) {
      group_in_inter <- group[as.logical(h_vecs[row, ][group])]

      c_val <- solve_c(
        w_vecs[row, group_in_inter],
        corr[group_in_inter, group_in_inter],
        alpha
      )

      c_mat[row, group] <- c_val * h_vecs[row, group]
    }
  }

  ifelse(h_vecs, c_mat * w_vecs, NA)[, unlist(groups), drop = FALSE]
}

#' @rdname critical-vals
#' @export
calculate_critical_parametric2 <- function(gw_small, corr, alpha, groups) {
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
  graph_size <- ncol(gw_small) / 2
  graph_names <- colnames(gw_small)[unlist(groups)]

  list_w_new <- vector("list", length(groups))
  i <- 1

  for (group in groups) {
    w_new <- gw_small[, group[order(p[group])], drop = FALSE]

    for (row in seq_len(nrow(w_new))) {
      w_new[row, !is.na(w_new[row, ])] <- cumsum(w_new[row, !is.na(w_new[row, ])])
    }

    list_w_new[[i]] <- w_new
    i <- i + 1
  }

  do.call(cbind, list_w_new)[, graph_names]
}

#' @rdname critical-vals
#' @export
calculate_critical_simes_vms <- function(gw_small, p, groups) {
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
