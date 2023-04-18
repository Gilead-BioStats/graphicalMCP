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
#' @param gw An numeric matrix as created by `generate_weights()`
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

add_critical2 <- function(gw, corr, alpha, groups) {
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

  ifelse(h_vecs, c_mat * w_vecs, NA)
}

bonf_small <- function(gw) {
  ifelse(
    gw[, seq_len(ncol(gw) / 2)],
    gw[, seq_len(ncol(gw) / 2) + (ncol(gw) / 2)],
    NA
  )
}

simes_new_weights <- function(gw, p, groups) {
  # may be more memory-efficient to not provision these ahead of time
  h_vecs <- gw[, seq_len(ncol(gw) / 2)]
  w_vecs <- gw[, seq_len(ncol(gw) / 2) + (ncol(gw) / 2)]

  w_new <- w_vecs # placeholder

  for (row in seq_len(nrow(w_vecs))) {
    w_new_row <- w_vecs[row, ]

    for (group in groups) {
      group_in_inter <- group[as.logical(h_vecs[row, ][group])]

      w_group <- w_vecs[row, group_in_inter]
      p_group <- p[group_in_inter]

      for (i in group_in_inter) {
        w_new_row[[i]] <- sum(w_group[p_group <= p[[i]]])
      }
    }

    w_new[row, ] <- w_new_row
  }

  cbind(h_vecs, w_new)
}

simes_new_weights2 <- function(gw, p, groups) {
  graph_size <- ncol(gw) / 2
  h_cols <- seq_len(graph_size)
  weight_cols <- h_cols + graph_size

  # may be more memory-efficient to not provision these ahead of time
  # h_vecs <- gw[, h_cols]
  # w_vecs <- gw[, weight_cols]

  w_new <- matrix(
    NA_real_,
    nrow = 2^graph_size - 1,
    ncol = graph_size,
    dimnames = list(rownames(gw), colnames(gw)[h_cols])
  ) # placeholder

  for (row in seq_len(nrow(gw))) {
    w_new_row <- gw[row, h_cols]

    for (group in groups) {
      group_in_inter <- group[as.logical(gw[row, h_cols][group])]

      w_group <- gw[row, group_in_inter + graph_size]
      p_group <- p[group_in_inter]

      for (i in group_in_inter) {
        w_new_row[[i]] <- sum(w_group[p_group <= p[[i]]])
      }
    }

    w_new[row, ] <- w_new_row
  }

  cbind(gw[, h_cols], w_new)
}

simes_new_weights3 <- function(gw, p, groups) {
  graph_size <- ncol(gw) / 2
  h_cols <- seq_len(graph_size)
  weight_cols <- h_cols + graph_size

  # may be more memory-efficient to not provision these ahead of time
  # h_vecs <- gw[, h_cols]
  # w_vecs <- gw[, weight_cols]

  # w_new <- matrix(NA_real_, nrow = 2^graph_size - 1, ncol = graph_size) # placeholder

  for (row in seq_len(nrow(gw))) {
    for (group in groups) {
      group_in_inter <- group[as.logical(gw[row, h_cols][group])]

      # w_group <- gw[row, group_in_inter + graph_size]
      # p_group <- p[group_in_inter]

      for (i in group_in_inter) {
        gw[row, i + graph_size] <- sum(gw[row, group_in_inter + graph_size][p[group_in_inter] <= p[[i]]])
      }
    }
  }

  gw
}

simes_order <- function(gw_small, p, groups) {
  graph_size <- ncol(gw_small) / 2
  graph_names <- colnames(gw_small)

  group_order <- lapply(groups, function(group) group[order(p[group])])

  w_vecs <- lapply(
    group_order,
    function(group_ord) gw_small[, group_ord]
  )

  list_w_new <- lapply(
    seq_along(groups),
    function(group) {
      w_new <- w_vecs[[group]]

      for (row in seq_len(nrow(w_new))) {
        w_new[row, !is.na(w_new[row, ])] <- cumsum(w_new[row, !is.na(w_new[row, ])])
      }

      w_new
    }
  )

  do.call(cbind, list_w_new)[, graph_names]
}

simes_order2 <- function(gw_small, p, groups) {
  graph_size <- ncol(gw) / 2
  graph_names <- colnames(gw_small)

  w_vecs <- lapply(
    groups,
    function(group) gw_small[, group[order(p[group])]]
  )

  list_w_new <- lapply(
    seq_along(groups),
    function(group) {
      w_new <- w_vecs[[group]]

      for (row in seq_len(nrow(w_new))) {
        w_new[row, !is.na(w_new[row, ])] <- cumsum(w_new[row, !is.na(w_new[row, ])])
      }

      w_new
    }
  )

  do.call(cbind, list_w_new)[, graph_names]
}

simes_order3 <- function(gw_small, p, groups) {
  graph_size <- ncol(gw_small) / 2
  graph_names <- colnames(gw_small)

  list_w_new <- lapply(
    groups,
    function(group) {
      w_new <- gw_small[, group[order(p[group])], drop = FALSE]

      for (row in seq_len(nrow(w_new))) {
        w_new[row, !is.na(w_new[row, ])] <- cumsum(w_new[row, !is.na(w_new[row, ])])
      }

      w_new
    }
  )

  do.call(cbind, list_w_new)[, graph_names]
}

simes_order4 <- function(gw_small, p, groups) {
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

simes_order_vms <- function(gw_small, p, groups) {
  graph_size <- ncol(gw_small) / 2
  graph_names <- colnames(gw_small)[unlist(groups)]
  missing_ind <- is.na(gw_small[, unlist(groups), drop = FALSE])
  gw_small[missing_ind] <- 0

  list_w_new <- vector("list", length(groups))
  i <- 1

  for (group in groups) {
    w_new <- matrixStats::rowCumsums(
      gw_small[, group[order(p[group])], drop = FALSE],
      useNames = TRUE
    )

    list_w_new[[i]] <- w_new
    i <- i + 1
  }

  res <- do.call(cbind, list_w_new)[, graph_names, drop = FALSE]
  res[missing_ind] <- NA

  res
}

#' @rdname critical-vals
parse_groups <- function(gw, corr, alpha, groups, test_types) {
  # w/o para, can be as simple as lapply(groups, function(group) gw[, c(group, group + (ncol(gw) / 2))])

  parsed <- vector("list", length(groups))

  for (group_num in seq_along(groups)) {
    group <- groups[[group_num]]
    h_group <- gw[, group, drop = FALSE]
    weight_group <- gw[, group + (ncol(gw) / 2), drop = FALSE]
    gw_group <- ifelse(h_group == 0, NA, weight_group)
    c_vals <- NULL

    if (test_types[[group_num]] == "parametric") {
      c_vals <- vector("numeric", nrow(gw_group))

      for (i in seq_len(nrow(gw))) {
        h <- !!h_group[i, , drop = TRUE]

        if (all(h == 0)) {
          c_vals[[i]] <- 1
        } else {
          weights <- weight_group[i, , drop = TRUE]

          c_vals[[i]] <- solve_c(weights[h], corr[h, h], alpha)
        }

      }

    }

    parsed[[group_num]] <- list(
      test = test_types[[group_num]],
      gw = gw_group,
      critical = c_vals
    )

  }

  parsed
}

