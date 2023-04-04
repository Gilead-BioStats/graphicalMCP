add_critical <- function(gw, corr, alpha, groups) {
  h_vecs <- gw[, seq_len(ncol(gw) / 2)]
  w_vecs <- gw[, seq_len(ncol(gw) / 2) + (ncol(gw) / 2)]

  c_vals <- matrix(nrow = nrow(gw), ncol = length(groups))
  colnames(c_vals) <- paste0("c_grp_", seq_along(groups))

  for (row in seq_len(nrow(gw))) {
    h <- h_vecs[row, ]
    w <- w_vecs[row, ]

    for (grp_index in seq_along(groups)) {
      group <- groups[[grp_index]]
      group_in_inter <- group[as.logical(h[group])]

      c_vals[row, grp_index] <- solve_c(
        w[group_in_inter],
        corr[group_in_inter, group_in_inter],
        alpha
      )
    }


  }

  cbind(gw, c_vals)
}

add_critical_list <- function(gw, corr, alpha, groups) {
  h_vecs <- gw[, seq_len(ncol(gw) / 2)]
  w_vecs <- gw[, seq_len(ncol(gw) / 2) + (ncol(gw) / 2)]

  res_list <- lapply(
    groups,
    function(group) {
      matrix(
        nrow = nrow(gw),
        ncol = length(group) * 2 + 1,
        dimnames = list(rownames(gw), c(rep(colnames(gw)[group], 2), "c"))
      )
    }
  )
  # c_vals <- vector("numeric", length(groups))

  # c_vals <- matrix(nrow = nrow(gw), ncol = length(groups))
  # colnames(c_vals) <- paste0("c_grp_", seq_along(groups))

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

# add_critical_list_list <- function(gw, corr, alpha, groups) {
#   h_vecs <- gw[, seq_len(ncol(gw) / 2)]
#   w_vecs <- gw[, seq_len(ncol(gw) / 2) + (ncol(gw) / 2)]
#
#   res_list <- lapply(
#     groups,
#     function(group) {
#       list(
#         h = matrix(
#           nrow = nrow(gw),
#           ncol = length(group),
#           dimnames = list(rownames(gw), colnames(gw)[group])
#         ),
#         w = matrix(
#           nrow = nrow(gw),
#           ncol = length(group),
#           dimnames = list(rownames(gw), colnames(gw)[group])
#         ),
#         c = matrix(
#           nrow = nrow(gw),
#           ncol = 1,
#           dimnames = list(rownames(gw), "critical")
#         ),
#         corr =
#       )
#       matrix(
#         nrow = nrow(gw),
#         ncol = length(group) * 2 + 1,
#         dimnames = list(rownames(gw), c(rep(colnames(gw)[group], 2), "c"))
#       )
#     }
#   )
#   # c_vals <- vector("numeric", length(groups))
#
#   # c_vals <- matrix(nrow = nrow(gw), ncol = length(groups))
#   # colnames(c_vals) <- paste0("c_grp_", seq_along(groups))
#
#   for (row in seq_len(nrow(gw))) {
#     h <- h_vecs[row, ]
#     w <- w_vecs[row, ]
#
#     for (grp_index in seq_along(groups)) {
#       group <- groups[[grp_index]]
#       group_in_inter <- group[as.logical(h[group])]
#
#       c_val <- solve_c(
#         w[group_in_inter],
#         corr[group_in_inter, group_in_inter],
#         alpha
#       )
#
#       res_list[[grp_index]] <- list(
#         h = h[group],
#         w = w[group],
#         c = c_val,
#         corr = corr[group, group]
#       )
#     }
#
#   }
#
#   res_list
# }

