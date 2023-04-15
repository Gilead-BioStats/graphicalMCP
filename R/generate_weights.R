#' Generate weights for each intersections hypothesis in the full closure tree
#' of an MCP graph
#'
#' @param graph An MCP graph as created by `create_graph()`
#'
#' @return A numeric matrix of all intersection hypothesis weights. Each row
#'   corresponds to a single intersection hypothesis. The first half of the
#'   columns indicate which hypotheses are included in the given intersection
#'   hypothesis, and the second half of columns are the weights
#'
#' @section Performance:

#' Much thought was given to the performance of this code, as the memory and
#' time usage can grow quickly as graph size grows. On the systems used for
#' testing, a size 10 graph had a median run time of 20-60 ms. Run time
#' increases at a rate of O(2 ^ n), so e.g. a size 5 graph takes approximately
#' twice as long to run as a size 4 graph. See
#' `vignette("generate-weights-performance")` for a detailed analysis and
#' explanation
#'
#' @export
#'
#' @examples
#'
#' par_gate <- create_graph(
#'   hypotheses = c(.5, .5, 0, 0),
#'   transitions = rbind(
#'     c(0, 0, 1, 0),
#'     c(0, 0, 0, 1),
#'     c(0, 1, 0, 0),
#'     c(1, 0, 0, 0)
#'   )
#' )
#'
#' generate_weights(par_gate)
#'
generate_weights <- function(graph) {
  g_names <- names(graph$hypotheses)
  n <- length(graph$hypotheses)

  parents <- do.call(c, lapply(2^(seq_len(n) - 1), seq_len))[-(2^n - 1)]
  delete <- rep(rev(seq_len(n)), 2^(seq_len(n) - 1))[-(2^n - 1)]

  graphs <- vector("list", length(parents))
  graphs[[1]] <- graph

  for (i in seq_along(parents)) {
    parent <- graphs[[parents[[i]]]]
    del_index <- which(g_names[[delete[[i]]]] == names(parent$hypotheses))

    init_hypotheses <- parent$hypotheses
    init_transitions <- parent$transitions

    hypotheses <- parent$hypotheses
    transitions <- parent$transitions

    hyp_nums <- seq_along(hypotheses)[seq_along(hypotheses) != del_index]

    for (hyp_num in hyp_nums) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[del_index]] * init_transitions[[del_index, hyp_num]]

      denominator <- 1 - init_transitions[[hyp_num, del_index]] *
        init_transitions[[del_index, hyp_num]]

      for (end_num in hyp_nums) {
        if (hyp_num == end_num || denominator <= 0) {
          transitions[[hyp_num, end_num]] <- 0
        } else {
          transitions[[hyp_num, end_num]] <-
            (init_transitions[[hyp_num, end_num]] +
              init_transitions[[hyp_num, del_index]] *
                init_transitions[[del_index, end_num]]) / denominator
        }
      }
    }

    graphs[[i + 1]] <- structure(
      list(
        hypotheses = hypotheses[-del_index],
        transitions = as.matrix(transitions[-del_index, -del_index])
      ),
      class = "initial_graph"
    )
  }

  wgts_mat <- structure(
    do.call(
      rbind,
      lapply(graphs, function(graph) {
        graph$hypotheses[g_names]
      })
    ),
    dimnames = list(1:(2^length(g_names) - 1), g_names)
  )

  wgts_mat_h <- !is.na(wgts_mat)
  wgts_mat[is.na(wgts_mat)] <- 0

  cbind(wgts_mat_h, wgts_mat)
}

generate_weights2 <- function(graph) {
  g_names <- names(graph$hypotheses)
  n <- length(graph$hypotheses)

  parents <- do.call(c, lapply(2^(seq_len(n) - 1), seq_len))[-(2^n - 1)]
  delete <- rep(rev(seq_len(n)), 2^(seq_len(n) - 1))[-(2^n - 1)]

  graphs <- vector("list", length(parents))
  graphs[[1]] <- graph

  for (i in seq_along(parents)) {
    parent <- graphs[[parents[[i]]]]
    del_index <- which(g_names[[delete[[i]]]] == names(parent$hypotheses))

    init_hypotheses <- parent$hypotheses
    init_transitions <- parent$transitions

    hypotheses <- parent$hypotheses
    transitions <- parent$transitions

    hyp_nums <- seq_along(hypotheses)[seq_along(hypotheses) != del_index]

    for (hyp_num in hyp_nums) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[del_index]] * init_transitions[[del_index, hyp_num]]

      denominator <- 1 - init_transitions[[hyp_num, del_index]] *
        init_transitions[[del_index, hyp_num]]

      for (end_num in hyp_nums) {
        if (hyp_num == end_num || denominator <= 0) {
          transitions[[hyp_num, end_num]] <- 0
        } else {
          transitions[[hyp_num, end_num]] <-
            (init_transitions[[hyp_num, end_num]] +
               init_transitions[[hyp_num, del_index]] *
               init_transitions[[del_index, end_num]]) / denominator
        }
      }
    }

    graphs[[i + 1]] <- structure(
      list(
        hypotheses = hypotheses[-del_index],
        transitions = as.matrix(transitions[-del_index, -del_index])
      ),
      class = "initial_graph"
    )
  }

  wgts_mat <- structure(
    do.call(
      rbind,
      lapply(graphs, function(graph) {
        graph$hypotheses[g_names]
      })
    ),
    dimnames = list(1:(2^length(g_names) - 1), g_names)
  )

  wgts_mat_h <- !is.na(wgts_mat)
  wgts_mat[is.na(wgts_mat)] <- 0

  # cbind(wgts_mat_h, wgts_mat)

  nm_graphs <- setNames(graphs, seq_along(graphs))

  nm_inter_wgt <- lapply(seq_along(gw_graph_list), \(i) {
      cbind(
        i = i,
        hyp = names(gw_graph_list[[i]]$hypotheses),
        wgt = gw_graph_list[[i]]$hypotheses
      )
    })

  df_wgt <- as.data.frame(do.call(rbind, nm_inter_wgt), row.names = FALSE)
  df_wgt$i <- as.integer(df_wgt$i)
  df_wgt$wgt <- as.numeric(df_wgt$wgt)

  df_wgt
}

test_graph_df <- function(df_wgt, p, alpha = .05) {
  p_all <- p[df_wgt$hyp]
  res_all <- p_all <= alpha * df_wgt$wgt

  rej_inter <- aggregate(res_all, by = list(df_wgt$i), max)
  # rej_inter <- aggregate(res ~ i, data = df_wgt, max)

  df_wgt_rej_inter <- merge(df_wgt, rej_inter, by.x = "i", by.y = "Group.1", sort = FALSE)

  rej_global <- aggregate(x ~ hyp, data = df_wgt_rej_inter, min)

  rej_global$x
}
