#' @rdname testing
#' @export
test_graph_shortcut <- function(graph,
                                p,
                                alpha = .025,
                                verbose = FALSE,
                                critical = FALSE) {
  test_input_val(
    graph,
    p,
    alpha,
    groups = list(seq_along(graph$hypotheses)),
    test_types = "bonferroni",
    corr = NULL,
    verbose = verbose,
    critical = critical
  )

  initial_graph <- graph

  adj_p_max <- 0
  adj_p <- vector("numeric", length(graph$hypotheses))

  rejected <- vector("logical", length(graph$hypotheses))
  keep <- rep(TRUE, length(graph$hypotheses))
  del_sequence <- vector("integer")

  list_critical <- if (critical) vector("list")
  df_critical <- NULL

  names(adj_p) <- names(graph$hypotheses)
  names(rejected) <- names(graph$hypotheses)

  for (i in seq_along(graph$hypotheses)) {
    adj_p_subgraph <- p / graph$hypotheses

    # chooses the first sequentially in case of a tie in value
    min_index <- which.min(adj_p_subgraph)

    # largest adjusted p-value seen so far
    adj_p_max <- max(adj_p_max, adj_p_subgraph[[min_index]])

    adj_p[[min_index]] <- min(adj_p_max, 1) # cap adjusted p-values at 1
    rejected[[min_index]] <- adj_p_max <= alpha

    keep[[min_index]] <- FALSE
    graph <- update_graph(graph, keep)$updated_graph

    # we only want critical values from graphs with a rejection. After that, all
    # critical values come from the last remaining graph
    if (adj_p_max <= alpha) {

      if (critical) {
        critical_step <- bonferroni_test_vals(
          p[min_index],
          graph$hypotheses[min_index],
          alpha
        )
        critical_step[[1]] <- i
        names(critical_step)[[1]] <- "step"
        critical_step$Test <- NULL
        critical_step$c <- NULL
        critical_step$`*` <- NULL

        list_critical <- c(list_critical, list(critical_step))
      }

      del_sequence <- c(del_sequence, i)

      # save graph state after last rejection
      last_graph <- graph
    }
  }

  if (critical) {
    critical_step <- bonferroni_test_vals(
      p[!rejected],
      graph$hypotheses[!rejected],
      alpha
    )
    if (any(!rejected)) critical_step[[1]] <- NA
    names(critical_step)[[1]] <- "step"
    critical_step$Test <- NULL
    critical_step$c <- NULL
    critical_step$`*` <- NULL

    list_critical <- c(list_critical, list(critical_step))

    df_critical <- list(results = do.call(rbind, list_critical))
  }

  details <- NULL

  if (verbose) {
    graph_seq <- vector("list", length(del_sequence) + 1)
    graph_seq[[1]] <- initial_graph

    for (i in seq_along(del_sequence)) {
      keep <- rep(TRUE, length(graph$hypotheses))
      keep[[del_sequence[[i]]]] <- FALSE

      graph_seq[[i + 1]] <- update_graph(graph_seq[[i]], keep)$updated_graph
    }

    details <- list(del_sequence = del_sequence, results = graph_seq)
  }

  structure(
    list(
      inputs = list(
        graph = initial_graph,
        p = p,
        alpha = alpha,
        groups = list(seq_along(initial_graph$hypotheses)),
        test_types = "bonferroni",
        corr = NULL
      ),
      outputs = list(
        p_adj = adj_p,
        rejected = rejected,
        graph = update_graph(initial_graph, !rejected)$updated_graph
      ),
      details = details,
      critical = df_critical
    ),
    class = "graph_report"
  )
}
