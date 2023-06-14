#' @rdname testing
#' @export
test_graph_shortcut <- function(graph,
                                  p,
                                  alpha = .05,
                                  critical = FALSE) {
  test_input_val(
    graph,
    p,
    alpha,
    groups = list(seq_along(graph$hypotheses)),
    test_types = "bonferroni",
    corr = NULL,
    verbose = FALSE,
    critical = critical
  )

  initial_graph <- graph

  adj_p_max <- 0
  adj_p <- vector("numeric", length(graph$hypotheses))
  rejected <- vector("logical", length(graph$hypotheses))
  critical_vals <- if (critical) vector("list", length(graph$hypotheses))
  keep <- rep(TRUE, length(graph$hypotheses))

  for (i in seq_along(graph$hypotheses)) {
    adj_p_subgraph <- p / graph$hypotheses
    min_index <- which.min(adj_p_subgraph)

    adj_p_max <- max(adj_p_max, adj_p_subgraph[[min_index]])

    adj_p[[min_index]] <- min(adj_p_max, 1) # cap adjusted p-values at 1
    rejected[[min_index]] <- adj_p_max <= alpha

    if (critical) {
      critical_step <- bonferroni_test_vals(
        p[min_index],
        graph$hypotheses[min_index],
        alpha
      )
      critical_step[[1]] <- i
      names(critical_step)[[1]] <- "step"
      critical_step[6:7] <- NULL

      critical_vals[[i]] <- critical_step
    }

    keep[[min_index]] <- FALSE
    graph <- update_graph(graph, keep)$updated_graph
  }
  names(adj_p) <- names(graph$hypotheses)
  names(rejected) <- names(graph$hypotheses)

  critical_vals <- if (critical) list(results = do.call(rbind, critical_vals))

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
      outputs = list(p_adj = adj_p, rejected = rejected),
      details = NULL,
      critical = critical_vals
    ),
    class = "graph_report"
  )
}
