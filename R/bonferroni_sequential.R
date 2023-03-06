#' @rdname testing
#' @export
bonferroni_sequential <- function(graph, p, alpha = .05) {
  initial_graph <- graph

  adj_p_max <- 0
  adj_p <- vector("numeric", length(graph$hypotheses))
  rejected <- vector("logical", length(graph$hypotheses))

  for (i in seq_along(graph$hypotheses)) {
    adj_p_subgraph <- p / graph$hypotheses
    min_index <- which.min(adj_p_subgraph)

    adj_p_max <- max(adj_p_max, adj_p_subgraph[[min_index]])

    adj_p[[min_index]] <- adj_p_max
    rejected[[min_index]] <- adj_p_max <= alpha
    graph <- zero_node_fast(graph, min_index)
  }
  names(adj_p) <- names(graph$hypotheses)
  names(rejected) <- names(graph$hypotheses)

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
      critical = NULL
    ),
    class = "graph_report"
  )
}
