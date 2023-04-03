#' @rdname testing
#' @export
# full function
bonferroni_sequential <- function(graph,
                                  p,
                                  alpha = .05,
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
  critical_vals <- if (critical) vector("list", length(graph$hypotheses))

  for (i in seq_along(graph$hypotheses)) {
    adj_p_subgraph <- p / graph$hypotheses
    min_index <- which.min(adj_p_subgraph)

    adj_p_max <- max(adj_p_max, adj_p_subgraph[[min_index]])

    adj_p[[min_index]] <- adj_p_max
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

    graph <- zero_node_fast(graph, min_index)
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

#' @rdname testing
#' @export
# full function with optional input val; delete node with C++
bs_r_del_node_cpp <- function(graph,
                                  p,
                                  alpha = .05,
                                  verbose = FALSE,
                                  critical = FALSE,
                                  check_input = TRUE) {
  if (check_input) {
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
  }

  initial_graph <- graph

  adj_p_max <- 0
  adj_p <- vector("numeric", length(graph$hypotheses))
  rejected <- vector("logical", length(graph$hypotheses))
  critical_vals <- if (critical) vector("list", length(graph$hypotheses))

  for (i in seq_along(graph$hypotheses)) {
    adj_p_subgraph <- p / graph$hypotheses
    min_index <- which.min(adj_p_subgraph)

    adj_p_max <- max(adj_p_max, adj_p_subgraph[[min_index]])

    adj_p[[min_index]] <- adj_p_max
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

    graph <- zero_node_cpp(graph, min_index)
  }
  names(adj_p) <- names(initial_graph$hypotheses)
  names(rejected) <- names(initial_graph$hypotheses)

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

#' @rdname testing
#' @export
# full function with optional input val; adjusted p with C++
bs_r_del_node_cpp <- function(graph,
                                   p,
                                   alpha = .05,
                                   verbose = FALSE,
                                   critical = FALSE,
                                   check_input = TRUE) {
  if (check_input) {
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
  }

  initial_graph <- graph

  adj_p_max <- 0
  adj_p <- vector("numeric", length(graph$hypotheses))
  rejected <- vector("logical", length(graph$hypotheses))
  critical_vals <- if (critical) vector("list", length(graph$hypotheses))

  p_adj <- bonferroni_sequential_cpp(graph$hypotheses, graph$transitions, p, alpha)
  rejected <- p_adj <= alpha
  names(p_adj) <- names(graph$hypotheses)
  names(rejected) <- names(graph$hypotheses)

  critical_vals <- if (critical) list(results = do.call(rbind, critical_vals))

  structure(
    list(
      inputs = list(
        graph = graph,
        p = p,
        alpha = alpha,
        groups = list(seq_along(initial_graph$hypotheses)),
        test_types = "bonferroni",
        corr = NULL
      ),
      outputs = list(p_adj = p_adj, rejected = rejected),
      details = NULL,
      critical = critical_vals
    ),
    class = "graph_report"
  )
}

#' @rdname testing
#' @export
# C++ only, pass/fail only
bs_cpp <- function(graph,
                                   p,
                                   alpha = .05,
                                   verbose = FALSE,
                                   critical = FALSE,
                                   check_input = TRUE) {
  bonferroni_sequential_cpp(graph$hypotheses, graph$transitions, p, alpha)
}
