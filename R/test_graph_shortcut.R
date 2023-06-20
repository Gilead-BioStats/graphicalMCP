#' @rdname testing
#' @export
test_graph_shortcut <- function(graph,
                                p,
                                alpha = .025,
                                verbose = FALSE,
                                critical = FALSE) {
  # Input processing and useful initial values ---------------------------------
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

  hyp_names <- names(graph$hypotheses)
  graph_size <- length(graph$hypotheses)


  names(p) <- hyp_names
  adj_p <- structure(vector("numeric", graph_size), names = hyp_names)
  adj_p_max <- 0

  proc_sequence <- vector("integer")

  list_critical <- if (critical) vector("list")

  # loop over and delete every hypothesis, not just until failure, so that the
  # adjusted p-values can all be calculated correctly
  for (i in seq_along(graph$hypotheses)) {
    hyps_not_processed <- setdiff(hyp_names, proc_sequence)

    adj_p_subgraph <- p / graph$hypotheses

    # which.min will throw an error if all elements are missing; we want to
    # catch this before which.min does
    if (all(is.nan(adj_p_subgraph[hyps_not_processed]))) {
      err_msg <- paste0(
        "All weights and p-values are 0\n",
        "  Deleted ", paste(proc_sequence, collapse = ", "), "\n",
        paste(
          utils::capture.output(print(
            graph,
            indent = 2,
            precision = 6,
            title = paste0("Step ", i, ", Graph state:")
          )),
          collapse = "\n"
        )
      )

      stop(err_msg)
    }

    # chooses the first sequentially in case of a tie in value
    # ignoring prior processed is necessary in the case of all 0 weights
    min_hyp_name <- names(which.min(adj_p_subgraph[hyps_not_processed]))

    # largest adjusted p-value seen so far
    adj_p_max <- max(adj_p_max, adj_p_subgraph[[min_hyp_name]])
    adj_p[[min_hyp_name]] <- adj_p_max

    proc_sequence <- c(proc_sequence, min_hyp_name)

    # we only want critical values from graphs with a rejection; after that, all
    # critical values come from the last remaining graph
    if (adj_p_max <= alpha) {
      if (critical) {
        critical_step <- bonferroni_test_vals(
          p[min_hyp_name],
          graph$hypotheses[min_hyp_name],
          alpha
        )
        critical_step[[1]] <- i
        names(critical_step)[[1]] <- "step"
        critical_step$Test <- NULL
        critical_step$c <- NULL
        critical_step$`*` <- NULL

        list_critical <- c(list_critical, list(critical_step))
      }

      # save graph state after last rejection
      last_graph <- graph
    }

    graph <- update_graph(graph, !hyp_names %in% proc_sequence)$updated_graph

  }

  rejected <- adj_p <= alpha
  adj_p <- pmin(adj_p, 1)

  # Verbose and critical calculations ------------------------------------------
  # the loop only calculates critical values for rejected hypotheses; here we
  # get the rest of the critical values from the last-updated graph
  if (verbose) {
    del_sequence <- proc_sequence[seq_along(which(rejected))]

    graph_seq <- vector("list", length(del_sequence) + 1)
    graph_seq[[1]] <- initial_graph

    for (i in seq_along(del_sequence)) {
      keep <- rep(TRUE, graph_size)
      names(keep) <- hyp_names
      keep[[del_sequence[[i]]]] <- FALSE

      graph_seq[[i + 1]] <- update_graph(graph_seq[[i]], keep)$updated_graph
    }

    details <- list(del_sequence = del_sequence, results = graph_seq)
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
      details = if (verbose) details,
      critical = if (critical) df_critical
    ),
    class = "graph_report"
  )
}
