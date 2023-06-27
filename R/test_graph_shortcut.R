#' @rdname testing
#' @export
test_graph_shortcut <- function(graph,
                                p,
                                alpha = .025,
                                verbose = FALSE,
                                critical = FALSE) {
  # Input validation -----------------------------------------------------------
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
  num_hyps <- length(graph$hypotheses)

  # Adjusted p-value calculations ----------------------------------------------
  names(p) <- hyp_names
  adjusted_p <- structure(vector("numeric", num_hyps), names = hyp_names)
  adjusted_p_max <- 0

  adjusted_p_sequence <- vector("integer")

  # Calculate adjusted p-values for all hypotheses by deleting every hypothesis
  # one at a time
  for (i in seq_along(graph$hypotheses)) {
    hyps_not_deleted <- setdiff(hyp_names, adjusted_p_sequence)

    adjusted_p_subgraph <-
      p[hyps_not_deleted] / graph$hypotheses[hyps_not_deleted]

    # which.min will throw an error if all elements are missing; we want to
    # catch this before which.min does
    if (all(is.nan(adjusted_p_subgraph))) {
      err_msg <- paste0(
        "Calculation of adjusted p-values stops when all remaining\n",
        "    hypotheses have 0 hypothesis weights and 0 p-values\n",
        "  Hypotheses [", paste(adjusted_p_sequence, collapse = ", "), "]\n",
        "    have been deleted\n",
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

    # Identify the hypothesis with the smallest adjusted p-value only among
    # hypotheses not deleted so far. Choose the first hypothesis in case of a
    # tie in adjusted p-values.
    min_hyp_name <- names(which.min(adjusted_p_subgraph[hyps_not_deleted]))

    # Record the adjusted p-value for the current hypothesis being considered;
    # that is, the largest adjusted p-value seen so far in the sequence
    adjusted_p_max <- max(adjusted_p_max, adjusted_p_subgraph[[min_hyp_name]])
    adjusted_p[[min_hyp_name]] <- adjusted_p_max

    adjusted_p_sequence <- c(adjusted_p_sequence, min_hyp_name)

    # Update graph to delete a hypothesis
    graph <-
      update_graph(graph, !hyp_names %in% adjusted_p_sequence)$updated_graph
  }

  adjusted_p <- pmin(adjusted_p, 1) # adjusted p-values should not exceed 1
  rejected <- adjusted_p <= alpha

  # Adjusted p-value details (sequence of graphs) ------------------------------
  if (verbose) {
    # The first n = (number of rejected) hypotheses in the adjusted p sequence
    # are the hypotheses that will be rejected
    rejection_sequence <- adjusted_p_sequence[seq_along(which(rejected))]

    # The sequence of graphs is the initial graph, plus one entry for each
    # rejected hypothesis
    graph_sequence <- vector("list", length(rejection_sequence) + 1)
    graph_sequence[[1]] <- initial_graph

    if (length(rejection_sequence) > 0) {
      verbose_keep <- rep(TRUE, num_hyps)
      names(verbose_keep) <- hyp_names

      # Starting from the original initial graph, delete each hypothesis with
      # adjusted p-value less than alpha. Record the graph state after each
      # deletion
      for (hyp_num_to_reject in seq_along(rejection_sequence)) {
        hyp_name_to_reject <- rejection_sequence[[hyp_num_to_reject]]
        verbose_keep[[hyp_name_to_reject]] <- FALSE

        # Update a graph to delete a hypothesis. Record the resulting graph
        graph_sequence[[hyp_num_to_reject + 1]] <- update_graph(
          graph_sequence[[hyp_num_to_reject]],
          verbose_keep
        )$updated_graph
      }
    }

    details <- list(del_seq = rejection_sequence, results = graph_sequence)
  }

  # Critical value details -----------------------------------------------------
  if (critical) {
    # Record the final graph after all rejected hypotheses have been deleted
    graph_after_rejections <-
      update_graph(initial_graph, !rejected)$updated_graph

    df_critical <- NULL

    critical_keep <- rep(TRUE, num_hyps)
    names(critical_keep) <- hyp_names

    step_graph <- initial_graph

    # Calculate critical values for all hypotheses. For rejected hypotheses,
    # critical values should come from the last graph they're present in. For
    # non-rejected hypotheses, critical values should be calculated from the
    # graph with all rejected hypotheses deleted.
    for (i in seq_along(adjusted_p_sequence)) {
      # Follow the same hypothesis order as adjusted p-values
      hyp_name_for_critical <- adjusted_p_sequence[[i]]

      # Record critical values
      critical_step <- bonferroni_test_vals(
        p[hyp_name_for_critical],
        step_graph$hypotheses[hyp_name_for_critical],
        alpha
      )

      # Normally the first column of `*_test_vals()` is an intersection counter.
      # Since shortcut testing doesn't track intersections, re-purpose that
      # column as a step counter. Steps count up one at a time for each
      # hypothesis rejected, then switch to NA for non-rejected hypotheses (i.e.
      # "These rows represent steps that are not taken by the shortcut rejection
      # algorithm")
      names(critical_step)[[1]] <- "step"
      critical_step$step <- if (rejected[hyp_name_for_critical]) i else NA
      critical_step[c("Test", "c", "*")] <- NULL

      df_critical <- rbind(df_critical, critical_step)

      hyp_name_for_critical_is_rejected <- rejected[hyp_name_for_critical]
      if (hyp_name_for_critical_is_rejected) {
        step_graph <- update_graph(
          step_graph,
          !hyp_names == hyp_name_for_critical
        )$updated_graph
      } else {
        step_graph <- graph_after_rejections
      }
    }
  }

  # Build the report -----------------------------------------------------------
  # The core output of a test report is the adjusted p-values, rejection
  # decisions, and resulting graph after deleting all rejected hypotheses.
  # Inputs are recorded as well. Details about adjusted p-values and critical
  # values are optionally available.
  structure(
    list(
      inputs = list(
        graph = initial_graph,
        p = p,
        alpha = alpha,
        groups = list(seq_len(num_hyps)),
        test_types = "bonferroni",
        corr = NULL
      ),
      outputs = list(
        adjusted_p = adjusted_p,
        rejected = rejected,
        graph = update_graph(initial_graph, !rejected)$updated_graph
      ),
      details = if (verbose) details,
      critical = if (critical) list(results = df_critical)
    ),
    class = "graph_report"
  )
}
