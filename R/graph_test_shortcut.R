#' @rdname testing
#' @export
graph_test_shortcut <- function(graph,
                                p,
                                alpha = .025,
                                verbose = FALSE,
                                test_values = FALSE) {
  # Input validation -----------------------------------------------------------
  test_input_val(
    graph,
    p,
    alpha,
    groups = list(seq_along(graph$hypotheses)),
    test_types = "bonferroni",
    corr = list(NA),
    verbose = verbose,
    test_values = test_values
  )

  initial_graph <- graph

  hyp_names <- names(graph$hypotheses)
  num_hyps <- length(graph$hypotheses)

  # Adjusted p-value calculations ----------------------------------------------
  names(p) <- hyp_names
  adjusted_p <- structure(vector("numeric", num_hyps), names = hyp_names)
  adjusted_p_max <- 0

  hyps_deleted_sequence <- vector("integer")

  # Calculate adjusted p-values for all hypotheses by deleting every hypothesis
  # one at a time
  for (i in seq_along(graph$hypotheses)) {
    hyps_not_deleted <- setdiff(hyp_names, hyps_deleted_sequence)

    adjusted_p_subgraph <-
      p[hyps_not_deleted] / graph$hypotheses[hyps_not_deleted]

    # which.min will throw an error if all elements are missing; we want to
    # catch this before which.min does
    if (all(is.nan(adjusted_p_subgraph))) {
      err_msg <- paste0(
        "Calculation of adjusted p-values stops when all remaining\n",
        "    hypotheses have 0 hypothesis weights and 0 p-values\n",
        "  Hypotheses [", paste(hyps_deleted_sequence, collapse = ", "), "]\n",
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

    hyps_deleted_sequence <- c(hyps_deleted_sequence, min_hyp_name)

    # Update graph to delete a hypothesis
    graph <-
      graph_update(graph, !hyp_names %in% hyps_deleted_sequence)$updated_graph
  }

  rejected <- round(adjusted_p, 10) <= alpha
  adjusted_p <- pmin(adjusted_p, 1 + 1e-14) # adj p-values should not exceed 1

  # Adjusted p-value details (sequence of graphs) ------------------------------
  if (verbose) {
    # The first n = (number of rejected) hypotheses in the adjusted p sequence
    # are the hypotheses that will be rejected
    rejection_sequence <- hyps_deleted_sequence[seq_along(which(rejected))]

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
        graph_sequence[[hyp_num_to_reject + 1]] <- graph_update(
          graph_sequence[[hyp_num_to_reject]],
          verbose_keep
        )$updated_graph
      }
    }

    details <- list(del_seq = rejection_sequence, results = graph_sequence)
  }

  # Adjusted weight details ----------------------------------------------------
  if (test_values) {
    # Record the final graph after all rejected hypotheses have been deleted
    graph_after_rejections <-
      graph_update(initial_graph, !rejected)$updated_graph

    df_test_values <- NULL

    test_values_keep <- rep(TRUE, num_hyps)
    names(test_values_keep) <- hyp_names

    step_graph <- initial_graph
    step_num <- 1

    # Calculate adjusted weights for all hypotheses. For rejected hypotheses,
    # adjusted weights should come from the last graph they're present in. For
    # non-rejected hypotheses, adjusted weights should be calculated from the
    # graph with all rejected hypotheses deleted.
    for (i in seq_along(hyps_deleted_sequence)) {
      # Follow the same hypothesis order as adjusted p-values
      hyp_name_for_test_values <- hyps_deleted_sequence[[i]]

      # Record adjusted weights
      test_values_step <- test_values_bonferroni(
        p[hyp_name_for_test_values],
        step_graph$hypotheses[hyp_name_for_test_values],
        alpha
      )

      # Normally the first column of `*_test_values()` is an intersection
      # counter. Since shortcut testing doesn't track intersections, re-purpose
      # that column as a step counter. Steps count up one at a time for each
      # hypothesis rejected, then switch to NA for non-rejected hypotheses (i.e.
      # "These rows represent steps that are not taken by the shortcut rejection
      # algorithm")
      names(test_values_step)[[1]] <- "Step"
      test_values_step$Step <- step_num
      test_values_step[c("Test", "c_value", "*")] <- NULL

      df_test_values <- rbind(df_test_values, test_values_step)

      if (rejected[hyp_name_for_test_values]) {
        step_num <- step_num + 1

        step_graph <- graph_update(
          step_graph,
          !hyp_names == hyp_name_for_test_values
        )$updated_graph
      } else {
        step_graph <- graph_after_rejections
      }
    }
  }

  # Build the report -----------------------------------------------------------
  # The core output of a test report is the adjusted p-values, rejection
  # decisions, and resulting graph after deleting all rejected hypotheses.
  # Inputs are recorded as well. Details about adjusted p-values and test
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
        graph = graph_update(initial_graph, !rejected)$updated_graph
      ),
      details = if (verbose) details,
      test_values = if (test_values) list(results = df_test_values)
    ),
    class = "graph_report"
  )
}
