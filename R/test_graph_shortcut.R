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
  num_hyps <- length(graph$hypotheses)

  # Adjusted p-value calculations ----------------------------------------------
  names(p) <- hyp_names
  adjusted_p <- structure(vector("numeric", num_hyps), names = hyp_names)
  adjusted_p_max <- 0

  adjusted_p_sequence <- vector("integer")

  # calculate adjusted p-values for all hypotheses by deleting every hypothesis
  # one at a time
  for (hyp_num in seq_along(graph$hypotheses)) {
    hyps_not_deleted <- setdiff(hyp_names, adjusted_p_sequence)
    hyps_zero_weight <- names(graph$hypotheses[graph$hypotheses == 0])

    adjusted_p_subgraph <-
      p[hyps_not_deleted] / graph$hypotheses[hyps_not_deleted]

    # which.min will throw an error if all elements are missing; we want to
    # catch this before which.min does
    if (all(is.nan(adjusted_p_subgraph))) {
      err_msg <- paste0(
        "All weights and p-values are 0\n",
        "  Deleted ", paste(adjusted_p_sequence, collapse = ", "), "\n",
        paste(
          utils::capture.output(print(
            graph,
            indent = 2,
            precision = 6,
            title = paste0("Step ", hyp_num, ", Graph state:")
          )),
          collapse = "\n"
        )
      )

      stop(err_msg)
    }

    # chooses the first sequentially in case of a tie in value
    # ignoring prior processed is necessary in the case of all 0 weights
    min_hyp_name <- names(which.min(adjusted_p_subgraph[hyps_not_deleted]))

    # the adjusted p-value for the current hypothesis being considered is the
    # largest adjusted p-value seen so far in the sequence
    adjusted_p_max <- max(adjusted_p_max, adjusted_p_subgraph[[min_hyp_name]])
    adjusted_p[[min_hyp_name]] <- adjusted_p_max

    adjusted_p_sequence <- c(adjusted_p_sequence, min_hyp_name)

    graph <-
      update_graph(graph, !hyp_names %in% adjusted_p_sequence)$updated_graph
  }

  adjusted_p <- pmin(adjusted_p, 1) # adjusted p-values should not exceed 1
  rejected <- adjusted_p <= alpha

  # Adjusted p-value details (sequence of graphs) ------------------------------
  if (verbose) {
    # the first n = (number of rejected) hypotheses in the adjusted p sequence
    # are the hypotheses that will be rejected
    rejection_sequence <- adjusted_p_sequence[seq_along(which(rejected))]

    # the sequence of graphs is the initial graph, plus one entry for each
    # rejected hypothesis
    graph_sequence <- vector("list", length(rejection_sequence) + 1)
    graph_sequence[[1]] <- initial_graph

    if (length(rejection_sequence) > 0) {
      verbose_keep <- rep(TRUE, num_hyps)
      names(verbose_keep) <- hyp_names

      for (hyp_num_to_reject in seq_along(rejection_sequence)) {
        hyp_name_to_reject <- rejection_sequence[[hyp_num_to_reject]]
        verbose_keep[[hyp_name_to_reject]] <- FALSE

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
    step_graph <- initial_graph
    graph_after_rejections <-
      update_graph(initial_graph, !rejected)$updated_graph

    df_critical <- NULL

    critical_keep <- rep(TRUE, num_hyps)
    names(critical_keep) <- hyp_names

    for (i in seq_along(adjusted_p_sequence)) {
      delete_hypothesis <- adjusted_p_sequence[[i]]

      step_num <- if (rejected[delete_hypothesis]) i else NA

      critical_step <- bonferroni_test_vals(
        p[delete_hypothesis],
        step_graph$hypotheses[delete_hypothesis],
        alpha
      )
      names(critical_step)[[1]] <- "step"
      critical_step$step <- step_num
      critical_step[c("Test", "c", "*")] <- NULL

      df_critical <- rbind(df_critical, critical_step)

      if (rejected[delete_hypothesis]) {
        step_graph <- update_graph(
          step_graph,
          !hyp_names == delete_hypothesis
        )$updated_graph
      } else {
        step_graph <- graph_after_rejections
      }
    }
  }

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
        p_adj = adjusted_p,
        rejected = rejected,
        graph = update_graph(initial_graph, !rejected)$updated_graph
      ),
      details = if (verbose) details,
      critical = if (critical) list(results = df_critical)
    ),
    class = "graph_report"
  )
}
