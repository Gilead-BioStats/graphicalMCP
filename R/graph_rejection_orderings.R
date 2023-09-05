graph_rejection_orderings <- function(shortcut_test_result) {
  # Extract basic testing values -----------------------------------------------
  graph <- shortcut_test_result$inputs$graph
  p <- shortcut_test_result$inputs$p
  alpha <- shortcut_test_result$inputs$alpha

  num_hyps <- length(graph$hypotheses)
  hyp_names <- names(graph$hypotheses)

  # Permute rejected hypotheses ------------------------------------------------
  rejected_names <- hyp_names[shortcut_test_result$outputs$rejected]

  list_possible_orderings <- apply(
    expand.grid(rep(list(rejected_names), length(rejected_names))),
    1,
    function(row) if (length(unique(row)) == length(row)) unname(row) else NULL
  )
  list_possible_orderings <- Filter(Negate(is.null), list_possible_orderings)

  # Find which permutations are valid rejection orderings ----------------------
  orderings_valid <- vector("logical", length(list_possible_orderings))
  graph_sequences <- rep(
    list(c(list(graph), vector("list", length(rejected_names)))),
    length(list_possible_orderings)
  )

  for (hyp_ordering_num in seq_along(list_possible_orderings)) {
    hyp_ordering <- list_possible_orderings[[hyp_ordering_num]]
    intermediate_graph <- graph
    graph_index <- 2

    for (hyp_name in hyp_ordering) {
      hyp_num <- which(hyp_name == names(graph$hypotheses))

      if (p[[hyp_num]] <= intermediate_graph$hypotheses[[hyp_num]] * alpha) {
        intermediate_graph <-
          graph_update(intermediate_graph, !hyp_name == hyp_names)$updated_graph

        graph_sequences[[hyp_ordering_num]][[graph_index]] <- intermediate_graph
      } else {
        orderings_valid[[hyp_ordering_num]] <- FALSE
        break
      }

      orderings_valid[[hyp_ordering_num]] <- TRUE
      graph_index <- graph_index + 1
    }
  }

  list(valid_hypothesis_sequences = list_possible_orderings[orderings_valid])

}
