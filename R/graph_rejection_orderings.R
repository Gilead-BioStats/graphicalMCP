#' Find alternate rejection orderings for shortcut testing
#'
#' When using `graph_test_shortcut()`, there may be multiple hypotheses which
#' can be rejected at each step. The specific hypothesis chosen is decided based
#' on the minimum adjusted p-value, but final results don't change if rejection
#' order changes. This function shows every other order that deletes the same
#' hypotheses, where each hypothesis is still a valid rejection at the step it
#' is deleted.
#'
#' @param shortcut_test_result A `graph_report` object as returned by
#'   [graph_test_shortcut()]
#'
#' @return A modified `graph_report` object containing all valid orderings for
#'   deleting the significant hypotheses
#' @export
#'
#' @examples
#' graph <- simple_successive_2()
#'
#' short_res <- graph_test_shortcut(graph, c(.018, .01, .03, .004))
#'
#' # Reject H1, H2, and H4
#' short_res$outputs$rejected
#'
#' # But these cannot be rejected in any order - For instance, H4 has 0 weight
#' # in the initial graph and cannot be rejected first
#' graph_rejection_orderings(short_res)
#'
#' # Finally, intermediate graphs can be obtained by putting one of the
#' # orderings outputs into `graph_update()`
#' graph_update(graph, delete = c(2, 1, 4))
graph_rejection_orderings <- function(shortcut_test_result) {
  # Extract basic testing values -----------------------------------------------
  graph <- shortcut_test_result$inputs$graph
  p <- shortcut_test_result$inputs$p
  alpha <- shortcut_test_result$inputs$alpha

  hyp_names <- names(graph$hypotheses)

  # Permute rejected hypotheses ------------------------------------------------
  rejected <- which(shortcut_test_result$outputs$rejected)

  list_possible_orderings <- apply(
    rev(expand.grid(rep(list(rejected), length(rejected)))),
    1,
    function(row) {
      if (length(unique(row)) == length(row)){
        structure(row, names = hyp_names[row])
      } else {
        NULL
      }
    }
  )
  list_possible_orderings <- Filter(Negate(is.null), list_possible_orderings)

  # Find which permutations are valid rejection orderings ----------------------
  orderings_valid <- vector("logical", length(list_possible_orderings))

  for (hyp_ordering_num in seq_along(list_possible_orderings)) {
    hyp_ordering <- list_possible_orderings[[hyp_ordering_num]]
    intermediate_graph <- graph

    for (hyp_num in hyp_ordering) {

      if (p[[hyp_num]] <= intermediate_graph$hypotheses[[hyp_num]] * alpha) {
        intermediate_graph <-
          graph_update(intermediate_graph, hyp_num)$updated_graph
      } else {
        orderings_valid[[hyp_ordering_num]] <- FALSE
        break
      }

      orderings_valid[[hyp_ordering_num]] <- TRUE
    }
  }

  structure(
    c(
      shortcut_test_result,
      list(valid_orderings = list_possible_orderings[orderings_valid])
    ),
    class = "graph_report"
  )
}
