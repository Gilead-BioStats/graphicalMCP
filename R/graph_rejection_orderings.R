#' Find alternate rejection orderings (sequences) for shortcut tests
#'
#' @description
#' When multiple hypotheses are rejected by using [graph_test_shortcut()],
#' there may be multiple orderings or sequences in which hypotheses are rejected
#' one by one. The default order in [graph_test_shortcut()] is based on the
#' adjusted p-values, from the smallest to the largest. This function
#' [graph_rejection_orderings()] provides all possible and valid orders
#' (or sequences) of rejections. Although the order of rejection does not affect
#' the final rejection decisions \insertCite{bretz-2009-graphical}{graphicalMCP},
#' different sequences could offer different ways to explain the step-by-step
#' process of shortcut graphical multiple comparison procedures.
#'
#' @param shortcut_test_result A `graph_report` object as returned by
#'   [graph_test_shortcut()].
#'
#' @return A modified `graph_report` object containing all valid orderings of
#'   rejections of hypotheses
#'
#' @family graphical tests
#'
#' @seealso
#'   [graph_test_shortcut()] for shortcut graphical multiple comparison
#'   procedures.
#'
#' @rdname graph_rejection_orderings
#'
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @references
#'  * \insertRef{bretz-2009-graphical}{graphicalMCP}
#'  * \insertRef{bretz-2011-graphical}{graphicalMCP}
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 4 in \insertCite{bretz-2011-graphical;textual}{graphicalMCP}.
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' delta <- 0.5
#' transitions <- rbind(
#'   c(0, delta, 1 - delta, 0),
#'   c(delta, 0, 0, 1 - delta),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#'
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' alpha <- 0.025
#'
#' shortcut_testing <- graph_test_shortcut(g, p, alpha, verbose = TRUE)
#'
#' # Reject H1, H2, and H4
#' shortcut_testing$outputs$rejected
#'
#' # Default order of rejections: H2, H1, H4
#' shortcut_testing$details$del_seq
#'
#' # There is another valid sequence of rejection: H2, H4, H1
#' graph_rejection_orderings(shortcut_testing)$valid_orderings
#'
#' # Finally, intermediate updated graphs can be obtained by providing the order
#' # of rejections into `[graph_update()]`
#' graph_update(g, delete = c(2, 4, 1))
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
      if (length(unique(row)) == length(row)) {
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
