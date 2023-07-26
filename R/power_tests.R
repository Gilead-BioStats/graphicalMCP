#' Calculate hypothesis rejection results efficiently
#'
#' For insight and nice reporting, prefer [graph_test_closure()] or
#' [graph_test_shortcut()] with all of their options. They are reasonably fast
#' for interactive use. However in order to minimize power run time, more
#' efficient testing functions are available. The power simulation can be
#' segmented so that certain parts, like generating weights and calculating some
#' critical values, can be done only a single time. The closure testing function
#' has been stripped down to just a few vectorized lines for efficiency. A
#' separate optimized function is available for testing a graph with the
#' Bonferroni sequential shortcut.
#'
#' @param graph An initial graph as returned by [graph_create()]
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param critical_values The weights (second half of columns) from
#'   [graph_generate_weights()] output, adjusted by the appropriate testing algorithm
#'   (Bonferroni, Simes, or parametric)
#' @param intersections The first half of columns from [graph_generate_weights()]
#'   output, indicating which hypotheses are contained in each intersection
#'
#' @return A logical or integer vector of results indicating whether each
#'   hypothesis can be accepted or rejected globally.
#'
#' @rdname testing-fast
#'
#' @keywords internal
#'
#' @seealso [graph_test_closure()], [graph_test_shortcut()]
#'
#' @template references
#'
#' @examples
#' par_gate <- simple_successive_1()
#' num_hyps <- length(par_gate$hypotheses)
#'
#' p <- c(.001, .02, .002, .03)
#'
#' weighting_strategy <- graph_generate_weights(par_gate)
#' intersections <- weighting_strategy[, seq_len(num_hyps), drop = FALSE]
#'
#' graphicalMCP:::graph_test_fast(p, .025, weighting_strategy, intersections)
#' graphicalMCP:::graph_test_shortcut_cpp(par_gate, p, .025)
graph_test_fast <- function(p, alpha, critical_values, intersections) {
  rej_hyps <- t(p <= alpha * t(critical_values))

  # "+ 0" converts to integer from logical
  matrixStats::colSums2(intersections * matrixStats::rowMaxs(rej_hyps + 0)) ==
    2^(ncol(critical_values) - 1)
}

#' @rdname testing-fast
graph_test_shortcut_r3 <- function(p,
                                   critical_values,
                                   num_hyps,
                                   bin_slots,
                                   nrow_critical) {
  rejected <- vector("logical", num_hyps)

  while (!all(rejected)) {
    intersection_num <-
      nrow_critical - sum(bin_slots * !rejected) + 1
    rejected_step <- p <= critical_values[intersection_num, , drop = TRUE]

    if (!any(rejected_step)) {
      break
    } else {
      rejected <- rejected | rejected_step
    }
  }

  rejected
}
