#' Calculate hypothesis rejection results efficiently
#'
#' For insight and nice reporting, prefer [graph_test_closure()] or
#' [graph_test_shortcut()] with all of their options. They are reasonably fast
#' for interactive use. However in order to minimize power run time, more
#' efficient testing functions are available. The power simulation can be
#' segmented so that certain parts, like generating weights and calculating some
#' adjusted weights, can be done only a single time. The closure testing
#' function has been stripped down to just a few vectorized lines for
#' efficiency. A separate optimized function is available for testing a graph
#' with the Bonferroni sequential shortcut.
#'
#' @param graph An initial graph as returned by [graph_create()]
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param adjusted_weights The weights (second half of columns) from
#'   [graph_generate_weights()] output, adjusted by the appropriate testing
#'   algorithm (Bonferroni, Simes, or parametric)
#' @param intersections The first half of columns from
#'   [graph_generate_weights()] output, indicating which hypotheses are
#'   contained in each intersection
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
#' adjusted_weights <-
#'   weighting_strategy[, seq_len(num_hyps) + num_hyps, drop = FALSE]
#'
#' graphicalMCP:::graph_test_closure_fast(
#'   p,
#'   .025,
#'   adjusted_weights,
#'   intersections
#' )
#' graphicalMCP:::graph_test_shortcut_fast(
#'   p,
#'   .025,
#'   adjusted_weights
#' )
graph_test_closure_fast <- function(p,
                                    alpha,
                                    adjusted_weights,
                                    matrix_intersections) {
  rej_hyps <- t(p <= alpha * t(adjusted_weights))

  # "+ 0" converts to integer from logical
  matrixStats::colSums2(
    matrix_intersections * matrixStats::rowMaxs(rej_hyps + 0)
  ) == 2 ^ (ncol(adjusted_weights) - 1)
}

#' @rdname testing-fast
graph_test_shortcut_fast <- function(p, alpha, adjusted_weights) {
  num_hyps <- ncol(adjusted_weights)

  # There is a mapping from current rejected hypotheses to corresponding row of
  # the closure weights matrix by treating the rejected vector as a binary
  # number. This line creates a vector of binary place values.
  binary_slots <- 2^(num_hyps:1 - 1)
  nrow_critical <- nrow(adjusted_weights)

  rejected <- vector("logical", num_hyps)

  while (!all(rejected)) {
    # The actual mapping to intersection number is to treat the rejected vector
    # as a binary number, then count that many lines up from the bottom of the
    # weights matrix, then go down one line
    intersection_num <-
      nrow_critical - sum(binary_slots * !rejected) + 1
    rejected_step <-
      p <= adjusted_weights[intersection_num, , drop = TRUE] * alpha

    if (!any(rejected_step)) {
      break
    } else {
      rejected <- rejected | rejected_step
    }
  }

  rejected
}
