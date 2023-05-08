#' Test a graph efficiently
#'
#' For insight and nice reporting, prefer `test_graph()` with all of its
#' options. It is reasonably fast for interactive use. However in order to
#' minimize power run time, a more efficient testing function has been written.
#' The power simulation can be segmented so that certain parts, like generating
#' weights and calculating some critical values, can be done only a single time.
#' The actual testing function has been stripped down to just a few vectorized
#' lines for efficiency
#'
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global level to test at
#' @param intersections A compact representation of the output created by
#'   `generate_weights()`
#'
#' @return A logical vector of results indicating whether each hypothesis can be
#'   accepted or rejected globally
#'
#' @rdname testing-fast
#' @export
#'
#' @examples
#' par_gate <- simple_successive_1()
#' graph_size <- length(par_gate$hypotheses)
#'
#' p <- c(.001, .02, .002, .03)
#'
#' weights <- generate_weights(par_gate)
#' compact_weights <- ifelse(
#'   weights[, seq_len(graph_size), drop = FALSE],
#'   weights[, seq_len(graph_size) + graph_size, drop = FALSE],
#'   NA
#' )
#'
#' test_graph_fast_vms(p, .025, compact_weights)
test_graph_fast_vms <- function(p,
                                alpha,
                                intersections) {
  graph_size <- ncol(intersections)
  inter_h <- !is.na(intersections) # extract h-matrix
  intersections[is.na(intersections)] <- 0 # replace missing weights with 0

  # Calculate test results -----------------------------------------------------
  rej_hyps <- t(p <= alpha * t(intersections))

  # "+ 0" converts to integer from logical
  matrixStats::colSums2(inter_h * matrixStats::rowMaxs(rej_hyps + 0)) ==
    2^(graph_size - 1)
}
