#' Calculate hypothesis rejection results efficiently
#'
#' For insight and nice reporting, prefer [test_graph()] or
#' [bonferroni_sequential()] with all of their options. They are reasonably fast
#' for interactive use. However in order to minimize power run time, more
#' efficient testing functions are available. The power simulation can be
#' segmented so that certain parts, like generating weights and calculating some
#' critical values, can be done only a single time. The closure testing function
#' has been stripped down to just a few vectorized lines for efficiency. A
#' separate optimized function is available for testing a graph with the
#' Bonferroni sequential shortcut.
#'
#' @param graph An initial graph as returned by [create_graph()]
#' @param p A numeric vector of p-values
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param intersections A compact representation of [generate_weights()] output,
#'   where missing hypotheses get a missing value for weights, and h-vectors are
#'   dropped
#'
#' @return A logical or integer vector of results indicating whether each
#'   hypothesis can be accepted or rejected globally.
#'
#' @rdname testing-fast
#'
#' @keywords internal
#'
#' @seealso [test_graph()], [bonferroni_sequential()]
#'
#' @template references
#'
#' @examples
#' par_gate <- simple_successive_1()
#' graph_size <- length(par_gate$hypotheses)
#'
#' p <- c(.001, .02, .002, .03)
#'
#' weights <- generate_weights(par_gate)
#' inter_h <- weights[, seq_len(graph_size), drop = FALSE]
#' compact_weights <- ifelse(
#'   inter_h,
#'   weights[, seq_len(graph_size) + graph_size, drop = FALSE],
#'   NA
#' )
#' compact_weights[is.na(compact_weights)] <- 0
#'
#' graphicalMCP:::test_graph_fast(p, .025, compact_weights, inter_h)
#' graphicalMCP:::bonferroni_sequential_cpp(par_gate, p, .025)
test_graph_fast <- function(p,
                            alpha,
                            intersections,
                            inter_h) {
  rej_hyps <- t(p <= alpha * t(intersections))

  # "+ 0" converts to integer from logical
  matrixStats::colSums2(inter_h * matrixStats::rowMaxs(rej_hyps + 0)) ==
    2^(ncol(intersections) - 1)
}

#' @rdname testing-fast
# C++ only, pass/fail only
bonferroni_sequential_cpp <- function(graph,
                                      p,
                                      alpha = .05) {
  bonferroni_sequential_cpp_(graph$hypotheses, graph$transitions, p, alpha)
}
