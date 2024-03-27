#' Perform graphical multiple comparison procedures efficiently for power
#' calculation
#'
#' @description
#' These functions performs similarly to [graph_test_closure()] or
#' [graph_test_shortcut()] but are optimized for efficiently calculating power.
#' For example, generating weights and calculating adjusted weights can be done
#' only once. Vectorization has been applied where possible.
#'
#' @param graph An initial graph as returned by [graph_create()].
#' @param p A numeric vector of one-sided p-values (unadjusted, raw), whose
#'   values should be between 0 & 1. The length should match the number of
#'   hypotheses in `graph`.
#' @param alpha A numeric value of the one-sided overall significance level,
#'   which should be between 0 & 1. The default is 0.025 for one-sided
#'   hypothesis testing. Note that only one-sided tests are supported.
#' @param adjusted_weights The adjusted hypothesis weights, which are the
#'   second half of columns from [graph_generate_weights()] output, adjusted by
#'   the appropriate test types (Bonferroni, Simes, or parametric).
#' @param matrix_intersections A matrix of hypothesis indicators in a weighting
#'   strategy, which are the first half the [graph_generate_weights()] output.
#'
#' @return A logical or integer vector indicating whether each hypothesis can
#'   be rejected or not.
#'
#' @family graphical tests
#'
#' @seealso
#'   * [graph_test_closure()] for closed graphical multiple comparison
#'   procedures.
#'   * [graph_test_shortcut()] for shortcut graphical multiple comparison
#'   procedures.
#'
#' @rdname graph_test_fast
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' alpha <- 0.025
#' p <- c(0.018, 0.01, 0.105, 0.006)
#' num_hyps <- length(p)
#' g <- bonferroni_holm(rep(1 / 4, 4))
#' weighting_strategy <- graph_generate_weights(g)
#'
#' matrix_intersections <- weighting_strategy[, seq_len(num_hyps), drop = FALSE]
#' adjusted_weights <-
#'   weighting_strategy[, -seq_len(num_hyps), drop = FALSE]
#'
#' graphicalMCP:::graph_test_closure_fast(
#'   p,
#'   alpha,
#'   adjusted_weights,
#'   matrix_intersections
#' )
#' graphicalMCP:::graph_test_shortcut_fast(
#'   p,
#'   alpha,
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
  ) == 2^(ncol(adjusted_weights) - 1)
}

#' @rdname graph_test_fast
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
