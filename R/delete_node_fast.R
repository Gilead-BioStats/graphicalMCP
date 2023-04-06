#' Remove a hypothesis from a graph
#'
#' @param graph An object of class `initial_graph`, for example as returned by
#'   `create_graph()`
#' @param del_index An integer scalar indicating which hypothesis to remove
#'
#' @return An object of class `initial_graph`
#' @export
#'
#' @examples
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#'
#' g <- create_graph(hypotheses, transitions)
#'
#' delete_hypothesis(g, 3)
delete_hypothesis <- function(graph, del_index) {
  init_hypotheses <- graph$hypotheses
  init_transitions <- graph$transitions

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  hyp_nums <- seq_along(hypotheses)[seq_along(hypotheses) != del_index]

  for (hyp_num in hyp_nums) {
    hypotheses[[hyp_num]] <-
      init_hypotheses[[hyp_num]] +
      init_hypotheses[[del_index]] * init_transitions[[del_index, hyp_num]]

    denominator <- 1 - init_transitions[[hyp_num, del_index]] *
      init_transitions[[del_index, hyp_num]]

    for (end_num in hyp_nums) {
      if (hyp_num == end_num || denominator <= 0) {
        transitions[[hyp_num, end_num]] <- 0
      } else {
        transitions[[hyp_num, end_num]] <-
          (init_transitions[[hyp_num, end_num]] +
            init_transitions[[hyp_num, del_index]] *
              init_transitions[[del_index, end_num]]) / denominator
      }
    }
  }

  hypotheses[del_index] <- 0
  transitions[del_index, ] <- 0
  transitions[, del_index] <- 0

  structure(
    list(
      hypotheses = hypotheses,
      transitions = as.matrix(transitions)
    ),
    class = "initial_graph"
  )
}
