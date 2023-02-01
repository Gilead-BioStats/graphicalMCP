# good for now - could convert to cpp at some point
delete_node_fast <- function(graph, del_index) {
  init_hypotheses <- graph$hypotheses
  init_transitions <- graph$transitions

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  hyp_nums <-
    seq_along(hypotheses)[seq_along(hypotheses) != del_index]

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

  structure(
    list(
      hypotheses = hypotheses[-del_index],
      transitions = as.matrix(transitions[-del_index, -del_index])
    ),
    class = "initial_graph"
  )
}
