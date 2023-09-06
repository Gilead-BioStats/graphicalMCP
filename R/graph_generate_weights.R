#' Calculate weights for the closure of a graph
#'
#' The closure of a graph is the set of all sub-graphs, or intersections
#' hypotheses, of a graph. Weights for each sub-graph are calculated using the
#' weighting strategy defined in Bretz et al (2011).
#'
#' @param graph An initial graph as returned by [graph_create()]
#'
#' @return A numeric matrix of all intersection hypothesis weights. Each row
#'   corresponds to a single intersection hypothesis. The first half of the
#'   columns indicate which hypotheses are included in the given intersection
#'   hypothesis, and the second half of columns are the weights
#'
#' @section Performance:

#' Much thought was given to the performance of this code, as the memory and
#' time usage can grow quickly as graph size grows. On the systems used for
#' testing, a size 10 graph had a median run time of 20-60 ms. Run time
#' increases at a rate of O(2 ^ n), so e.g. a size 5 graph takes approximately
#' twice as long to run as a size 4 graph. See
#' `vignette("generate-weights-performance")` for a detailed analysis and
#' explanation
#'
#' @export
#'
#' @template references
#'
#' @examples
#'
#' par_gate <- graph_create(
#'   hypotheses = c(.5, .5, 0, 0),
#'   transitions = rbind(
#'     c(0, 0, 1, 0),
#'     c(0, 0, 0, 1),
#'     c(0, 1, 0, 0),
#'     c(1, 0, 0, 0)
#'   )
#' )
#'
#' graph_generate_weights(par_gate)
#'
graph_generate_weights <- function(graph) {
  hyp_names <- names(graph$hypotheses)
  num_hyps <- length(graph$hypotheses)

  parents <- do.call(c, lapply(2^(seq_len(num_hyps) - 1), seq_len))
  parents <- parents[-(2^num_hyps - 1)]

  delete <- rep(rev(seq_len(num_hyps)), 2^(seq_len(num_hyps) - 1))
  delete <- delete[-(2^num_hyps - 1)]

  graphs <- vector("list", length(parents))
  graphs[[1]] <- graph

  matrix_weights <- matrix(nrow = 2^num_hyps - 1, ncol = num_hyps)
  dimnames(matrix_weights) <- list(seq_len(2^num_hyps - 1), hyp_names)
  matrix_weights[1, ] <- graph$hypotheses

  for (i in seq_along(parents)) {
    parent <- graphs[[parents[[i]]]]
    del_index <- which(hyp_names[[delete[[i]]]] == names(parent$hypotheses))

    init_hypotheses <- parent$hypotheses
    init_transitions <- parent$transitions

    hypotheses <- parent$hypotheses
    transitions <- parent$transitions

    hyp_nums <- seq_along(hypotheses)[-del_index]

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

    graphs[[i + 1]] <- structure(
      list(
        hypotheses = hypotheses[-del_index],
        transitions =
          as.matrix(transitions[-del_index, -del_index, drop = FALSE])
      ),
      class = "initial_graph"
    )

    matrix_weights[i + 1, ] <- hypotheses[-del_index][hyp_names]
  }

  matrix_intersections <- !is.na(matrix_weights)
  matrix_weights[is.na(matrix_weights)] <- 0

  cbind(matrix_intersections, matrix_weights)
}
