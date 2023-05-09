#' Calculate weights for the closure of a graph
#'
#' The closure of a graph is the set of all sub-graphs, or intersections
#' hypotheses, of a graph. Weights for each sub-graph are calculated using the
#' weighting strategy defined in Bretz et al (2011).
#'
#' @param graph An initial graph as returned by [create_graph()]
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
#' @examples
#'
#' par_gate <- create_graph(
#'   hypotheses = c(.5, .5, 0, 0),
#'   transitions = rbind(
#'     c(0, 0, 1, 0),
#'     c(0, 0, 0, 1),
#'     c(0, 1, 0, 0),
#'     c(1, 0, 0, 0)
#'   )
#' )
#'
#' generate_weights(par_gate)
#'
generate_weights <- function(graph) {
  g_names <- names(graph$hypotheses)
  n <- length(graph$hypotheses)

  parents <- do.call(c, lapply(2^(seq_len(n) - 1), seq_len))[-(2^n - 1)]
  delete <- rep(rev(seq_len(n)), 2^(seq_len(n) - 1))[-(2^n - 1)]

  graphs <- vector("list", length(parents))
  graphs[[1]] <- graph

  for (i in seq_along(parents)) {
    parent <- graphs[[parents[[i]]]]
    del_index <- which(g_names[[delete[[i]]]] == names(parent$hypotheses))

    init_hypotheses <- parent$hypotheses
    init_transitions <- parent$transitions

    hypotheses <- parent$hypotheses
    transitions <- parent$transitions

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

    graphs[[i + 1]] <- structure(
      list(
        hypotheses = hypotheses[-del_index],
        transitions = as.matrix(transitions[-del_index, -del_index])
      ),
      class = "initial_graph"
    )
  }

  wgts_mat <- structure(
    do.call(
      rbind,
      lapply(graphs, function(graph) {
        graph$hypotheses[g_names]
      })
    ),
    dimnames = list(1:(2^length(g_names) - 1), g_names)
  )

  wgts_mat_h <- !is.na(wgts_mat)
  wgts_mat[is.na(wgts_mat)] <- 0

  cbind(wgts_mat_h, wgts_mat)
}
