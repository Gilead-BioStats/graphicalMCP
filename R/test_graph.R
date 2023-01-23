#' Apply weighted Bonferroni, parametric, and Simes tests
#'
#' @param graph An initial graph as created by `create_graph()`
#' @param p_values A numeric vector of p-values, one for each hypothesis in the
#'   graph
#' @param alpha A numeric vector of length 1 specifying the un-weighted alpha
#'   level at which to test each hypothesis
#' @param tests A list with three elements, `bonferroni`, `simes`, and
#'   `parametric`. Each element is a list of hypothesis groups to apply the
#'   given test to. Each hypothesis must be specified exactly once, so that the
#'   length of all elements of the list equals the number of hypotheses. The
#'   default is to apply the weighted Bonferroni test to all hypotheses
#'
#' @return A `graph_report` object, consisting of
#'   * The initial graph being tested,
#'   * p-values & alpha used for tests,
#'   * Which hypotheses can be rejected, and
#'   * Detailed test results matrix, including the results of
#'   `generate_weights()` & test results for each intersection hypothesis
#' @export
#'
#' @examples
#'
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- create_graph(hypotheses, transitions)
#'
#' test_graph(
#'   g,
#'   p_vals = c(.01, .02, .05, .1),
#'   alpha = .05,
#'   tests = list(
#'     bonferroni = 1,
#'     simes = list(c(2)),
#'     parametric = list(c(3, 4))
#'   )
#' )
test_graph <- function(graph, p_values, alpha = .05,
                       tests = list(
                         bonferroni = list(seq_along(graph$hypotheses)),
                         simes = NULL,
                         parametric = NULL
                       )) {
  stopifnot(
    "please choose exactly one test per hypothesis" =
      setequal(seq_along(graph$hypotheses), unlist(tests))
  )

  graph_size <- length(graph$hypotheses)
  hyp_names <- names(graph$hypotheses)

  subgraphs <- generate_weights(graph)
  subgraphs_h_vecs <- subgraphs[, seq_len(graph_size)]
  subgraphs_weights <- subgraphs[, seq_len(graph_size) + graph_size]

  res_list <- apply(
    X = subgraphs_weights,
    MARGIN = 1,
    FUN = function(weights) {
      # Weighted Bonferroni test
      res_bonferroni <- lapply(
        tests$bonferroni,
        function(bonf_group) {
          p_values[bonf_group] <= weights[bonf_group] * alpha
        }
      )

      # TODO: Simes test will go here
      res_simes <- lapply(
        tests$simes,
        function(simes_group) {
          weights[simes_group] == weights[simes_group]
        }
      )

      # TODO: Parametric test will go here
      res_parametric <- lapply(
        tests$parametric,
        function(para_group) {
          weights[para_group] == weights[para_group]
        }
      )

      unlist(c(res_bonferroni, res_simes, res_parametric))[hyp_names]
    },
    simplify = FALSE
  )

  test_results <- do.call(rbind, res_list)
  reject_intersection <- rowSums(test_results) > 0
  reject_hypotheses <- (reject_intersection %*% subgraphs_h_vecs) ==
    2 ^ graph_size / 2


  # This is kind of print-y stuff ---
  res_names <- c(
    hyp_names,
    "|",
    paste(hyp_names, "wgt", sep = "_"),
    "|",
    paste(hyp_names, "test", sep = "_"),
    "|",
    "rej_Hj"
  )

  weight_res_matrix <- structure(
    cbind(
      as.data.frame(subgraphs_h_vecs),
      data.frame("|" = "|"),
      as.data.frame(subgraphs_weights),
      data.frame("|" = "|"),
      as.data.frame(test_results),
      data.frame("|" = "|"),
      data.frame(rej_Hj = reject_intersection)
    ),
    names = res_names
  )
  # ---

  structure(
    list(
      initial_graph = graph,
      alpha = alpha,
      test_used = tests,
      p_values = structure(p_values, names = hyp_names),
      hypotheses_rejected = reject_hypotheses[1, ],
      test_results = weight_res_matrix
    ),
    class = "graph_report"
  )
}
