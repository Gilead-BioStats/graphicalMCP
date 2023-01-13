# target usage
# for a given graph:
#   check how p-val compares to weight/alpha
# hypotheses <- c(0.5, 0.5, 0, 0)
# transitions <- rbind(
#   c(0, 0, 1, 0),
#   c(0, 0, 0, 1),
#   c(0, 1, 0, 0),
#   c(1, 0, 0, 0)
# )
# names <- c("H1", "H2", "H3", "H4")
# g <- create_graph(hypotheses, transitions, names)
#
# perform_tests(
#   g,
#   p_vals = c(.01, .02, .05, .1),
#   alpha = .05,
#   tests = list(
#     bonferroni = 1,
#     simes = list(c(2)),
#     parametric = list(c(3, 4))
#   )
# )
#
# Do we test an individual graph, or do we test the output of generate_weights?
test_graph <- function(graph, p_values, alpha,
                       tests = list(
                         bonferroni = seq_along(graph$hypotheses),
                         simes = NULL,
                         parametric = NULL
                       )) {
  hypotheses <- graph$hypotheses

  # Weighted Bonferroni test
  res_bonferroni <- p_values[tests$bonferroni] <=
    hypotheses[tests$bonferroni] * alpha

  # TODO: Simes test will go here
  res_simes <- lapply(
    tests$simes,
    \(simes_group) hypotheses[simes_group] == hypotheses[simes_group]
  )

  # TODO: Parametric test will go here
  res_parametric <- lapply(
    tests$parametric,
    \(para_group) hypotheses[para_group] == hypotheses[para_group]
  )

  structure(
    list(
      graph = graph,
      p_values = p_values,
      alpha = alpha,
      test_results = list(
        bonferroni = res_bonferroni,
        simes = res_simes,
        parametric = res_parametric
      )
    ),
    class = "graph_report"
  )
}

test_all_subgraphs <- function(graph, p_values, alpha = .05,
                       tests = list(
                         bonferroni = list(seq_along(graph$hypotheses)),
                         simes = NULL,
                         parametric = NULL
                       )) {
  stopifnot(
    "please choose exactly one test per hypothesis" =
      setequal(seq_along(graph$hypotheses), unlist(tests))
  )

  hypothesis_names <- names(graph$hypotheses)

  subgraphs <- generate_weights_recursive(graph)
  subgraphs_weights <- subgraphs[, (ncol(subgraphs) / 2 + 1):ncol(subgraphs)]

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

      unlist(c(res_bonferroni, res_simes, res_parametric))[hypothesis_names]
    },
    simplify = FALSE
  )

  weight_res_matrix <- cbind(subgraphs, do.call(rbind, res_list))

  structure(
    list(
      initial_graph = graph,
      p_values = p_values,
      alpha = alpha,
      test_results = weight_res_matrix
    ),
    class = "graph_report"
  )
}
