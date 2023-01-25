myfct <- function(x, a, w, sig) {
  1 - a - mvtnorm::pmvnorm(lower = -Inf, upper = qnorm(1 - x * w * a), sigma = sig)
}

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
test_graph <- function(graph, p_values, alpha = .05, corr = NULL,
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
          res <- vector(length = length(simes_group))
          simes_weights <- weights[simes_group]
          simes_p <- p_values[simes_group]

          for (i in simes_group) {
            w_sum <- sum(simes_weights[simes_p <= simes_p[[i]]])
            res[[i]] <- p_values[[i]] <= alpha * w_sum
          }

          weights[simes_group] == weights[simes_group]
        }
      )

      # This is a good start, but definitely not quite right
      # uniroot() breaks when corr has NAs
      res_parametric <- lapply(
        tests$parametric,
        function(para_group) {
          sub_corr <- corr[para_group, para_group]
          browser()
          cJ <- uniroot(
            myfct,
            lower = 1,
            upper = 9,
            a = alpha,
            w = weights[para_group],
            sig = sub_corr
          )$root

          p_values[para_group] <= cJ * weights[para_group] * alpha
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


  # This is kind of print-y stuff that may not belong here ---
  res_names <- c(
    hyp_names,
    "|",
    paste(hyp_names, "wgt", sep = "_"),
    "|",
    paste(hyp_names, "test", sep = "_"),
    "|",
    "rej_Hj"
  )

  names_mat <- matrix(
    rep(colnames(subgraphs_h_vecs), nrow(subgraphs_h_vecs)),
    nrow = nrow(subgraphs_h_vecs),
    byrow = TRUE
  )

  format_names <- ifelse(subgraphs_h_vecs, names_mat, "")

  weight_res_matrix <- structure(
    cbind(
      as.data.frame(format_names),
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
