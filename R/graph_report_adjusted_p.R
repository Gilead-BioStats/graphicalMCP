#' Title
#'
#' @param graph
#' @param p_values
#' @param alpha
#' @param groups
#' @param tests
#' @param corr
#' @param verbose
#'
#' @return
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
#'
#' g <- create_graph(hypotheses, transitions)
#' p <- c(.01, .005, .015, .022)
#'
#' corr <- matrix(nrow = 4, ncol = 4)
#' corr[3:4, 3:4] <- .5
#' diag(corr) <- 1
#'
#' corr2 <- matrix(.5, nrow = 4, ncol = 4)
#' diag(corr2) <- 1
#'
#' # The default is all Bonferroni with alpha = .05
#' test_graph2(g, p)
#'
#' # But tests can be specified at the hypothesis-level
#' test_graph2(
#'   graph = g,
#'   p_values = p,
#'   alpha = .025,
#'   groups = list(1, 2, 3:4),
#'   tests = c("bonferroni", "simes", "parametric"),
#'   corr = corr
#' )
test_graph2 <- function(graph,
                        p_values,
                        alpha = .05,
                        groups = list(seq_along(graph$hypotheses)),
                        tests = c("bonferroni"),
                        corr = NULL,
                        verbose = TRUE) {
  g_size <- length(graph$hypotheses)
  hyp_names <- names(graph$hypotheses)

  subgraphs <- generate_weights(graph)
  subgraphs_h_vecs <- subgraphs[, seq_len(g_size), drop = FALSE]
  subgraphs_weights <- subgraphs[, seq_len(g_size) + g_size, drop = FALSE]

  # test_results <- matrix(
  #   nrow = nrow(subgraphs_weights),
  #   ncol = ncol(subgraphs_weights)
  # )

  # adj_p_results <- matrix(
  #   nrow = nrow(subgraphs_weights),
  #   ncol = ncol(subgraphs_weights)
  # )

  adj_p_j <- vector("numeric", nrow(subgraphs_weights))

  for (row in seq_len(nrow(subgraphs_weights))) {
    # if (row == 3) browser()
    h <- as.logical(subgraphs_h_vecs[row, ])
    weights <- subgraphs_weights[row, ]

    # Need to remove indices from groups that aren't in this intersection
    # Furthermore, need to pass them in as names rather than positions to
    # account for some indices being removed
    groups_in <- lapply(
      groups,
      function(group) hyp_names[group][as.logical(h[group])]
    )

    # Only calculate adjusted p-values for hypotheses in this intersection
    # But add NA for hypotheses that are missing
    # Returns one adjusted p-value per group
    adj_p_values <- p_adjust(
      p_values[h],
      weights[h],
      groups_in,
      tests,
      corr[h, h, drop = FALSE]
    )

    adj_p_j[[row]] <- min(1, adj_p_values, na.rm = TRUE)

    # test_results[row, ] <- adj_p_values <= alpha

    # adj_p_results[row, ] <- adj_p_values
  }

  adj_p_global <- apply(subgraphs_h_vecs * adj_p_j, 2, max, na.rm = TRUE)
# browser()
  reject_intersection <- adj_p_j <= alpha
  # Each hypothesis appears in half of the 2^n intersections hypotheses. Each
  # intersection a hypothesis is in must be rejected to reject the hypothesis
  # globally
  reject_hyps <- adj_p_global <= alpha
  reject_hyps1 <- (reject_intersection %*% subgraphs_h_vecs) == 2^g_size / 2
  if (!isTRUE(all.equal(reject_hyps1[1, ], reject_hyps))) browser()
  if (verbose) {
    # Removes the "c *" columns from the detail dataframe when using only Simes
    # & Bonferroni
    # if (length(tests$parametric) == 0) test_details[, 3:4] <- NULL

    res_names <- c(
      hyp_names,
      paste(hyp_names, "wgt", sep = "_"),
      "rej_h_j",
      "adj_p_j"
    )

    weight_res_matrix <- structure(
      cbind(
        as.data.frame(subgraphs_h_vecs),
        as.data.frame(subgraphs_weights),
        data.frame(rej_h_j = reject_intersection),
        data.frame(adj_p_j = adj_p_j)
      ),
      names = res_names
    )

  }

  structure(
    list(
      initial_graph = graph,
      p_values = structure(p_values, names = hyp_names),
      adj_p_values = structure(adj_p_global, colnames = hyp_names),
      alpha = alpha,
      test_used = list(
        bonferroni = groups[tests == "bonferroni"],
        parametric = groups[tests == "parametric"],
        simes = groups[tests == "simes"]
      ),
      corr = corr,
      hypotheses_rejected = reject_hyps,
      test_results = if (verbose) weight_res_matrix
    ),
    class = "graph_report"
  )

}
