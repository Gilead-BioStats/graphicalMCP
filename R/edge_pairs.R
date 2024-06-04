#' Find pairs of vertices that are connected in both directions
#'
#' @description
#' For an initial graph, find pairs of hypotheses that are connected in both
#' directions. This is used to plot graphs using [plot.initial_graph()].
#'
#' @inheritParams graph_update
#'
#' @return A list of vertex pairs which are connected in both directions. NULL
#'   if no such pairs are found.
#'
#' @rdname edge_pairs
#'
#' @keywords internal
#'
#' @examples
#' graphicalMCP:::edge_pairs(bonferroni_holm(hypotheses = rep(1 / 3, 3)))
edge_pairs <- function(graph) {
  g_names <- names(graph$hypotheses)

  pair_indices <- graph$transitions > 0 & t(graph$transitions) > 0

  pair_nums <- which(pair_indices, arr.ind = TRUE, useNames = FALSE)

  if (nrow(pair_nums) > 0) {
    apply(
      pair_nums,
      1,
      function(row) paste(g_names[[row[[1]]]], g_names[[row[[2]]]], sep = "|"),
      simplify = FALSE
    )
  } else {
    NULL
  }
}
