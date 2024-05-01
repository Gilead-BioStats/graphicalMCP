#' Find pairs of vertices that are connected in both directions
#'
#' @param graph An initial graph as returned by [graph_create()].
#'
#' @return A list of vertex pairs which are connected in both directions. NULL
#'   if no such pairs are found
#'
#' @keywords internal
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
