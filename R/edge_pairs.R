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
