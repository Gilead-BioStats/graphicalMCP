sort_gw_like_gmcp <- function(weights_matrix) {
  h_vecs <- data.frame(weights_matrix[, seq_len(ncol(weights_matrix) / 2)])

  h_vecs$sort_order <- apply(h_vecs, 1, paste0, collapse = "")

  weights_matrix[order(h_vecs$sort_order), ]
}

compare_gw <- function(gw, gw_gmcp) {
  gw_sorted <- sort_gw_like_gmcp(gw)

  dimnames(gw_sorted) <- dimnames(gw_gmcp)

  all.equal(gw_sorted, gw_gmcp)
}

# Usage
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
# gw4 <- generate_weights(g)
#
# gw4_gmcp <- gMCP::generateWeights(g$transitions, g$hypotheses)
#
# compare_gw(gw4, gw4_gmcp)
# #> TRUE
