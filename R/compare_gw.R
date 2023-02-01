# Sort the results of generate_weights() the way gMCP::generateWeights
# sorts them. Then set the row & columns names equal and compare
# Currently not needed, as generate_weights() just sorts the reverse of gMCP
compare_gw <- function(gw, gw_gmcp) {
  h_vecs <- data.frame(gw[, seq_len(ncol(gw) / 2)])

  h_vecs$sort_order <- apply(h_vecs, 1, paste0, collapse = "")

  gw_sorted <- gw[order(h_vecs$sort_order), ]

  dimnames(gw_sorted) <- dimnames(gw_gmcp)

  isTRUE(all.equal(gw_sorted, gw_gmcp))
}
