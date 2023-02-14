find_blocks <- function(cr) {
  n_hyps <- seq_len(nrow(cr))
  names(n_hyps) <- n_hyps
  dimnames(cr) <- list(n_hyps, n_hyps)

  indices <- powerset(n_hyps)

  blocks <- lapply(indices, \(h) cr[h, h, drop = FALSE])

  cols <- do.call(rbind, lapply(indices, \(h) n_hyps %in% h))

  colnames(cols) <- n_hyps

  df <- data.frame(
    cols,
    block_size = rowSums(cols),
    has_na = do.call(rbind, lapply(blocks, \(sub_cr) any(is.na(sub_cr))))
  )

  df_good <- df[!df$has_na,]

  df_good
}
