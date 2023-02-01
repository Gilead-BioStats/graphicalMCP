# This is a version of generate_weights_recursive() that is based on
# update_graph() and is as simple as possible. It is rather slow, but may be
# useful for testing, since it will be easier to make sure it is correct
gw_original <- function(graph) {
  ps <- powerset(seq_along(graph$hypotheses))
  ps_indices <- lapply(
    ps,
    function(h) !is.na(h[seq_along(graph$hypotheses)])
  )

  weights <- apply(
    ps_indices,
    1,
    function(h) update_graph(graph, h)$updated_graph$hypotheses
  )

  cbind(do.call(rbind, ps_indices), do.call(rbind, weights))
}

powerset <- function(s) {
  l <- vector("list", 2 ^ length(s))
  counter <- 1

  for (x in seq_along(s)) {
    for (subset in seq_len(counter)) {
      counter <- counter + 1L
      l[[counter]] <- c(l[[subset]], s[x])
    }
  }

  l[-1]
}
