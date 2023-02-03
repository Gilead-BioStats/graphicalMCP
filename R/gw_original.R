# This is a version of generate_weights_recursive() that is based on
# update_graph() and is as simple as possible. It is rather slow, but may be
# useful for testing, since it will be easier to make sure it is correct
#' @export
#' @rdname generate_weights
gw_original <- function(graph) {
  n <- length(graph$hypotheses)
  ps_indices <- rev(expand.grid(rep(list(1:0), n))[-2^n, ])

  weights <- apply(
    ps_indices,
    1,
    function(h) update_graph(graph, h)$updated_graph$hypotheses,
    simplify = FALSE
  )

  cbind(ps_indices, do.call(rbind, weights))
}

powerset <- function(s) {
  l <- vector("list", 2^length(s))
  counter <- 1

  for (x in seq_along(s)) {
    for (subset in seq_len(counter)) {
      counter <- counter + 1L
      l[[counter]] <- c(l[[subset]], s[x])
    }
  }

  l[-1]
}
