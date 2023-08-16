i <- 1

while (TRUE) {
  cat(i, "\n")
  p_sim <- stats::pnorm(
    mvtnorm::rmvnorm(1e6, rep(0, 15), sigma = matrix(1, 15, 15)),
    lower.tail = FALSE
  )

  if (length(p_sim) != length(unique(p_sim))) {
    stop("found a duplicate")
  }
  i <- i + 1

}
