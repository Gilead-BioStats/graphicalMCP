wgt_delete_nodes <- function(h, w, g) {
  graph <- list(w = w, g = g)

  I <- 1:length(w)
  J <- which(as.logical(1 - h))

  for (j in J) {
    graph <- wgt_delete_node(j, w, g, I)
    w <- graph[["w"]]
    g <- graph[["g"]]
    I <- I[I != j]
  }

  w
}
