# Calculate weight for index h
# Less time than gMCP::mtp.weights and gMCP:mtp.edges
# It would be great to further improve efficiency
calcWeight <- function(w, g, h){
  if (sum(h == 0) > 0) {
    for (i in 1:sum(h == 0)) {
      rej <- which(h == 0)[i]
      g1 <- array(0, dim = c(length(w), length(w)))
      for (j in 1:length(w)){
        w[j] <- w[j] + w[rej] * g[rej, j]
        g1[j, ] <- (g[j, ] + g[j, rej] * g[rej, ]) / (1 - g[j, rej] * g[rej, j])
        g1[j, j] <- 0
        g1[j, is.nan(g1[j, ])] <- 0
      }
      w[rej] <- 0
      g <- g1
      g[rej, ] <- 0
      g[, rej] <- 0
    }
  }
  return(list(w, g))
}

# Vectorized
calcWeight <- function (h, g, w)
{
  m <- length(h)
  index <- 1L:m
  Jc <- index[h == 0]
  for (j in Jc) {
    index <- index[index != j]
    Jc <- Jc[Jc != j]
    w[index] <- w[index] + w[j] * g[j, index]
    w[j] <- 0
    gg1 <- g[, j] %o% g[j, ]
    gg2 <- (g[, j] * g[j, ]) %o% rep(1L, m)
    g1 <- (g + gg1)/(1 - gg2)
    g1[j, ] <- 0
    g1[, j] <- 0
    diag(g1) <- 0
    g1[gg2 >= 1] <- 0
    g <- g1
  }
  return(w)
}

delete_nodes_vec <- function (h, g, w)
{
  m <- length(h)
  index <- 1L:m
  Jc <- index[h == 0]
  for (j in Jc) {
    index <- index[index != j]
    Jc <- Jc[Jc != j]
    w[index] <- w[index] + w[j] * g[j, index]
    w[j] <- 0
    gg1 <- g[, j] %o% g[j, ]
    gg2 <- (g[, j] * g[j, ]) %o% rep(1L, m)
    g1 <- (g + gg1)/(1 - gg2)
    g1[j, ] <- 0
    g1[, j] <- 0
    diag(g1) <- 0
    g1[gg2 >= 1] <- 0
    g <- g1
  }
  return(structure(list(hypotheses = w, transitions = g), class = "initial_graph"))
}
