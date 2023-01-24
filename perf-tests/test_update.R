

mtp.weights <- function(h,g,w){
  ## recursively compute weights for a given graph and intersection hypothesis
  if(sum(h)==length(h)){
    return(w)
  } else {
    j <- which(h==0)[1]
    h[j] <- 1
    wu <- mtp.weights(h,g,w)
    gu <- mtp.edges(h,g,w)
    guj <- gu[j,]
    wt <- wu+wu[j]*guj
    wt[j] <- 0
    return(wt)
  }
}

mtp.edges <- function(h,g,w){
  ## recursively compute the edges for the graph of a given intersection hypothesis
  if(sum(h)==length(h)){
    return(g)
  } else {
    j <- which(h==0)[1]
    h[j] <- 1
    gu <- mtp.edges(h,g,w)
    gj <- gu[,j]%*%t(gu[j,])
    gt <- ((gu+gj)/(1-matrix(rep(diag(gj),nrow(gj)),nrow=nrow(gj))))
    gt[j,] <- 0
    gt[,j] <- 0
    diag(gt) <- 0
    gt[is.nan(gt)] <- 0
    return(gt)
  }
}

set.seed(1234)
m <- 10
nsim <- 1e3
diff_weight <- diff_transition <- NULL

for (i in 1:nsim) {
  w <- sample(1:m, replace = T)
  w <- w / sum(w)
  g <- replicate(m, sample(1:m, replace = T), simplify = T)
  diag(g) <- 0
  g <- g / rowSums(g)
  h <- sample(c(0, 1), m, replace = T)

  # gMCP
  gmcp_weight <- mtp.weights(h,g,w)
  gmcp_transition <- mtp.edges(h,g,w)

  # graphicalMCP
  graph <- create_graph(w, g)
  graphicalmcp_weight <- update_graph(graph, h)$updated_graph$hypotheses
  graphicalmcp_transition <- update_graph(graph, h)$updated_graph$transitions

  diff_weight <- c(diff_weight, max(abs(gmcp_weight - graphicalmcp_weight)))
  diff_transition <- c(diff_transition, max(abs(gmcp_transition - graphicalmcp_transition)))
}
all.equal(0, max(diff_weight))
all.equal(0, max(diff_transition))
