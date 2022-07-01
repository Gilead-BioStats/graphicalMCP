# Generate index matrix for all intersection hypotheses
permutations <- function(n) {
  outer((1:(2^n))-1, (n:1)-1, FUN=function(x,y) {(x%/%2^y)%%2})
}
# Function to calculate local weights
calcWeight <- function(w, g, h){
  if (sum(h == 0) > 0) {
    for (i in 1:sum(h == 0)) {
      rej <- which(h == 0)[i]
      g1 <- array(0, dim = c(length(w), length(w)))
      for (j in 1:length(w)){
        w[j] <- w[j] + w[rej] * g[rej, j]
        if (g[j, rej] * g[rej, j] < 1) {
          for (k in 1:length(w)){
            g1[j, k] = (g[j, k] + g[j, rej] * g[rej, k]) / (1 - g[j, rej] * g[rej, j])
          }
        }
        g1[j, j] <- 0
      }
      w[rej] <- 0
      g = g1
      g[rej, ] = 0
      g[, rej] = 0
    }
  }
  return(list(w, g))
}
# Example
weights <- c(0.4, 0.3, 0.2, 0.1)
names(weights) <- c("H1", "H2", "H3", "H4")
n <- length(weights)
m <- matrix(c(0, 0.4, 0.3, 0.3,
              0.8, 0, 0.1, 0.1,
              0.2, 0.4, 0, 0.4,
              0.5, 0.3, 0.2, 0),
            nrow = 4, byrow = TRUE)

indexMat <- permutations(n)[-1,]
colnames(indexMat) <- names(weights)
row.names(m) <- names(weights)

# Matrix for local weights
weightMat <- indexMat
for(i in 1:dim(indexMat)[1]){
  weightMat[i, ] <- calcWeight(w = weights, g = m, h = indexMat[i, ])[[1]]
}

# Check against generateWeights
library(gMCP)
sum(abs(indexMat - generateWeights(w = weights, g = m)[, 1:n]))
sum(abs(weightMat - generateWeights(w = weights, g = m)[, -(1:n)]))
