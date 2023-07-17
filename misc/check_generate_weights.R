library(Rcpp)
sourceCpp("C:/Users/dxi1/OneDrive - Gilead Sciences/Initiative/graphicalMCP/calcWeight.cpp")

############################ Rcpp #########################################
generateWeights2 <- function (w, g) {
  n <- length(w)
  intersect <- expand.grid(rep(list(0:1), n))[-1, ]
  weighting <- apply(intersect, 1, function(x) {
    list(int = x, w = calcWeightCpp(w, g, x)[[1]])
  })
  m <- as.matrix(as.data.frame(lapply(weighting, function(i) c(i$int, i$w))))
  colnames(m) <- NULL
  t(m)
}

############################ Check #############################################
library(microbenchmark)
library(gMCP)

set.seed(1234)
# Randomly generate a graph
m <- 10
w <- sample(1:m, replace = T)
w <- w / sum(w)
g <- replicate(m, sample(1:m, replace = T), simplify = T)
diag(g) <- 0
g <- g / rowSums(g)
graph <- new("graphMCP", m = g, weights = w)
graph2 <- create_graph(w, g)
sum(abs(generateWeights(graph) - generate_weights(graph2)))
# [1] 9.57498e-14
index <- generateWeights2(w, g)[, 1:m]
index$sort_order <- apply(index, 1, paste0, collapse = "")
weighting <- generateWeights2(w, g)[order(index$sort_order), ]
sum(abs(generateWeights(graph) - weighting))
# [1] 9.57498e-14
microbenchmark(generateWeights(graph), generate_weights(graph2), generateWeights2(w, g), times = 100)
# Unit: milliseconds
# expr      min        lq      mean    median        uq      max neval cld
# generateWeights(graph) 271.3074 351.05725 381.27900 366.52960 405.13855 573.3192   100   c
# generate_weights(graph2)  19.6396  25.02335  27.43096  26.45735  30.01245  44.6063   100 a
# generateWeights2(w, g)  46.9391  61.18770  66.57833  65.35435  70.96360 178.6717   100  b

