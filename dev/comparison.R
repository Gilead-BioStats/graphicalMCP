library(Rcpp)
setwd("C:/Users/dxi1/OneDrive - Gilead Sciences/Initiative/graphicalMCP")
sourceCpp("calcWeight.cpp")

############################ From gMCP #########################################
permutations <- function(n) {
  outer((1:(2^n)) - 1, (n:1) - 1, FUN = function(x,y) {(x %/% 2^y) %% 2})
}

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

generateWeights <- function (g, w) {
  if ("entangledMCP" %in% class(g)) {
    mL <- getMatrices(g)
    wL <- getWeights(g)
    split <- g@weights
    result <- 0
    for (i in 1:length(mL)) {
      m <- mL[[i]]
      w <- wL[i, ]
      result <- result + split[i] * generateWeights(m, 
                                                    w)
    }
    n <- dim(m)[1]
    result[, 1:n][result[, 1:n] > 0] <- 1
    return(result)
  }
  else if ("graphMCP" %in% class(g)) {
    if (missing(w)) {
      w <- getWeights(g)
    }
    g <- getMatrix(g)
  }
  n <- length(w)
  intersect <- (permutations(n))[-1, ]
  g <- apply(intersect, 1, function(i) list(int = i, w = mtp.weights(i, 
                                                                     g, w)))
  m <- as.matrix(as.data.frame(lapply(g, function(i) c(i$int, 
                                                       i$w))))
  colnames(m) <- NULL
  t(m)
}

############################ graphicalMCP ######################################
# Modified from sssheridan at https://stackoverflow.com/q/18715580
# Less time compared to gMCP::permutations
permutations2 <- function(n){
  l <- vector(mode = "list", length = 2^n)
  l[[1]] <- numeric()
  counter = 1
  for(x in n:1){
    for(subset in 1:counter){
      counter <- counter + 1
      l[[counter]] <- c(l[[subset]], x)
    }
  }
  return(l)
}

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

# Less time than gMCP::generateWeights
generateWeights2 <- function (g, w) {
  if ("entangledMCP" %in% class(g)) {
    mL <- getMatrices(g)
    wL <- getWeights(g)
    split <- g@weights
    result <- 0
    for (i in 1:length(mL)) {
      m <- mL[[i]]
      w <- wL[i, ]
      result <- result + split[i] * generateWeights2(m, w)
    }
    n <- dim(m)[1]
    result[, 1:n][result[, 1:n] > 0] <- 1
    return(result)
  }
  else if ("graphMCP" %in% class(g)) {
    if (missing(w)) {
      w <- getWeights(g)
    }
    g <- getMatrix(g)
  }
  n <- length(w)
  intersect <- permutations2(n)[-1]
  g <- lapply(intersect, function(i) {
    temp <- rep(0, n)
    temp[i] <- 1
    list(int = temp, w = calcWeight(h = temp, g = g, w = w)[[1]])
  })
  m <- as.matrix(as.data.frame(lapply(g, function(i) c(i$int, i$w))))
  colnames(m) <- NULL
  t(m)
}

# Less time than gMCP::generateWeights
generateWeights3 <- function (g, w) {
  if ("entangledMCP" %in% class(g)) {
    mL <- getMatrices(g)
    wL <- getWeights(g)
    split <- g@weights
    result <- 0
    for (i in 1:length(mL)) {
      m <- mL[[i]]
      w <- wL[i, ]
      result <- result + split[i] * generateWeights3(m, w)
    }
    n <- dim(m)[1]
    result[, 1:n][result[, 1:n] > 0] <- 1
    return(result)
  }
  else if ("graphMCP" %in% class(g)) {
    if (missing(w)) {
      w <- getWeights(g)
    }
    g <- getMatrix(g)
  }
  n <- length(w)
  intersect <- permutations2(n)[-1]
  g <- lapply(intersect, function(i) {
    temp <- rep(0, n)
    temp[i] <- 1
    list(int = temp, w = calcWeightCpp(w, g, temp)[[1]])
  })
  m <- as.matrix(as.data.frame(lapply(g, function(i) c(i$int, i$w))))
  colnames(m) <- NULL
  t(m)
}

############################ Check #############################################
library(microbenchmark)
library(gMCP)

set.seed(1234)
# Randomly generate a graph
m <- 5
w <- sample(1:m, replace = T)
w <- w / sum(w)
g <- replicate(m, sample(1:m, replace = T), simplify = T)
diag(g) <- 0
g <- g / rowSums(g)
graph <- new("graphMCP", m = g, weights = w)
sum(abs(generateWeights(graph) - generateWeights2(graph)))
sum(abs(generateWeights(graph) - generateWeights3(graph)))
# [1] 1.69309e-15
microbenchmark(generateWeights(graph), generateWeights2(graph), generateWeights3(graph), times = 100)
# Unit: milliseconds
# expr                        min       lq     mean   median       uq       max neval cld
# generateWeights(graph)  2.908101 3.210101 3.719067 3.412351 3.686251 16.046000   100   b
# generateWeights2(graph) 2.914300 3.189450 3.625144 3.468351 3.773151  8.375101   100   b
# generateWeights3(graph) 1.609700 1.787402 2.303689 1.889050 2.083000 30.799901   100  a 

set.seed(1234)
# Randomly generate a graph
m <- 10
w <- sample(1:m, replace = T)
w <- w / sum(w)
g <- replicate(m, sample(1:m, replace = T), simplify = T)
diag(g) <- 0
g <- g / rowSums(g)
graph <- new("graphMCP", m = g, weights = w)
sum(abs(generateWeights(graph) - generateWeights2(graph)))
sum(abs(generateWeights(graph) - generateWeights3(graph)))
# [1] 1.02425e-13
microbenchmark(generateWeights(graph), generateWeights2(graph), generateWeights3(graph), times = 100)
# Unit: milliseconds
# expr                        min       lq      mean    median       uq      max neval cld
# generateWeights(graph)  280.8984 367.1443 406.19562 385.40235 434.7857 656.3710   100   c
# generateWeights2(graph) 233.4635 297.0807 320.86889 313.84490 342.8356 450.2896   100  b 
# generateWeights3(graph)  69.5846  88.3271  94.87051  92.06235 100.3919 132.3285   100 a  



sim <- function(x, scen) {
  seed <- scen$seed[x]
  m <- scen$m[x]
  set.seed(seed)
  w <- sample(1:m, replace = T)
  w <- w / sum(w)
  g <- replicate(m, sample(1:m, replace = T), simplify = T)
  diag(g) <- 0
  g <- g / rowSums(g)
  graph <- new("graphMCP", m = g, weights = w)
  start <- proc.time()
  res1 <- generateWeights(graph)
  time1 <- (proc.time() - start)[3]
  # start <- proc.time()
  # res2 <- generateWeights2(graph)
  # time2 <- (proc.time() - start)[3]
  start <- proc.time()
  res3 <- generateWeights3(graph)
  time3 <- (proc.time() - start)[3]
  # diff2 <- sum(abs(res1 - res2))
  diff3 <- sum(abs(res1 - res3))
  # out <- c(m, seed, time1, time2, time3, (time1 - time2) / time1, (time1 - time3) / time1, diff2, diff3)
  # names(out) <- c("m", "seed", "time1", "time2", "time3", "reduction2", "reduction3", "diff2", "diff3")
  out <- c(m, seed, time1, time3, (time1 - time3) / time1, diff3)
  names(out) <- c("m", "seed", "time1", "time3", "reduction3", "diff3")
  return(out)
}

seed <- 1:100
m <- 10
scen <- expand.grid(seed, m)
colnames(scen) <- c("seed", "m")
rownames(scen) <- NULL

out <- NULL
for (i in 1:length(seed)) {
  out <- rbind(out, sim(i, scen))
}
out <- as.data.frame(out)
max(out$diff3)
# [1] 1.168753e-13
summary(out$reduction3)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6522  0.7237  0.7643  0.7652  0.8056  0.8727 

seed <- 1:100
m <- 15
scen <- expand.grid(seed, m)
colnames(scen) <- c("seed", "m")
rownames(scen) <- NULL

out <- NULL
for (i in 1:length(seed)) {
  out <- rbind(out, sim(i, scen))
}
out <- as.data.frame(out)
max(out$diff3)
# [1] 4.05726e-12
summary(out$reduction3)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6857  0.8283  0.8332  0.8309  0.8387  0.8981 



