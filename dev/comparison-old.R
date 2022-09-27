############################ From gMCP #########################################
permutations <- function(n) {
    outer((1:(2^n)) - 1, (n:1) - 1, FUN = function(x, y) {
        (x %/% 2^y) %% 2
    })
}

mtp.weights <- function(h, g, w) {
    ## recursively compute weights for a given graph and intersection hypothesis
    if (sum(h) == length(h)) {
        return(w)
    } else {
        j <- which(h == 0)[1]
        h[j] <- 1
        wu <- mtp.weights(h, g, w)
        gu <- mtp.edges(h, g, w)
        guj <- gu[j, ]
        wt <- wu + wu[j] * guj
        wt[j] <- 0
        return(wt)
    }
}

mtp.edges <- function(h, g, w) {
    ## recursively compute the edges for the graph of a given intersection hypothesis
    if (sum(h) == length(h)) {
        return(g)
    } else {
        j <- which(h == 0)[1]
        h[j] <- 1
        gu <- mtp.edges(h, g, w)
        gj <- gu[, j] %*% t(gu[j, ])
        gt <- ((gu + gj) / (1 - matrix(rep(diag(gj), nrow(gj)), nrow = nrow(gj))))
        gt[j, ] <- 0
        gt[, j] <- 0
        diag(gt) <- 0
        gt[is.nan(gt)] <- 0
        return(gt)
    }
}

generateWeights <- function(g, w) {
    if ("entangledMCP" %in% class(g)) {
        mL <- getMatrices(g)
        wL <- getWeights(g)
        split <- g@weights
        result <- 0
        for (i in 1:length(mL)) {
            m <- mL[[i]]
            w <- wL[i, ]
            result <- result + split[i] * generateWeights(
                m,
                w
            )
        }
        n <- dim(m)[1]
        result[, 1:n][result[, 1:n] > 0] <- 1
        return(result)
    } else if ("graphMCP" %in% class(g)) {
        if (missing(w)) {
            w <- getWeights(g)
        }
        g <- getMatrix(g)
    }
    n <- length(w)
    intersect <- (permutations(n))[-1, ]
    g <- apply(intersect, 1, function(i) {
        list(int = i, w = mtp.weights(
            i,
            g, w
        ))
    })
    m <- as.matrix(as.data.frame(lapply(g, function(i) {
        c(
            i$int,
            i$w
        )
    })))
    colnames(m) <- NULL
    t(m)
}

############################ graphicalMCP ######################################
# Modified from sssheridan at https://stackoverflow.com/q/18715580
# Less time compared to gMCP::permutations
permutations2 <- function(n) {
    l <- vector(mode = "list", length = 2^n)
    l[[1]] <- numeric()
    counter <- 1
    for (x in n:1) {
        for (subset in 1:counter) {
            counter <- counter + 1
            l[[counter]] <- c(l[[subset]], x)
        }
    }
    return(l)
}

# Calculate weight for index h
# Less time than gMCP::mtp.weights and gMCP:mtp.edges
# It would be great to further improve efficiency
calcWeight <- function(w, g, h) {
    if (sum(h == 0) > 0) {
        for (i in 1:sum(h == 0)) {
            rej <- which(h == 0)[i]
            g1 <- array(0, dim = c(length(w), length(w)))
            for (j in 1:length(w)) {
                w[j] <- w[j] + w[rej] * g[rej, j]
                g1[j, ] <- (g[j, ] + g[j, rej] * g[rej, ]) / (1 - g[j, rej] * g[rej, j])
                g1[j, j] <- 0
                g1[is.nan(g1)] <- 0
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
generateWeights2 <- function(g, w) {
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
    } else if ("graphMCP" %in% class(g)) {
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
        list(int = temp, w = calcWeight2(h = temp, g = g, w = w)[[1]])
    })
    m <- as.matrix(as.data.frame(lapply(g, function(i) c(i$int, i$w))))
    colnames(m) <- NULL
    t(m)
}


############################ Check #############################################
require("microbenchmark")

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
# [1] 1.69309e-15
microbenchmark(generateWeights(graph), generateWeights2(graph), times = 100)
# Unit: milliseconds
# expr                       min      lq     mean  median      uq    max neval cld
# generateWeights(graph)  2.8345 2.93025 3.220707 3.06055 3.30485 9.7408   100   a
# generateWeights2(graph) 2.7382 2.89195 3.227686 3.03240 3.25625 9.2995   100   a

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
# [1] 1.02425e-13
microbenchmark(generateWeights(graph), generateWeights2(graph), times = 100)
# Unit: milliseconds
# expr                      min       lq     mean   median       uq      max neval cld
# generateWeights(graph)  375.6343 412.7095 441.5188 430.9961 455.0715 703.1644   100   b
# generateWeights2(graph) 310.6881 340.1382 356.9771 352.7269 372.1873 435.8859   100  a


### More than 10 hypotheses
library(future.apply)
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
    start <- proc.time()
    res2 <- generateWeights2(graph)
    time2 <- (proc.time() - start)[3]
    diff <- sum(abs(res1 - res2))
    out <- c(m, seed, time1, time2, (time1 - time2) / time1, diff)
    names(out) <- c("m", "seed", "time1", "time2", "reduction", "diff")
    return(out)
}

seed <- 1:100
m <- 10
scen <- expand.grid(seed, m)
colnames(scen) <- c("seed", "m")
rownames(scen) <- NULL

plan(multisession)

start <- proc.time()
result <- future_lapply(1:nrow(scen),
    FUN = sim, future.seed = TRUE,
    future.packages = c("gMCP"), scen = scen
)
(proc.time() - start)[3]
data <- as.data.frame(do.call(rbind, result))
max(data$diff)
# [1] 1.099398e-13
summary(data$reduction)
# Min.    1st Qu. Median   Mean    3rd Qu.     Max.
# -0.36765 0.20262 0.13145 0.11910 0.04933  0.51754

seed <- 1:100
m <- 15
scen <- expand.grid(seed, m)
colnames(scen) <- c("seed", "m")
rownames(scen) <- NULL

plan(multisession)

start <- proc.time()
result <- future_lapply(1:nrow(scen),
    FUN = sim, future.seed = TRUE,
    future.packages = c("gMCP"), scen = scen
)
(proc.time() - start)[3]
data <- as.data.frame(do.call(rbind, result))
max(data$diff)
# [1] 4.016325e-12
summary(data$reduction)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -0.47355 -0.29740 -0.23280 -0.22865 -0.17750  0.02246
