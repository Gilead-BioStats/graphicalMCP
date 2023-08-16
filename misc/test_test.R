library(gMCP)
library(lrstat)

alpha <- 0.025
###### Bonferroni
# Use the graph in the case study
set.seed(1234)
x <- 1e-4
m <- rbind(H1=c(0, 0.5, 0.25, 0.25, 0, 0),
           H2=c(0.5, 0, 0, 0, 0.25, 0.25),
           H3=c(0, 0, 0, 1, 0, 0),
           H4=c(0, x, 1 - x, 0, 0, 0),
           H5=c(x, 0, 0, 0, 0, 1 - x),
           H6=c(0, 0, 0, 0, 1, 0))
weights <- c(0.5, 0.5, 0, 0, 0, 0)
graph_gMCP <- new("graphMCP", m = m, weights = weights)
graph_graphicalMCP <- graph_create(weights, m)

n <- 1e3
pvalues <- matrix(runif(length(weights) * n, 0, alpha / 2),
                  ncol = length(weights))
adjp_gMCP <- adjp_graphicalMCP <- pvalues
for (i in 1:n) {
  temp <- pvalues[i, ]
  adjp_gMCP[i, ] <- gMCP::gMCP(graph_gMCP, temp,
                               test = "Bonferroni", alpha = alpha)@adjPValues
  adjp_graphicalMCP[i, ] <- graph_test_shortcut(graph_graphicalMCP,
                                                              temp,
                                                              alpha = alpha)$outputs$adjusted_p
}
diff <- max(abs(adjp_gMCP - adjp_graphicalMCP))
diff


###### Simes
# Figure 2 in Lu (2016)
set.seed(1234)
x <- 0.5
m <- rbind(H1=c(0, 0, 0.5, 0.5, 0, 0),
           H2=c(0, 0, 0, 0, 0.5, 0.5),
           H3=c(0, x, 0, 1 - x, 0, 0),
           H4=c(0, x, 1 - x, 0, 0, 0),
           H5=c(x, 0, 0, 0, 0, 1 - x),
           H6=c(x, 0, 0, 0, 1 - x, 0))
weights <- c(0.5, 0.5, 0, 0, 0, 0)
graph_graphicalMCP <- graph_create(weights, m)
weighting_strategy_graphicalMCP <- graph_generate_weights(graph_graphicalMCP)
weighting_strategy_lrstat <- lrstat::fwgtmat(weights, m)

test_group_lrstat <- matrix(c(1, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1),
                            nrow = 2, ncol = 6, byrow = TRUE)
test_group_graphicalMCP <- list(c(1, 2, 3, 5), c(4, 6))

n <- 1e3
pvalues <- matrix(runif(length(weights) * n, 0, alpha / 2),
                  ncol = length(weights))
adjp_lrstat <- adjp_graphicalMCP <- pvalues

for (i in 1:n) {
  temp <- pvalues[i, ]
  adjp_lrstat[i, ] <- lrstat::fadjpsim(weighting_strategy_lrstat, temp, test_group_lrstat)
  adjp_graphicalMCP[i, ] <- graph_test_closure(graph_graphicalMCP, temp,
                                               test_types = c("s", "s"),
                                               groups = test_group_graphicalMCP,
                                               alpha = alpha)$outputs$adjusted_p
}
diff <- max(abs(adjp_lrstat - adjp_graphicalMCP))
diff

###### Parametric
# Figure 1 in Xi et al. (2017)
subset_function <- function(x){
  subset <- !is.na(x)
  id <- !duplicated(subset)
  subsets <- vector("list", sum(id))
  for (i in 1:sum(id)){
    if (!is.null(nrow(x))){
      subsets[[i]] <- (1:nrow(x))[which(subset[which(id)[i], ])]
    } else {
      subsets[[i]] <- 1
    }
  }
  return(subsets)
}
p_separate_c_function <- function(p, w, cr){
  require(mvtnorm)
  I <- which(w > 0)
  subw <- w[I]
  subcr <- cr[I, I]
  subp <- p[I]
  subsets <- subset_function(subcr)
  nsubsets <- length(subsets)
  pJ <- subp
  for (i in 1:nsubsets){
    ind <- subsets[[i]]
    if (length(ind) != 1){
      q <- min(subp[ind] / subw[ind])
      q <- q * subw[ind]
      pJ[ind] <- 1 / sum(subw[ind]) *
        (1 - pmvnorm(upper = qnorm(q, lower.tail = F), corr = subcr[ind, ind], algorithm = GenzBretz(abseps = 1e-6)))
    } else {
      pJ[ind] <- subp[ind] / subw[ind]
    }
  }
  return(min(pJ))
}

set.seed(1234)
m <- rbind(H1=c(0, 0, 0, 1, 0, 0),
           H2=c(0, 0, 0, 0, 1, 0),
           H3=c(0, 0, 0, 0, 0, 1),
           H4=c(0, 0.5, 0.5, 0, 0, 0),
           H5=c(0.5, 0, 0.5, 0, 0, 0),
           H6=c(0.5, 0.5, 0, 0, 0, 0))
weights <- c(0.4, 0.4, 0.2, 0, 0, 0)
graph_graphicalMCP <- graph_create(weights, m)
weighting_strategy_graphicalMCP <- graph_generate_weights(graph_graphicalMCP)

test_group_graphicalMCP <- list(c(1, 2, 3), c(4, 5, 6))
cr <- matrix(c(1, 0.5, 0.5, NA, NA, NA,
               0.5, 1, 0.5, NA, NA, NA,
               0.5, 0.5, 1, NA, NA, NA,
               NA, NA, NA, 1, NA, NA,
               NA, NA, NA, NA, 1, NA,
               NA, NA, NA, NA, NA, 1),
             nrow = length(weights))

n <- 1e3
pvalues <- matrix(runif(length(weights) * n, 0, alpha / 2),
                  ncol = length(weights))
adjp <- adjp_graphicalMCP <- pvalues

for (i in 1:n) {
  temp <- pvalues[i, ]
  p_intersection <- rep(NA, nrow(weighting_strategy_graphicalMCP))
  for (j in 1:nrow(weighting_strategy_graphicalMCP)){
    p_intersection[j] <- p_separate_c_function(temp, weighting_strategy_graphicalMCP[j, -c(1:length(weights))], cr)
  }
  adjp_C <- rep(NA, length(weights))
  for (j in 1:length(weights)){
    adjp_C[j] <- max(p_intersection[weighting_strategy_graphicalMCP[, j] > 0])
  }
  adjp[i, ] <- adjp_C
  adjp_graphicalMCP[i, ] <- graph_test_closure(graph_graphicalMCP, temp,
                                               test_types = c("p", "b"),
                                               corr = cr,
                                               groups = test_group_graphicalMCP,
                                               alpha = alpha)$outputs$adjusted_p
}
diff <- max(abs(adjp - adjp_graphicalMCP))
diff
