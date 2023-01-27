# Generate adjusted p-values for Table 2

# make sure the current working directory is the folder code/
# now source the function definitions:
source("./perf-tests/functions/fun_miscellaneous.R")
source("./perf-tests/functions/fun_separate_c.R")
source("./perf-tests/functions/fun_common_c.R")

# Table 2
library(mvtnorm)

# Generate the weighting scheme
w <- c(0.4, 0.4, 0.2, 0,  0, 0)
g <- rbind(c(0, 0, 0, 1, 0, 0),
           c(0, 0, 0, 0, 1, 0),
           c(0, 0, 0, 0, 0, 1),
           c(0, 0.5, 0.5, 0, 0, 0),
           c(0.5, 0, 0.5, 0, 0, 0),
           c(0.5, 0.5, 0, 0, 0, 0))
index <- generateWeights(w = w, g = g)[, 1:length(w)]
weight <- generateWeights(w = w, g = g)[, (length(w) + 1):(2 * length(w))]
# Correlation matrix
cr <- matrix(c(1, 0.5, 0.5, NA, NA, NA,
               0.5, 1, 0.5, NA, NA, NA,
               0.5, 0.5, 1, NA, NA, NA,
               NA, NA, NA, 1, NA, NA,
               NA, NA, NA, NA, 1, NA,
               NA, NA, NA, NA, NA, 1),
             nrow = length(w))
# Unadjusted p-values
p <- c(0.009, 0.011, 0.009, 0.013, 0.016, 0.004)
# Set seed
set.seed(123456)

# (A) Bonferroni test
# The correlation matrix has NA for all off-diagonal entries
cr_bonf <- matrix(rep(NA, length(w) * length(w)), nrow = length(w))
diag(cr_bonf) <- 1
# Calculate p-value for every intersection hypothesis
p_intersection <- rep(NA, nrow(weight))
for (i in 1:nrow(weight)){
  p_intersection[i] <- p_separate_c_function(p, weight[i, ], cr_bonf)
}
# Adjusted p-values for (A) Bonferroni test
adjp_A <- rep(NA, length(w))
for (i in 1:length(w)){
  adjp_A[i] <- max(p_intersection[index[, i] > 0])
}
ceiling(adjp_A * 10000) / 100

## (B) parametric test using a common c as in equation (5)
# Calculate p-value for every intersection hypothesis
p_intersection <- rep(NA, nrow(weight))
for (i in 1:nrow(weight)){
  p_intersection[i] <- uniroot(p_common_c_function, lower = 0.0001,
                               upper = 0.2,
                               p = p, w = weight[i, ], cr = cr)$root
}
# Adjusted p-values for (B) parametric test using equation (5)
adjp_B <- rep(NA, length(w))
for (i in 1:length(w)){
  adjp_B[i] <- max(p_intersection[index[, i] > 0])
}
ceiling(adjp_B * 10000) / 100


## (C) parametric test using separate c's as in equation (7)
# Calculate p-value for every intersection hypothesis
p_intersection <- rep(NA, nrow(weight))
for (i in 1:nrow(weight)){
  p_intersection[i] <- p_separate_c_function(p, weight[i, ], cr)
}
# Adjusted p-values for (B) parametric test using equation (5)
adjp_C <- rep(NA, length(w))
for (i in 1:length(w)){
  adjp_C[i] <- max(p_intersection[index[, i] > 0])
}
ceiling(adjp_C * 10000) / 100


