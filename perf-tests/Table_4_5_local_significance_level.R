# Generate local significance levels for Table 4 and 5

# make sure the current working directory is the folder code/
# now source the function definitions:
source("functions/fun_miscellaneous.R")
source("functions/fun_separate_c.R")
source("functions/fun_common_c.R")

# Input parameters
# alpha = overall significance level
# w = a vector of local weights
# g = a matrix of transition weights
# cr = correlation matrix among test statistics

# Table 4 and 5
# Note the same set of information is allocated to Table 4 and 5
# for limited space
library(mvtnorm)

# Overall significance level
alpha <- 0.025
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
# set seed
set.seed(123456)

# (A) Bonferroni test
level_A <- alpha * weight
level_A <- round(level_A * 100, 2)
cbind(index, level_A)

# (B) parametric test using a common c as in equation (5)
# Calculate local significance levels for every intersection hypothesis
c_intersection <- rep(NA, nrow(weight))
for (i in 1:nrow(weight)){
  ww <- weight[i, ]
  c_intersection[i] <- uniroot(common_c_function, lower = 0.9,
                               upper = 1 / min(ww[ww > 0]),
                               w = ww, cr = cr, alpha = alpha)$root
}
level_B <- alpha * c_intersection * weight
level_B <- round(level_B * 100, 2)
cbind(index, level_B)

## (C) parametric test using separate c's as in equation (7)
# Calculate local significance levels for every intersection hypothesis
local_weight_intersection <- matrix(NA, ncol = ncol(weight),
                                    nrow = nrow(weight))
for (i in 1:nrow(weight)){
  local_weight_intersection[i, ] <- separate_c_function(w = weight[i, ],
                                                        cr = cr,
                                                        alpha = alpha)[[2]]
}
level_C <- alpha * local_weight_intersection
level_C <- round(level_C * 100, 2)
cbind(index, level_C)

