# Functions to calculate the critical value and the p-value
# for an intersection hypothesis using common c
# Section 4.3 equation (5)

## Input parameters
# alpha = significance level
# p = a vector of unadjusted p-values
# w = a vector of local weights
# cr = correlation matrix among test statistics

# make sure the current working directory is the folder code/
# now source the function definitions:
source("./perf-tests/functions/fun_miscellaneous.R")

# Function to find the common critical value c for an intersection hypothesis
# x is to be solved
common_c_function <- function(x, w, cr, alpha){
  require(mvtnorm)
  I <- which(w>0)
  subw <- w[I]
  subcr <- cr[I, I]
  subsets <- subset_function(subcr)
  nsubsets <- length(subsets)
  y <- 0
  for (i in 1:nsubsets){
    ind <- subsets[[i]]
    z <- qnorm(pmin(x * subw[ind] * alpha, 1), lower.tail=FALSE)
    y1 <- ifelse(length(z)==1, pnorm(z, lower.tail=FALSE),
                 1-pmvnorm(upper=z, corr=subcr[ind, ind]))
    y <- y + y1
  }
  return(y - alpha * sum(w))
}

# p-value function for an intersection hypothesis using a common c
# a is the p-value to be solved
p_common_c_function <- function(a, p, w, cr){
  require(mvtnorm)
  I <- which(w > 0)
  subw <- w[I]
  subcr <- cr[I, I]
  subp <- p[I]
  subsets <- subset_function(subcr)
  nsubsets <- length(subsets)
  c_common <- ifelse(length(nsubsets) == 1 && length(subsets[[1]]) == 1, 1,
                     uniroot(common_c_function, lower = 0.1,
                             upper = 1 / min(subw),
                             w = w, cr = cr, alpha = a)$root)
  wP <- w * c_common * a
  return(min(p[wP > 0] - wP[wP > 0]))
}
