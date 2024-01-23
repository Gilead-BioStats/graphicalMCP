# Functions to calculate the critical value and the p-value
# for an intersection hypothesis using separate c's
# Section 4.3 equation (7)

# Input parameters
# alpha = overall significance level
# p = a vector of unadjusted p-values
# w = a vector of local weights
# cr = correlation matrix among test statistics

# make sure the current working directory is the folder code/
# now source the function definitions:
source("./perf-tests/functions/fun_miscellaneous.R")

# Function to find the separate critical value c's for all subsets
# in an intersection hypothesis
# separate_c_function <- function(w, cr, alpha){
#   require(mvtnorm)
#   I <- which(w>0)
#   subw <- w[I]
#   subcr <- cr[I, I]
#   subsets <- subset_function(subcr)
#   nsubsets <- length(subsets)
#   wP <- w
#   c <- rep(0,nsubsets)
#   for (i in 1:nsubsets){
#     ind <- subsets[[i]]
#     c[i] <- ifelse(length(ind)==1, 1,
#                    uniroot(c_function, lower = 0.9, upper = 1/min(subw),
#                            w = subw[ind], cr = subcr[ind, ind],
#                            alpha = alpha)$root)
#     wP[I[ind]] <- subw[ind] * c[i]
#   }
#   return(list(c, wP))
# }

# p-value function for an intersection hypothesis using separate c's
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
        (1 - pmvnorm(upper = qnorm(q, lower.tail = F), test_corr = subcr[ind, ind]))
    } else {
      pJ[ind] <- subp[ind] / subw[ind]
    }
  }
  return(min(pJ))
}
