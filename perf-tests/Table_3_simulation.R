# Generate simulation results for Table 3

# make sure the current working directory is the folder code/
# now source the function definitions:
source("./perf-tests/functions/fun_miscellaneous.R")
source("./perf-tests/functions/fun_separate_c.R")

# Input parameters
# alpha = overall significance level
# w = a vector of local weights
# ncp = a vector of non-centrality parameter
# rho = correlation between test statistics
# nsim = number of simulations
# Note: the processing time is above 2 hours for EACH CASE
# To reduce the processing time, reduce nsim

library(mvtnorm)
# Overall significance level
alpha <- 0.025
# Local weight
w <- c(0.4, 0.4, 0.2)
# Number of simulations
nsim <- 10^6

# Function to conduct simulation studies in Table 3
simulation <- function(alpha, w, ncp, rho, nsim){
  require(mvtnorm)
  # Correlation matrix
  cr <- rbind(c(1, rho, rho),
              c(rho, 1, rho),
              c(rho, rho, 1))
  # Generate unadjusted p-values
  data <- rmvnorm(nsim, mean = ncp, sigma = cr)
  data <- 1 - pnorm(data)

  # Adjusted p-values for (A) Bonferroni and (B) single-step parametric tests
  adj_A <- NULL
  adj_B <- NULL
  for (i in 1:nrow(data)){
    adj_A <- rbind(adj_A, pmin(data[i, ] / w, 1))
    adj_B <- rbind(adj_B, p_single_step_function(p = data[i, ], w = w, cr = cr))
  }

  # pi for individual hypotheses
  pi_A <- colMeans(adj_A <= alpha)
  pi_B <- colMeans(adj_B <= alpha)

  # pi for any hypothesis
  pi_any_A <- mean(apply(adj_A, 1, function(x) min(x) <= alpha))
  pi_any_B <- mean(apply(adj_B, 1, function(x) min(x) <= alpha))

  # A: weighted Bonferroni
  result_A <- round(100 * c(pi_A, pi_any_A),2)

  # B: weighted single-step parametric
  result_B <- round(100 * c(pi_B, pi_any_B),2)

  return(rbind(result_A,result_B))
}


# Case 1
# Non-centrality parameter
ncp <- c(3.4, 3.4, 3.4)
# Correlation
rho <- 0
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 2
# Non-centrality parameter
ncp <- c(3.4, 3.4, 3.4)
# Correlation
rho <- 0.5
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 3
# Non-centrality parameter
ncp <- c(3.4, 3.4, 3.4)
# Correlation
rho <- 0.9
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 4
# Non-centrality parameter
ncp <- c(3.4, 3.4, 0)
# Correlation
rho <- 0
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 5
# Non-centrality parameter
ncp <- c(3.4, 3.4, 0)
# Correlation
rho <- 0.5
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 6
# Non-centrality parameter
ncp <- c(3.4, 3.4, 0)
# Correlation
rho <- 0.9
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 7
# Non-centrality parameter
ncp <- c(0, 0, 0)
# Correlation
rho <- 0
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 8
# Non-centrality parameter
ncp <- c(0, 0, 0)
# Correlation
rho <- 0.5
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

# Case 9
# Non-centrality parameter
ncp <- c(0, 0, 0)
# Correlation
rho <- 0.9
# Set seed
set.seed(123456)
simulation(alpha, w, ncp, rho, nsim)

