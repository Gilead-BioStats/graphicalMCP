# Example with testing from vignette("gMCP")
#
# Estimates:
est <- c("H1" = 0.860382, "H2" = 0.9161474, "H3" = 0.9732953)
# Sample standard deviations:
ssd <- c("H1" = 0.8759528, "H2" = 1.291310, "H3" = 0.8570892)
pval <- c(0.01260, 0.05154, 0.02124) / 2
simConfint(
  BonferroniHolm(3),
  pvalues = pval,
  confint = function(node, alpha) {
    c(est[node] - qt(1 - alpha, df = 9) * ssd[node] / sqrt(10), Inf)
  },
  estimates = est,
  alpha = 0.025,
  mu = 0,
  alternative = "greater"
)
## lower bound estimate upper bound
## H1 0.0000 0.8604 Inf
## H2 -0.0076 0.9161 Inf
## H3 0.0000 0.9733 Inf
# Note that the sample standard deviations in the following call
# will be calculated from the pvalues and estimates.
simConfint(
  BonferroniHolm(3),
  pvalues = pval,
  confint = "t",
  df = 9,
  estimates = est,
  alpha = 0.025,
  alternative = "greater"
)
## lower bound estimate upper bound
## [1,] 0.000000 0.8604 Inf
## [2,] -0.007581 0.9161 Inf
## [3,] 0.000000 0.9733 Inf
