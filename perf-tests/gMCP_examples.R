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


hypotheses <- c(ni_lo = 0.5, ni_hi = 0.5, su_lo = 0, su_hi = 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- create_graph(hypotheses, transitions)
G <- matrix2graph(transitions, hypotheses)

p_vals <- c(.001, .02, .05, .1)
corr1 <- rbind(
  c(1, NA, NA, NA),
  c(NA, 1, NA, NA),
  c(NA, NA, 1, NA),
  c(NA, NA, NA, 1)
)
corr2 <- rbind(
  c(1, .5, NA, NA),
  c(.5, 1, NA, NA),
  c(NA, NA, 1, .5),
  c(NA, NA, .5, 1)
)
corr2_ <- rbind(
  c(1, .5, 0, 0),
  c(.5, 1, 0, 0),
  c(0, 0, 1, .5),
  c(0, 0, .5, 1)
)
dimnames(corr1) <- dimnames(corr2) <- list(names(hypotheses), names(hypotheses))
alpha <- .025

p <- 1-pnorm(c(2.24,2.24,2.24,2.3))

gMCP(G, p)
test_graph(g, p)

gMCP(G, p, corr = corr2, test = "parametric")
test_graph(g, p, corr = corr2_, alpha = .05,
           tests = list(parametric = list(1:4)))
