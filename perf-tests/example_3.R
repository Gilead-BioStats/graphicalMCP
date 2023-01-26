myfct <- function(x, a, w, sig) {
  1 -
    a -
    mvtnorm::pmvnorm(lower = -Inf, upper = qnorm(1 - x * w * a), sigma = sig)
}

# 1/2 non-inferiority hypotheses (say, for low and high dose against control)
# 3/4 superiority hypotheses for the same two doses
hypotheses <- c(ni_lo = 0.5, ni_hi = 0.5, su_lo = 0, su_hi = 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- create_graph(hypotheses, transitions)

alpha <- .025

# Part 1 -----------------------------------------------------------------------

# Assume a known common variance & same population for each test & group sample
# sizes equal
# Then rho(H1, H3) = rho(H2, H4) = 1
# All other rho = .5
corr <- rbind(
  c( 1, .5,  1, .5),
  c(.5,  1, .5,  1),
  c( 1, .5,  1, .5),
  c(.5,  1, .5,  1)
)

gw <- generate_weights(g)
gw_weights <- gw[, 5:8]

# I thought this was the way gMCP calculated cJ in a doc (parametric vignette
# maybe?) But it's different than what it says in the paper. The paper says it
# should be 1.078... when J == 1/2, 1/4, 2/3, and [or?] 3/4 (All the .5 rhos).
# But it equals 1.078... when J *contains* any of those pairs according to this
# lovely little function
cJ <- vector("numeric", nrow(gw_weights))

for (i in seq_len(nrow(gw))) {
  cJ[[i]] <- uniroot(
    myfct,
    lower = 1,
    upper = 9,
    a = alpha,
    w = gw_weights[i, ],
    sig = corr
  )$root
}

cbind(gw, cJ)

# Reject H1, H3, H2
# The paper does it sequentially because consonance is satisfied, but
# `test_graph()` handles this case fine using the whole closure tree and the cJ
# calculation above
p_vals <- c(.01, .02, .005, .5)

gMCP(as_gmcp_graph(g), p_vals, alpha = alpha, correlation = corr,
     test = "parametric")@rejected
test_graph(g, p_vals, alpha = alpha, corr = corr,
           tests = list(parametric = list(1:4)))$hypotheses_rejected

# Part 2 -----------------------------------------------------------------------
# Per-protocol population (PP) for non-inferiority (H1/H2)
# Intention-to-treat (ITT) for superiority (H3/H4)
# PP subset of ITT
# ni is ITT sample size for group i (n0 is placebo)
# nstari <= ni is PP sample size for group i
# Ti is the test statistic for Hi, rho(Ti, Tj) is the correlation
part2 <- function(n0 = 1000, n1 = n0, n2 = n0, p_vals = c(.01, .02, .005, .05),
                  sim_rho = FALSE) {
  # n0 <- 1000
  # n1 <- 1000
  # n2 <- 1000

  nstar0 <- .9 * n0
  nstar1 <- .9 * n1
  nstar2 <- .9 * n2

  alpha <- .025

  test_stats <- paste0("T", 1:4)

  if (sim_rho) {
    rho <- matrix(runif(16), nrow = 4)
    rho[lower.tri(rho)] <- t(rho)[lower.tri(rho)]
    diag(rho) <- 1
  } else {
    rho <- structure(matrix(NA, nrow = 4, ncol = 4),
                     dimnames = list(test_stats, test_stats))
    diag(rho) <- 1
    rho[1, 2] <- rho[2, 1] <- rho[3, 4] <- rho[4, 3] <-
      (n1 / (n0 + n1)) ^ .5 * (n2 / (n0 + n2)) ^ .5

    rho[1, 3] <- rho[3, 1] <-
      ((n0 + n1) / (n0 * n1)) ^ .5 *
      ((nstar0 * nstar1) / (nstar0 + nstar1)) ^ .5

    rho[2, 4] <- rho[4, 2] <-
      ((n0 + n2) / (n0 * n2)) ^ .5 *
      ((nstar0 * nstar2) / (nstar0 + nstar2)) ^ .5

    rho[1, 4] <- rho[4, 1] <- rho[1, 3] * rho [3, 4]
    rho[2, 3] <- rho[3, 2] <- rho[2, 4] * rho [4, 3]
  }

  cat("\nrho ---------------------------------------------------------------\n")
  print(rho)

  gw <- generate_weights(g)
  gw_weights <- gw[, 5:8]

  for (i in seq_len(nrow(gw))) {
    cJ[[i]] <- uniroot(
      myfct,
      lower = 1,
      upper = 9,
      a = alpha,
      w = gw_weights[i, ],
      sig = rho
    )$root
  }

  # So for part 2, this uniroot() example function matches what's in the paper
  cat("\ncJ ----------------------------------------------------------------\n")
  print(cbind(gw, cJ))

  # So how do the tests look? They match, but they also don't show different
  # results than the last ones, or even differ from Bonferroni. So it's not that
  # interesting, I think
  cat("\ngMCP --------------------------------------------------------------\n")
  print(gMCP(as_gmcp_graph(g), p_vals, alpha = alpha, correlation = rho,
       test = "parametric")@rejected)

  cat("\ngraphicalMCP ------------------------------------------------------\n")
  graphical <- test_graph(g, p_vals, alpha = alpha, corr = rho,
                          tests = list(parametric = list(1:4)))$hypotheses_rejected

  print(graphical)

  invisible(graphical)
}

# Non-one c-values are around 1.07
part2()

# Making n1/n2 both large increases c-values, making more pass (potentially)
part2(n1 = 2000, n2 = 2000)

# And vice versa
part2(n1 = 500, n2 = 500)

# Moving them in opposite directions has little impact
part2(n1 = n0 * .5, n2 = n0 / .5)

# Moving just one has more than half the effect of moving both
part2(n1 = 2000)

p <- .028 / 2
# Make 2/3 close to passing with small c-values
part2(p_vals = c(.01, p, p, .5))

# Then actually passing with larger c-values
part2(n1 = 2000, n2 = 2000, p_vals = c(.01, p, p, .5))


p <- .026 / 2
# Make 2/3 close to failing with small c-values
part2(p_vals = c(.01, p, p, .5))

# Then actually failing with smaller c-values
part2(n1 = 500, n2 = 500, p_vals = c(.01, p, p, .5))

# This whole sensitivity testing makes me feel like parametric is not
# particularly powerful. But probably a movement of .001 in your p-values can be
# significant in the real world. I suppose this is somewhat re-inventing a power
# simulation, huh?
#
# Also, how cool is it that gMCP and test_graph always match for all these?
# Pretty baller! ⛹️

# Okay, so the sample sizes can't have a huge effect on c-values, but the
# correlation matrix certainly can (Run the next line several times) But, if a
# p-value if over α, it can't ever pass, unless there's some way to make cJ > 1,
# which I doubt? So that sweet spot (in this graph's case) will be to hover just
# above α / 2. Then you'll fail Bonferroni, but might pass parametric, depending
# on (*gestures wildly*) stuff
part2(p_vals = c(.01, .027 / 2, .027 / 2, .05), sim_rho = TRUE) # Illustrates parallel gatekeeping well

# Also illustrates the parallel gatekeeping well
# Should be uniform results with something like Bonferroni-Holm
res <- matrix(nrow = 10000, ncol = 4)
for (i in 1:10000) {
  res[i, ] <- part2(p_vals = rep(.027 / 2, 4), sim_rho = TRUE)
}
