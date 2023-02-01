hypotheses <- c(ni_lo = 0.5, ni_hi = 0.5, su_lo = 0, su_hi = 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- create_graph(hypotheses, transitions)
G <- matrix2graph(transitions, hypotheses)

bh4 <- bonferroni_holm(4)
BH4 <- matrix2graph(bh4$transitions, bh4$hypotheses)

p_vals <- c(.001, .02, .05, .1)
p_vals2 <- c(.001, .02, .05, .05)
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

# Bonferroni
gMCP(G, p)
test_graph(g, p)

# parametric
gMCP(G, p, corr = corr2, test = "parametric")
test_graph(g, p, corr = corr2_, alpha = .05,
           tests = list(parametric = list(1:4)))


# Simes
gMCP(G, p, test = "Simes")
test_graph(g, p, alpha = .05, tests = list(simes = list(1:4)))
test_graph(g, p, alpha = .05, tests = list(simes = list(1, 2, 3, 4)))

gMCP(BH4, .051 / 1:4, test = "Simes")
test_graph(bh4, .051 / 1:4, alpha = .05, tests = list(simes = list(1:4)))

test_graph(bh4, .051 / 1:4, tests = list(bonferroni = list(1:4)))
# Simes reduces to Bonferroni if all groups are separated
test_graph(bh4, .051 / 1:4, tests = list(simes = list(1, 2, 3, 4)))
test_graph(bh4, .051 / 1:4, tests = list(simes = list(1:4)))

# Simes gets more powerful when p-values are equal
test_graph(bh4, rep(.049, 4), tests = list(simes = list(1:4)))
test_graph(bh4, rep(.049, 4), tests = list(bonferroni = list(1:4)))

test_graph(g, p_vals, tests = list(simes = list(1:4)))
# T/T/F/F
#
# But making p for 3/4 equal actually adds to the weight that both of them are
# getting, making them both pass
test_graph(g, p_vals2, tests = list(simes = list(1:4)))
test_graph(g, c(.001, .02, .049, .051), tests = list(simes = list(1:4)))
test_graph(g, c(.001, .02, .051, .049), tests = list(simes = list(1:4)))

