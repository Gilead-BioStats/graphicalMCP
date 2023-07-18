# test input ideas

g <- bonferroni_holm(10)

corr <- diag(10)

# Option 1, nested lists
graph_test_closure(
  g,
  p_values = .05 / 1:10,
  alpha = .05,
  tests = list(
    bonferroni = list(1:3),
    parametric = list(c(4, 6, 8), 9:10),
    simes = list(c(5, 7))
  ),
  corr = corr
)

# Option 2, named parameters
graph_test_closure(
  g,
  p_values = .05 / 1:10,
  alpha = .05,
  bonferroni = list(1:3),
  parametric = list(c(4, 6, 8), 9:10),
  simes = list(c(5, 7)),
  corr = corr
)

# Option 3, the three dots
graph_test_closure(
  g,
  p_values = .05 / 1:10,
  alpha = .05,
  bonferroni = 1:3,
  parametric = c(4, 6, 8),
  parametric = 9:10,
  simes = list(5, 7), # Lists could get "un-listed" if necessary
  corr = corr
)

# Option 4, positional
graph_test_closure(
  g,
  p_values = .05 / 1:10,
  alpha = .05,
  tests = "b1b1b1p1s1p1s1p1p2p2",
  corr = corr
)

graph_test_closure(
  g,
  p_values = .05 / 1:10,
  alpha = .05,
  tests = c("b1", "b1", "b1", "p1", "s1", "p1", "s1", "p1", "p2", "p2"),
  corr = corr
)

# Option 5, separate
graph_test_closure(
  g,
  p_values = .05 / 1:10,
  alpha = .05,
  groups = list(1:3, c(4, 6, 8), c(5, 7), 9:10),
  tests = c("b", "p", "s", "p"),
  corr = corr
)





