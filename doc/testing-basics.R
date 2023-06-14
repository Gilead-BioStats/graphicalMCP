## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(graphicalMCP)

## ----create-graph-1-----------------------------------------------------------
# A graphical multiple comparison procedure with two primary hypotheses (H1
# and H2) and two secondary hypotheses (H3 and H4)
# See Figure 1 in Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer,
# W., & Rohmeyer, K. (2011). Graphical approaches for multiple comparison
# procedures using weighted Bonferroni, Simes, or parametric tests. Biometrical
# Journal, 53(6), 894-913.
hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("A1", "A2", "B1", "B2")
g_dose <- create_graph(hypotheses, transitions, names)

g_dose

## ----bonferroni-mix-1---------------------------------------------------------
test_graph_closure(g_dose, p_values = c(.024, .01, .026, .027), alpha = .05)

## ----simes-all-1--------------------------------------------------------------
test_graph_closure(
  g_dose,
  p_values = c(.024, .01, .026, .027),
  alpha = .05,
  tests = list(simes = list(1:4))
)

## ----parametric-1-------------------------------------------------------------
corr1 <- matrix(nrow = 4, ncol = 4)
corr1[3:4, 3:4] <- .5
diag(corr1) <- 1

test_graph_closure(
  g_dose,
  p_values = c(.024, .01, .026, .027),
  alpha = .05,
  corr = corr1,
  use_cj = FALSE,
  tests = list(parametric = list(1, 2, 3:4))
)

## ----parametric-2-------------------------------------------------------------
corr2 <- diag(4)

test_graph_closure(
  g_dose,
  p_values = c(.024, .01, .026, .027),
  alpha = .05,
  corr = corr2,
  use_cj = FALSE,
  tests = list(parametric = list(1:4))
)

## ----parametric-3-------------------------------------------------------------
corr3 <- matrix(nrow = 4, ncol = 4)
diag(corr3) <- 1

test_graph_closure(
  g_dose,
  p_values = c(.024, .01, .026, .027),
  alpha = .05,
  corr = corr3, # Which correlation matrix doesn't matter when each group is 1
  use_cj = FALSE,
  tests = list(parametric = list(1, 2, 3, 4))
)

## ----parametric-4-------------------------------------------------------------
corr4 <- matrix(.5, nrow = 4, ncol = 4)
diag(corr4) <- 1

test_graph_closure(
  g_dose,
  p_values = c(.024, .01, .026, .027),
  alpha = .05,
  corr = corr4,
  use_cj = TRUE,
  tests = list(parametric = list(1:4))
)

## ----parametric-5, eval=FALSE-------------------------------------------------
#  test_graph_closure(
#    g_dose,
#    p_values = c(.024, .01, .026, .027),
#    alpha = .05,
#    corr = corr1,
#    use_cj = TRUE,
#    tests = list(parametric = list(1:4))
#  )

