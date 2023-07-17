params <-
list(m = 5L, sims = 100000L)

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache.lazy = FALSE
)

## ----setup--------------------------------------------------------------------
library(gt)
library(gMCP)
library(graphicalMCP)

## ----create-graph-1-----------------------------------------------------------
ss_graph <- simple_successive_2(c("A1", "B1", "A2", "B2"))

pvals <- c(.024, .01, .026, .027)

ss_graph

## ----bonferroni-mix-1---------------------------------------------------------
test_graph_closure(ss_graph, p = pvals, alpha = .05)

## ----simes-all-1--------------------------------------------------------------
test_graph_closure(ss_graph, p = pvals, alpha = .05, test_types = "s")

## ----parametric-1-------------------------------------------------------------
corr1 <- matrix(nrow = 4, ncol = 4)
corr1[3:4, 3:4] <- .5
diag(corr1) <- 1

test_graph_closure(ss_graph,
  p = pvals,
  alpha = .05,
  groups = list(1:2, 3:4),
  test_types = c("b", "p"),
  corr = corr1
)

## ----verbose------------------------------------------------------------------
test_graph_closure(
  ss_graph,
  p = pvals,
  alpha = .05,
  corr = corr1,
  groups = list(1:2, 3:4),
  test_types = c("s", "p"),
  verbose = TRUE
)

## ----critical-----------------------------------------------------------------
test_graph_closure(
  ss_graph,
  p = pvals,
  alpha = .05,
  corr = corr1,
  groups = list(1:2, 3:4),
  test_types = c("s", "p"),
  verbose = TRUE,
  critical = TRUE
)

## ----sequential---------------------------------------------------------------
test_graph_shortcut(ss_graph, p = pvals, alpha = .05, critical = TRUE)

## ----print-indent-------------------------------------------------------------
set.seed(3123)
# Randomly generate a graph
m <- 5
w <- sample(1:m, replace = TRUE)
w <- w / sum(w)
g <- replicate(m, sample(1:m, replace = TRUE), simplify = TRUE)
diag(g) <- 0
g <- g / rowSums(g)
graph <- new("graphMCP", m = g, weights = w)
graph2 <- create_graph(w, g)

p <- runif(m, .0001, .05)
sim_corr <- diag(m)

mix_test <- test_graph_closure(
  graph2,
  p = p,
  alpha = .05,
  corr = sim_corr,
  groups = list(1, 2:3, 4:5),
  test_types = c("b", "s", "p"),
  verbose = TRUE,
  critical = TRUE
)

print(mix_test)

print(mix_test, indent = 6, precision = 10)

## ----power-bonf---------------------------------------------------------------
calculate_power(ss_graph, sim_n = 1e5)

## ----power-mix----------------------------------------------------------------
corr2 <- matrix(.5, nrow = 4, ncol = 4)
diag(corr2) <- 1

calculate_power(
  ss_graph,
  test_groups = list(1:4),
  test_types = c("p"),
  test_corr = corr2,
  sim_n = 1e5
)

## ----power-sims---------------------------------------------------------------
s_corr1 <- rbind(
  c(1, .5, .5, .25),
  c(.5, 1, .25, .5),
  c(.5, .25, 1, .5),
  c(.25, .5, .5, 1)
)

calculate_power(
  ss_graph,
  test_groups = list(1:4),
  test_types = c("p"),
  test_corr = corr2,
  marginal_power = c(1, 1, 3, 3),
  sim_corr = s_corr1,
  sim_success = function(.) .[1] || .[2],
  sim_seed = 52423, # Set a seed if you need consistent p-values
  sim_n = 1e5
)

