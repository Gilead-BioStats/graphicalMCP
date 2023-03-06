
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/graphicalMCP)](https://cran.r-project.org/package=graphicalMCP)
[![Codecov test
coverage](https://codecov.io/gh/Gilead-BioStats/graphicalMCP/branch/s3-graph_mcp/graph/badge.svg)](https://app.codecov.io/gh/Gilead-BioStats/graphicalMCP?branch=s3-graph_mcp)
[![R-CMD-check](https://github.com/Gilead-BioStats/graphicalMCP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Gilead-BioStats/graphicalMCP/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# graphicalMCP <img src="man/figures/logo.png" align="right" height="350" />

## Introduction

A multiple comparison procedure (MCP) is a statistical analysis method
that allows for assessing the efficacy of multiple endpoints, some of
which are dependent on each other, in a single clinical trial. Endpoints
can be different doses, treatment of different conditions, or combined
superiority & non-inferiority testing.

In [Bretz et al
(2011)](https://onlinelibrary.wiley.com/doi/10.1002/bimj.201000239), a
graphical method to MCPs was described, which separates the weighting of
the dependency graph from the particular statistical test used to assess
each endpoint. This package is a low-dependency implementation of those
methods.

## Installation

graphicalMCP is not on CRAN, so install it from GitHub with

``` r
# install.packages("pak")
pak::pak("Gilead-BioStats/graphicalMCP")
```

## Basic usage

### Initial graph

The base object in graphicalMCP is an `initial_graph`, which is a
weighted, directed graph represented by a matrix of transition (edge)
weights, and a vector of hypothesis (vertex) weights.

``` r
library(graphicalMCP)

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
#> An initial graph
#> 
#> --- Hypothesis weights ---
#> A1: 0.5000
#> A2: 0.5000
#> B1: 0.0000
#> B2: 0.0000
#> 
#> --- Transition weights ---
#>        A1     A2     B1     B2
#> A1 0.0000 0.0000 1.0000 0.0000
#> A2 0.0000 0.0000 0.0000 1.0000
#> B1 0.0000 1.0000 0.0000 0.0000
#> B2 1.0000 0.0000 0.0000 0.0000
```

### Update graph

Hypotheses can be rejected from the MCP using `update_graph()`. Updated
weights and transitions are calculated according to the weighting
strategy in [Bretz et al
(2011)](https://onlinelibrary.wiley.com/doi/10.1002/bimj.201000239)

``` r
update_graph(g_dose, c(TRUE, FALSE, FALSE, TRUE))
#> An initial graph
#> 
#> --- Hypothesis weights ---
#> A1: 0.5000
#> A2: 0.5000
#> B1: 0.0000
#> B2: 0.0000
#> 
#> --- Transition weights ---
#>        A1     A2     B1     B2
#> A1 0.0000 0.0000 1.0000 0.0000
#> A2 0.0000 0.0000 0.0000 1.0000
#> B1 0.0000 1.0000 0.0000 0.0000
#> B2 1.0000 0.0000 0.0000 0.0000
#> 
#> --------------------------------------------------------------------------------
#> 
#> --- Hypotheses kept ---
#>    A1    A2    B1   B2
#>  TRUE FALSE FALSE TRUE
#> 
#> --------------------------------------------------------------------------------
#> 
#> An initial graph
#> 
#> --- Hypothesis weights ---
#> A1: 0.5000
#> A2: 0.0000
#> B1: 0.0000
#> B2: 0.5000
#> 
#> --- Transition weights ---
#>        A1     A2     B1     B2
#> A1 0.0000 0.0000 0.0000 1.0000
#> A2 0.0000 0.0000 0.0000 0.0000
#> B1 0.0000 0.0000 0.0000 0.0000
#> B2 1.0000 0.0000 0.0000 0.0000
```

### Generate weights

The weights of all sub-graphs can be calculated with
`generate_weights()`. This uses more efficient code under the hood than
`update_graph()` in order to be performant for larger graphs.

``` r
generate_weights(g_dose)
#>    A1 A2 B1 B2  A1  A2  B1  B2
#> 1   1  1  1  1 0.5 0.5 0.0 0.0
#> 2   1  1  1  0 0.5 0.5 0.0 0.0
#> 3   1  1  0  1 0.5 0.5 0.0 0.0
#> 4   1  1  0  0 0.5 0.5 0.0 0.0
#> 5   1  0  1  1 0.5 0.0 0.0 0.5
#> 6   1  0  1  0 1.0 0.0 0.0 0.0
#> 7   1  0  0  1 0.5 0.0 0.0 0.5
#> 8   1  0  0  0 1.0 0.0 0.0 0.0
#> 9   0  1  1  1 0.0 0.5 0.5 0.0
#> 10  0  1  1  0 0.0 0.5 0.5 0.0
#> 11  0  1  0  1 0.0 1.0 0.0 0.0
#> 12  0  1  0  0 0.0 1.0 0.0 0.0
#> 13  0  0  1  1 0.0 0.0 0.5 0.5
#> 14  0  0  1  0 0.0 0.0 1.0 0.0
#> 15  0  0  0  1 0.0 0.0 0.0 1.0
```

### Test hypotheses

A mixture of statistical tests are (or will be) supported in
graphicalMCP. A graph can be tested against a given alpha with
`test_all_subgraphs()`. A report is then generated, showing the graph &
test results.

In this example, a weighted Bonferroni test is applied to all
hypotheses, with a threshold of 0.05. We can reject a given intersection
hypothesis if any of the individual hypotheses within that sub-graph
passes the test assigned to it. We can then reject a hypothesis globally
if all intersection hypotheses containing that hypothesis are rejected.
For instance, in this example we can reject every intersection
hypothesis except the one containing only B1 & B2 (Row 3 of
`test_results`). Thus, we can reject the null hypotheses for A1 & A2,
but we cannot reject the null hypotheses for B1 & B2.

``` r
test_graph(
  g_dose,
  p = c(.01, .02, .03, .05),
  alpha = .05,
  test_types = "b",
  groups = list(1:4)
)
#> 
#> Test parameters ----------------------------------------------------------------
#>   An initial graph
#>   
#>   --- Hypothesis weights ---
#>   A1: 0.5000
#>   A2: 0.5000
#>   B1: 0.0000
#>   B2: 0.0000
#>   
#>   --- Transition weights ---
#>          A1     A2     B1     B2
#>   A1 0.0000 0.0000 1.0000 0.0000
#>   A2 0.0000 0.0000 0.0000 1.0000
#>   B1 0.0000 1.0000 0.0000 0.0000
#>   B2 1.0000 0.0000 0.0000 0.0000
#> 
#>   Global alpha = 0.05
#> 
#>                          A1   A2   B1   B2
#>   Unadjusted p-values: 0.01 0.02 0.03 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1         0.02   TRUE
#>           A2         0.04   TRUE
#>           B1         0.06  FALSE
#>           B2         0.06  FALSE
```

## Related work

These methods were originally implemented in R after the 2011 paper in
the gMCP package, which is still available on CRAN today:

- `install.packages("gMCP")`
- <https://github.com/kornl/gMCP>

However, because development has ceased on this original package, we
hope to re-implement the methods with a clearer distinction between
weighting procedures and test procedures; with fewer dependencies, in
particular shedding the Java dependency; with the simpler, more
transparent S3 class framework; and with improvements to the accuracy of
the parametric tests method.
