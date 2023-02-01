
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/graphicalMCP)](https://cran.r-project.org/package=graphicalMCP)
[![R-CMD-check](https://github.com/Gilead-BioStats/graphicalMCP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Gilead-BioStats/graphicalMCP/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Gilead-BioStats/graphicalMCP/branch/s3-graph_mcp/graph/badge.svg)](https://app.codecov.io/gh/Gilead-BioStats/graphicalMCP?branch=s3-graph_mcp)
<!-- badges: end -->

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
#> An MCP graph
#> 
#> --- Hypothesis weights ---
#> A1: (0.5000)
#> A2: (0.5000)
#> B1: (0.0000)
#> B2: (0.0000)
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
#> An MCP graph
#> 
#> --- Hypothesis weights ---
#> A1: (0.5000)
#> A2: (0.5000)
#> B1: (0.0000)
#> B2: (0.0000)
#> 
#> --- Transition weights ---
#>        A1     A2     B1     B2
#> A1 0.0000 0.0000 1.0000 0.0000
#> A2 0.0000 0.0000 0.0000 1.0000
#> B1 0.0000 1.0000 0.0000 0.0000
#> B2 1.0000 0.0000 0.0000 0.0000
#> 
#> --------------------------------------
#> 
#> --- Hypotheses kept ---
#>    A1    A2    B1   B2
#>  TRUE FALSE FALSE TRUE
#> 
#> --------------------------------------
#> 
#> An MCP graph
#> 
#> --- Hypothesis weights ---
#> A1: (0.5000)
#> A2: (0.0000)
#> B1: (0.0000)
#> B2: (0.5000)
#> 
#> --- Transition weights ---
#>        A1     A2     B1     B2
#> A1 0.0000 0.0000 0.0000 1.0000
#> A2 0.0000 0.0000 0.0000 0.0000
#> B1 0.0000 0.0000 0.0000 0.0000
#> B2 1.0000 0.0000 0.0000 0.0000
```

### Generate weights

The weights of all subgraphs can be calculated with
`generate_weights()`. This uses some more efficient code under the hood
than `update_graph()` in order to be performant for larger graphs.

``` r
generate_weights(g_dose)
#>    A1 A2 B1 B2  A1  A2  B1  B2
#> 1   1  1  1  1 0.5 0.5 0.0 0.0
#> 2   0  1  1  1 0.0 0.5 0.5 0.0
#> 3   0  0  1  1 0.0 0.0 0.5 0.5
#> 4   0  0  0  1 0.0 0.0 0.0 1.0
#> 5   0  0  1  0 0.0 0.0 1.0 0.0
#> 6   0  1  0  1 0.0 1.0 0.0 0.0
#> 7   0  1  0  0 0.0 1.0 0.0 0.0
#> 8   0  1  1  0 0.0 0.5 0.5 0.0
#> 9   1  0  1  1 0.5 0.0 0.0 0.5
#> 10  1  0  0  1 0.5 0.0 0.0 0.5
#> 11  1  0  0  0 1.0 0.0 0.0 0.0
#> 12  1  0  1  0 1.0 0.0 0.0 0.0
#> 13  1  1  0  1 0.5 0.5 0.0 0.0
#> 14  1  1  0  0 0.5 0.5 0.0 0.0
#> 15  1  1  1  0 0.5 0.5 0.0 0.0
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
