
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

A mixture of statistical tests are supported in graphicalMCP. A graph
can be tested against a given alpha with `test_graph()`. A report is
then generated, showing the graph & test results.

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

Simes and parametric testing methods are also supported, using the
`test_types` argument. Try setting the `verbose` and `critical` flags
for a more detailed report on testing.

``` r
test_graph(
  g_dose,
  p = c(.01, .02, .03, .05),
  alpha = .05,
  test_types = c("p", "s"),
  groups = list(1:2, 3:4),
  corr = rbind(
    c(1, .5, NA, NA),
    c(.5, 1, NA, NA),
    c(NA, NA, NA, NA),
    c(NA, NA, NA, NA)
  ),
  verbose = TRUE,
  critical = TRUE
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
#>   Correlation matrix:     A1  A2 B1 B2
#>                       A1 1.0 0.5 NA NA
#>                       A2 0.5 1.0 NA NA
#>                       B1  NA  NA NA NA
#>                       B2  NA  NA NA NA
#> 
#>   Test types
#>   parametric: (A1-A2)
#>        simes: (B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1         0.02   TRUE
#>           A2         0.04   TRUE
#>           B1         0.05   TRUE
#>           B2         0.05   TRUE
#> 
#> Test details - Adjusted p ------------------------------------------------------
#>       A1  A2  B1  B2 padj_grp1 padj_grp2 p_adj_inter res
#>   1  0.5 0.5 0.0 0.0  0.018706      1.00    0.018706   1
#>   2  0.5 0.5 0.0  NA  0.018706      1.00    0.018706   1
#>   3  0.5 0.5  NA 0.0  0.018706      1.00    0.018706   1
#>   4  0.5 0.5  NA  NA  0.018706      1.00    0.018706   1
#>   5  0.5  NA 0.0 0.5  0.020000      0.10    0.020000   1
#>   6  1.0  NA 0.0  NA  0.010000      1.00    0.010000   1
#>   7  0.5  NA  NA 0.5  0.020000      0.10    0.020000   1
#>   8  1.0  NA  NA  NA  0.010000      1.00    0.010000   1
#>   9   NA 0.5 0.5 0.0  0.040000      0.06    0.040000   1
#>   10  NA 0.5 0.5  NA  0.040000      0.06    0.040000   1
#>   11  NA 1.0  NA 0.0  0.020000      1.00    0.020000   1
#>   12  NA 1.0  NA  NA  0.020000      1.00    0.020000   1
#>   13  NA  NA 0.5 0.5  1.000000      0.05    0.050000   1
#>   14  NA  NA 1.0  NA  1.000000      0.03    0.030000   1
#>   15  NA  NA  NA 1.0  1.000000      0.05    0.050000   1
#> 
#> Test details - Critical values -------------------------------------------------
#>    intersection hypothesis       test    p <=        c *   w * alpha   res
#>               1         A1 parametric 0.01 <= 1.106454 * 0.5 *  0.05  TRUE
#>               1         A2 parametric 0.02 <= 1.106454 * 0.5 *  0.05  TRUE
#>               1         B1      simes 0.03 <=            0.0 *  0.05 FALSE
#>               1         B2      simes 0.05 <=            0.0 *  0.05 FALSE
#>               2         A1 parametric 0.01 <= 1.106454 * 0.5 *  0.05  TRUE
#>               2         A2 parametric 0.02 <= 1.106454 * 0.5 *  0.05  TRUE
#>               2         B1      simes 0.03 <=            0.0 *  0.05 FALSE
#>               3         A1 parametric 0.01 <= 1.106454 * 0.5 *  0.05  TRUE
#>               3         A2 parametric 0.02 <= 1.106454 * 0.5 *  0.05  TRUE
#>               3         B2      simes 0.05 <=            0.0 *  0.05 FALSE
#>               4         A1 parametric 0.01 <= 1.106454 * 0.5 *  0.05  TRUE
#>               4         A2 parametric 0.02 <= 1.106454 * 0.5 *  0.05  TRUE
#>               5         A1 parametric 0.01 <=        1 * 0.5 *  0.05  TRUE
#>               5         B1      simes 0.03 <=            0.0 *  0.05 FALSE
#>               5         B2      simes 0.05 <=            0.5 *  0.05 FALSE
#>               6         A1 parametric 0.01 <=        1 * 1.0 *  0.05  TRUE
#>               6         B1      simes 0.03 <=            0.0 *  0.05 FALSE
#>               7         A1 parametric 0.01 <=        1 * 0.5 *  0.05  TRUE
#>               7         B2      simes 0.05 <=            0.5 *  0.05 FALSE
#>               8         A1 parametric 0.01 <=        1 * 1.0 *  0.05  TRUE
#>               9         A2 parametric 0.02 <=        1 * 0.5 *  0.05  TRUE
#>               9         B1      simes 0.03 <=            0.5 *  0.05 FALSE
#>               9         B2      simes 0.05 <=            0.5 *  0.05 FALSE
#>              10         A2 parametric 0.02 <=        1 * 0.5 *  0.05  TRUE
#>              10         B1      simes 0.03 <=            0.5 *  0.05 FALSE
#>              11         A2 parametric 0.02 <=        1 * 1.0 *  0.05  TRUE
#>              11         B2      simes 0.05 <=            0.0 *  0.05 FALSE
#>              12         A2 parametric 0.02 <=        1 * 1.0 *  0.05  TRUE
#>              13         B1      simes 0.03 <=            0.5 *  0.05 FALSE
#>              13         B2      simes 0.05 <=            1.0 *  0.05  TRUE
#>              14         B1      simes 0.03 <=            1.0 *  0.05  TRUE
#>              15         B2      simes 0.05 <=            1.0 *  0.05  TRUE
```

## Power simulations

It’s not always obvious from a given graph structure how easy or
difficult it will be to reject each hypothesis. One way to understand
this better is to run a power simulation. The essence of a power
simulation is to generate many different p-values using some chosen
distribution, then test the graph against each set of p-values to see
how it performs.

### Bonferroni

``` r
calculate_power(
  g_dose,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1)
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
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Simulation parameters ----------------------------------------------------------
#>   Testing 100,000 simulations with multivariate normal params:
#> 
#>                     A1 A2 B1 B2
#>   Simulation means:  1  1  1  1
#> 
#>   Simulation covariance:    A1 A2 B1 B2
#>                          A1  1  0  0  0
#>                          A2  0  1  0  0
#>                          B1  0  0  1  0
#>                          B2  0  0  0  1
#> 
#>   Success is defined as rejecting any of [A1, A2]
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1      A2      B1     B2
#>        Power to reject each: 0.16956 0.17023 0.02943 0.0291
#> 
#>         Expected rejections: 0.39832
#>   Power to reject 1 or more: 0.3074
#>         Power to reject all: 0.00294
#>      Probability of success: 0.3074
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.734324 0.066643 0.072314 0.192624  FALSE  FALSE  FALSE  FALSE
#>   0.219480 0.000344 0.579458 0.276940  FALSE   TRUE  FALSE  FALSE
#>   0.259436 0.000584 0.302898 0.332274  FALSE   TRUE  FALSE  FALSE
#>   0.804838 0.003552 0.044241 0.003158  FALSE   TRUE  FALSE   TRUE
#>   0.584070 0.008772 0.287811 0.120703  FALSE   TRUE  FALSE  FALSE
#>   0.645343 0.076597 0.583397 0.123549  FALSE  FALSE  FALSE  FALSE
#>   ...
```

The `simple_successive_2()` function creates a parallel gate-keeping
graph, but some weight is transferred between the primary hypotheses,
rather than each passing weight on to its respective secondary
hypothesis. This causes the primary hypotheses to be rejected slightly
more often, and the secondary hypotheses less often.

``` r
g_dose_2 <- simple_successive_2(names)

calculate_power(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1)
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
#>   A1 0.0000 0.5000 0.5000 0.0000
#>   A2 0.5000 0.0000 0.0000 0.5000
#>   B1 0.0000 1.0000 0.0000 0.0000
#>   B2 1.0000 0.0000 0.0000 0.0000
#> 
#>   Global alpha = 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Simulation parameters ----------------------------------------------------------
#>   Testing 100,000 simulations with multivariate normal params:
#> 
#>                     A1 A2 B1 B2
#>   Simulation means:  1  1  1  1
#> 
#>   Simulation covariance:    A1 A2 B1 B2
#>                          A1  1  0  0  0
#>                          A2  0  1  0  0
#>                          B1  0  0  1  0
#>                          B2  0  0  0  1
#> 
#>   Success is defined as rejecting any of [A1, A2]
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.17694 0.17667 0.02229 0.02256
#> 
#>         Expected rejections: 0.39846
#>   Power to reject 1 or more: 0.30739
#>         Power to reject all: 0.00294
#>      Probability of success: 0.30739
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.121233 0.625074 0.185369 0.365154  FALSE  FALSE  FALSE  FALSE
#>   0.003418 0.005331 0.471701 0.088073   TRUE   TRUE  FALSE  FALSE
#>   0.414792 0.145246 0.353021 0.207961  FALSE  FALSE  FALSE  FALSE
#>   0.054151 0.447358 0.044497 0.026981  FALSE  FALSE  FALSE  FALSE
#>   0.381960 0.203325 0.197936 0.004214  FALSE  FALSE  FALSE  FALSE
#>   0.308600 0.110535 0.043437 0.043006  FALSE  FALSE  FALSE  FALSE
#>   ...
```

### Other tests

Bonferroni testing uses a shortcut to make testing, and therefore power,
run very fast (\<1s up to a 10-graph/100K-simulation). However, power
simulations can be run with any test strategy. Note that other testing
strategies will cause a substantial increase in the time a power
simulation takes. In rough ascending order of time impact: parametric
tests, multiple groups, Simes tests.

``` r
calculate_power(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = "p",
  test_corr = diag(4)
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
#>   A1 0.0000 0.5000 0.5000 0.0000
#>   A2 0.5000 0.0000 0.0000 0.5000
#>   B1 0.0000 1.0000 0.0000 0.0000
#>   B2 1.0000 0.0000 0.0000 0.0000
#> 
#>   Global alpha = 0.05
#> 
#>   Parametric testing correlation:    A1 A2 B1 B2
#>                                   A1  1  0  0  0
#>                                   A2  0  1  0  0
#>                                   B1  0  0  1  0
#>                                   B2  0  0  0  1
#> 
#>   Test types
#>   parametric: (A1-A2-B1-B2)
#> 
#> Simulation parameters ----------------------------------------------------------
#>   Testing 100,000 simulations with multivariate normal params:
#> 
#>                     A1 A2 B1 B2
#>   Simulation means:  1  1  1  1
#> 
#>   Simulation covariance:    A1 A2 B1 B2
#>                          A1  1  0  0  0
#>                          A2  0  1  0  0
#>                          B1  0  0  1  0
#>                          B2  0  0  0  1
#> 
#>   Success is defined as rejecting any of [A1, A2]
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.18042 0.17984 0.02352 0.02318
#> 
#>         Expected rejections: 0.40696
#>   Power to reject 1 or more: 0.31237
#>         Power to reject all: 0.00293
#>      Probability of success: 0.31237
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.059851 0.214734 0.087130 0.085869  FALSE  FALSE  FALSE  FALSE
#>   0.669420 0.019747 0.417605 0.338864  FALSE   TRUE  FALSE  FALSE
#>   0.415546 0.132561 0.026116 0.007222  FALSE  FALSE  FALSE  FALSE
#>   0.028955 0.411062 0.972080 0.684987  FALSE  FALSE  FALSE  FALSE
#>   0.004987 0.434737 0.409300 0.068290   TRUE  FALSE  FALSE  FALSE
#>   0.056020 0.631518 0.354720 0.157413  FALSE  FALSE  FALSE  FALSE
#>   ...
```

``` r
calculate_power(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = "s",
  test_groups = list(1:4)
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
#>   A1 0.0000 0.5000 0.5000 0.0000
#>   A2 0.5000 0.0000 0.0000 0.5000
#>   B1 0.0000 1.0000 0.0000 0.0000
#>   B2 1.0000 0.0000 0.0000 0.0000
#> 
#>   Global alpha = 0.05
#> 
#>   Test types
#>   simes: (A1-A2-B1-B2)
#> 
#> Simulation parameters ----------------------------------------------------------
#>   Testing 100,000 simulations with multivariate normal params:
#> 
#>                     A1 A2 B1 B2
#>   Simulation means:  1  1  1  1
#> 
#>   Simulation covariance:    A1 A2 B1 B2
#>                          A1  1  0  0  0
#>                          A2  0  1  0  0
#>                          B1  0  0  1  0
#>                          B2  0  0  0  1
#> 
#>   Success is defined as rejecting any of [A1, A2]
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1     A2      B1      B2
#>        Power to reject each: 0.18334 0.1833 0.02481 0.02559
#> 
#>         Expected rejections: 0.41704
#>   Power to reject 1 or more: 0.31522
#>         Power to reject all: 0.00481
#>      Probability of success: 0.31522
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.009093 0.002317 0.120415 0.243067   TRUE   TRUE  FALSE  FALSE
#>   0.122093 0.033363 0.193634 0.602013  FALSE  FALSE  FALSE  FALSE
#>   0.032154 0.214218 0.029764 0.371079  FALSE  FALSE  FALSE  FALSE
#>   0.060998 0.207009 0.726477 0.721505  FALSE  FALSE  FALSE  FALSE
#>   0.420560 0.636468 0.172388 0.198972  FALSE  FALSE  FALSE  FALSE
#>   0.252351 0.079659 0.024400 0.088242  FALSE  FALSE  FALSE  FALSE
#>   ...
```

``` r
calculate_power(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = c("s", "p"),
  test_groups = list(1:2, 3:4),
  test_corr = diag(4)
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
#>   A1 0.0000 0.5000 0.5000 0.0000
#>   A2 0.5000 0.0000 0.0000 0.5000
#>   B1 0.0000 1.0000 0.0000 0.0000
#>   B2 1.0000 0.0000 0.0000 0.0000
#> 
#>   Global alpha = 0.05
#> 
#>   Parametric testing correlation:    A1 A2 B1 B2
#>                                   A1  1  0  0  0
#>                                   A2  0  1  0  0
#>                                   B1  0  0  1  0
#>                                   B2  0  0  0  1
#> 
#>   Test types
#>        simes: (A1-A2)
#>   parametric: (B1-B2)
#> 
#> Simulation parameters ----------------------------------------------------------
#>   Testing 100,000 simulations with multivariate normal params:
#> 
#>                     A1 A2 B1 B2
#>   Simulation means:  1  1  1  1
#> 
#>   Simulation covariance:    A1 A2 B1 B2
#>                          A1  1  0  0  0
#>                          A2  0  1  0  0
#>                          B1  0  0  1  0
#>                          B2  0  0  0  1
#> 
#>   Success is defined as rejecting any of [A1, A2]
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.18379 0.18369 0.02414 0.02331
#> 
#>         Expected rejections: 0.41493
#>   Power to reject 1 or more: 0.31749
#>         Power to reject all: 0.00349
#>      Probability of success: 0.31749
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.235136 0.542285 0.050847 0.347626  FALSE  FALSE  FALSE  FALSE
#>   0.019190 0.193386 0.026304 0.412830   TRUE  FALSE  FALSE  FALSE
#>   0.120857 0.109389 0.012917 0.068154  FALSE  FALSE  FALSE  FALSE
#>   0.005220 0.133472 0.389114 0.108096   TRUE  FALSE  FALSE  FALSE
#>   0.617954 0.258738 0.140075 0.111332  FALSE  FALSE  FALSE  FALSE
#>   0.002952 0.036309 0.475232 0.506712   TRUE   TRUE  FALSE  FALSE
#>   ...
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
the parametric and Simes test methods.

A portion of Simes testing is also implemented in the lrstat package
(`install.packages("lrstat")`) with similar speed to graphicalMCP.
