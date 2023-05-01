
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
    c( 1, .5, NA, NA),
    c(.5,  1, NA, NA),
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
#>               1         A1 parametric 0.01 <= 1.106486 * 0.5 *  0.05  TRUE
#>               1         A2 parametric 0.02 <= 1.106486 * 0.5 *  0.05  TRUE
#>               1         B1      simes 0.03 <=            0.0 *  0.05 FALSE
#>               1         B2      simes 0.05 <=            0.0 *  0.05 FALSE
#>               2         A1 parametric 0.01 <= 1.106486 * 0.5 *  0.05  TRUE
#>               2         A2 parametric 0.02 <= 1.106486 * 0.5 *  0.05  TRUE
#>               2         B1      simes 0.03 <=            0.0 *  0.05 FALSE
#>               3         A1 parametric 0.01 <= 1.106486 * 0.5 *  0.05  TRUE
#>               3         A2 parametric 0.02 <= 1.106486 * 0.5 *  0.05  TRUE
#>               3         B2      simes 0.05 <=            0.0 *  0.05 FALSE
#>               4         A1 parametric 0.01 <= 1.106486 * 0.5 *  0.05  TRUE
#>               4         A2 parametric 0.02 <= 1.106486 * 0.5 *  0.05  TRUE
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
calculate_power_vms(
  g_dose,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  sim_success = integer(0)
)
#> $power_local
#> [1] 0.16965 0.17159 0.02813 0.02340
#> 
#> $power_expected
#> [1] 0.39277
#> 
#> $power_at_least_1
#> [1] 0.30799
#> 
#> $power_all
#> [1] 0
#> 
#> $power_success
#> [1] 0
```

The `simple_successive_2()` function creates a parallel gate-keeping
graph, but some weight is transferred between the primary hypotheses,
rather than each passing weight on to its respective secondary
hypothesis. This causes the primary hypotheses to be rejected slightly
more often, and the secondary hypotheses less often.

``` r
g_dose_2 <- simple_successive_2(names)

calculate_power_vms(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = "s"
)
#> $power_local
#>      A1      A2      B1      B2 
#> 0.18410 0.18554 0.02554 0.02472 
#> 
#> $power_expected
#> [1] 0.4199
#> 
#> $power_at_least_1
#> [1] 0.31827
#> 
#> $power_all
#> [1] 0.00455
#> 
#> $power_success
#> [1] 0.31827
```

### Other tests

Bonferroni testing uses a shortcut to make testing, and therefore power,
run very fast (\<1s up to a 10-graph/100K-simulation). However, power
simulations can be run with any test strategy. Note that other testing
strategies will cause a substantial increase in the time a power
simulation takes. In rough ascending order of time impact: parametric
tests, multiple groups, Simes tests. Vignette to come detailing the
typical impact of each.

``` r
calculate_power_vms(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = "p",
  test_corr = diag(4)
)
#> $power_local
#>      A1      A2      B1      B2 
#> 0.17721 0.17790 0.02263 0.02302 
#> 
#> $power_expected
#> [1] 0.40076
#> 
#> $power_at_least_1
#> [1] 0.30822
#> 
#> $power_all
#> [1] 0.00307
#> 
#> $power_success
#> [1] 0.30822
```

``` r
calculate_power_vms(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = "p",
  test_groups = c(1:2, 3:4),
  test_corr = diag(4)
)
#> $power_local
#> [1] 0.17477 0.17819 0.01744 0.01485
#> 
#> $power_expected
#> [1] 0.38525
#> 
#> $power_at_least_1
#> [1] 0.30747
#> 
#> $power_all
#> [1] 0.00012
#> 
#> $power_success
#> [1] 0.30747
```

``` r
calculate_power_vms(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = "s",
  test_groups = c(1:4)
)
#> $power_local
#> [1] 0.17681 0.17775 0.01767 0.01484
#> 
#> $power_expected
#> [1] 0.38707
#> 
#> $power_at_least_1
#> [1] 0.30808
#> 
#> $power_all
#> [1] 1e-04
#> 
#> $power_success
#> [1] 0.30808
```

``` r
calculate_power_vms(
  g_dose_2,
  sim_n = 1e5,
  sim_theta = c(1, 1, 1, 1),
  test_types = "s",
  test_groups = c(1:2, 3:4)
)
#> $power_local
#> [1] 0.17960 0.17803 0.01891 0.01552
#> 
#> $power_expected
#> [1] 0.39206
#> 
#> $power_at_least_1
#> [1] 0.31051
#> 
#> $power_all
#> [1] 1e-04
#> 
#> $power_success
#> [1] 0.31051
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
