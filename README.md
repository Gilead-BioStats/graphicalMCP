
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/graphicalMCP)](https://cran.r-project.org/package=graphicalMCP)
[![Codecov test
coverage](https://codecov.io/gh/Gilead-BioStats/graphicalMCP/branch/s3-graph_mcp/graph/badge.svg)](https://app.codecov.io/gh/Gilead-BioStats/graphicalMCP?branch=s3-graph_mcp)
[![R-CMD-check](https://github.com/Gilead-BioStats/graphicalMCP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Gilead-BioStats/graphicalMCP/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# graphicalMCP <img src="man/figures/logo.png" align="right" height="350"/>

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

## Glossary of terms

| Entity                          | Definition                                                                                                                                                                                            | Aliases                               | Variable(s)                     | Related                         |
|---------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------|---------------------------------|---------------------------------|
| **Graph**                       | A set of nodes and edges representing a potential clinical trial design                                                                                                                               |                                       | `graph`                         | Hypotheses, Transitions         |
|                                 | **Graphs** are so central that two of their core qualities get their own common variable names: Node names, and number of hypotheses                                                                  |                                       | `hyp_names`, `num_hyps`         |                                 |
| **Hypotheses**                  | The weighted nodes in a **graph**. Each node represents a null hypothesis, and its weight the local significance level.                                                                               | weights, hypothesis weights           | `hypotheses`                    | Weighting strategy, Transitions |
| **Transitions**                 | The weighted edges in a **graph**. Each edge defines how to propagate local significance when a source node is deleted.                                                                               |                                       | `transitions`                   | Hypotheses                      |
| **Intersection** **hypothesis** | A subset of **hypotheses** from a **graph**. Plural often implies all such subsets.                                                                                                                   | intersection,sub-graphs, closure      | `intersections`                 | Weighting strategy              |
| **Weighting strategy**          | The set of all **intersections** and their **weights** according to Algorithm 1 in Bretz et al                                                                                                        | intersection weights, closure weights | `weighting_strategy`            |                                 |
| **Critical values**             |                                                                                                                                                                                                       |                                       |                                 |                                 |
| **P-values**                    | The set of p-values from a real or simulated clinical trial                                                                                                                                           |                                       | `p`                             | Adjusted & ordered p-values     |
| **Ordered p-values**            | **P-values** sorted from smallest to largest                                                                                                                                                          |                                       | `ordered_p`                     | (Adjusted) P-values             |
| **Adjusted p-values**           | **P-values** that have been divided by **critical values**, allowing direct comparison to **alpha** to determine significance                                                                         |                                       | `adjusted_p`                    | (Ordered) P-values              |
| **Significance level**          | The threshold chosen for results of a clinical trial to be considered significant                                                                                                                     |                                       | `alpha`                         | P-values                        |
| **Test types**                  | A specification of which testing algorithm to use - Bonferroni, Simes, and parametric are supported                                                                                                   | tests                                 | `test_types`                    | Testing strategy                |
| **Test groups**                 | A partition of nodes in a **graph** specifying which **hypotheses** should be tested together                                                                                                         | groups                                | `groups`, `test_groups`         | Testing strategy                |
| **Testing strategy**            | **Test types** and **test groups** combined                                                                                                                                                           |                                       |                                 |                                 |
| **Marginal power**              | The mean of each null **hypothesis** in the underlying multivariate normal distribution of the null hypotheses                                                                                        |                                       | `marginal_power`                | Correlation matrix              |
| **Correlation matrix**          | Specification of correlations between **hypotheses**. Together with **marginal power**, this specifies the (known or assumed) underlying multivariate normal distribution of the null **hypotheses**. |                                       | `corr`, `test_corr`, `sim_corr` | Marginal power                  |
| **Success**                     | A specification of which null **hypotheses** must be rejected to consider a clinical trial a success                                                                                                  |                                       | `sim_success`                   |                                 |
| **Power**                       | Under a given **graph**, **testing strategy**, **significance level**, and underlying distribution, the estimated likelihood that a particular combination of null hypotheses will be rejected        |                                       | `power_*`                       | Success                         |

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
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> A1: 0.5000
#> A2: 0.5000
#> B1: 0.0000
#> B2: 0.0000
#> 
#> --- Transition weights ---
#>         A1     A2     B1     B2
#>  A1 0.0000 0.0000 1.0000 0.0000
#>  A2 0.0000 0.0000 0.0000 1.0000
#>  B1 0.0000 1.0000 0.0000 0.0000
#>  B2 1.0000 0.0000 0.0000 0.0000
```

### Update graph

Hypotheses can be rejected from the MCP using `update_graph()`. Updated
weights and transitions are calculated according to the weighting
strategy in [Bretz et al
(2011)](https://onlinelibrary.wiley.com/doi/10.1002/bimj.201000239)

``` r
update_graph(g_dose, c(TRUE, FALSE, FALSE, TRUE))
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> A1: 0.5000
#> A2: 0.5000
#> B1: 0.0000
#> B2: 0.0000
#> 
#> --- Transition weights ---
#>         A1     A2     B1     B2
#>  A1 0.0000 0.0000 1.0000 0.0000
#>  A2 0.0000 0.0000 0.0000 1.0000
#>  B1 0.0000 1.0000 0.0000 0.0000
#>  B2 1.0000 0.0000 0.0000 0.0000
#> 
#> --------------------------------------------------------------------------------
#> 
#> --- Hypotheses kept ---
#>    A1    A2    B1   B2
#>  TRUE FALSE FALSE TRUE
#> 
#> --------------------------------------------------------------------------------
#> 
#> Updated graph
#> 
#> --- Hypothesis weights ---
#> A1: 0.5000
#> A2: 0.0000
#> B1: 0.0000
#> B2: 0.5000
#> 
#> --- Transition weights ---
#>         A1     A2     B1     B2
#>  A1 0.0000 0.0000 0.0000 1.0000
#>  A2 0.0000 0.0000 0.0000 0.0000
#>  B1 0.0000 0.0000 0.0000 0.0000
#>  B2 1.0000 0.0000 0.0000 0.0000
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
can be tested against a given alpha with `test_graph_closure()`. A
report is then generated, showing the graph & test results.

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
test_graph_closure(
  g_dose,
  p = c(.01, .02, .03, .05),
  alpha = .05,
  test_types = "b",
  groups = list(1:4)
)
#> 
#> Test parameters ----------------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.500000
#>   A2: 0.500000
#>   B1: 0.000000
#>   B2: 0.000000
#> 
#>   --- Transition weights ---
#>            A1       A2       B1       B2
#>   A1 0.000000 0.000000 1.000000 0.000000
#>   A2 0.000000 0.000000 0.000000 1.000000
#>   B1 0.000000 1.000000 0.000000 0.000000
#>   B2 1.000000 0.000000 0.000000 0.000000
#> 
#>   Alpha = 0.05
#> 
#>                          A1   A2   B1   B2
#>   Unadjusted p-values: 0.01 0.02 0.03 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Test summary -------------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1         0.02   TRUE
#>           A2         0.04   TRUE
#>           B1         0.06  FALSE
#>           B2         0.06  FALSE
#> 
#>   Updated graph after rejections
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.000000
#>   A2: 0.000000
#>   B1: 0.500000
#>   B2: 0.500000
#> 
#>   --- Transition weights ---
#>            A1       A2       B1       B2
#>   A1 0.000000 0.000000 0.000000 0.000000
#>   A2 0.000000 0.000000 0.000000 0.000000
#>   B1 0.000000 0.000000 0.000000 1.000000
#>   B2 0.000000 0.000000 1.000000 0.000000
```

Simes and parametric testing methods are also supported, using the
`test_types` argument. Try setting the `verbose` and `critical` flags
for a more detailed report on testing.

``` r
test_graph_closure(
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
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.500000
#>   A2: 0.500000
#>   B1: 0.000000
#>   B2: 0.000000
#> 
#>   --- Transition weights ---
#>            A1       A2       B1       B2
#>   A1 0.000000 0.000000 1.000000 0.000000
#>   A2 0.000000 0.000000 0.000000 1.000000
#>   B1 0.000000 1.000000 0.000000 0.000000
#>   B2 1.000000 0.000000 0.000000 0.000000
#> 
#>   Alpha = 0.05
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
#> Test summary -------------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1         0.02   TRUE
#>           A2         0.04   TRUE
#>           B1         0.05   TRUE
#>           B2         0.05   TRUE
#> 
#>   Updated graph after rejections
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.000000
#>   A2: 0.000000
#>   B1: 0.000000
#>   B2: 0.000000
#> 
#>   --- Transition weights ---
#>            A1       A2       B1       B2
#>   A1 0.000000 0.000000 0.000000 0.000000
#>   A2 0.000000 0.000000 0.000000 0.000000
#>   B1 0.000000 0.000000 0.000000 0.000000
#>   B2 0.000000 0.000000 0.000000 0.000000
#> 
#> Test details - Adjusted p ------------------------------------------------------
#>       A1  A2  B1  B2 padj_grp1 padj_grp2 p_adj_inter rej
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
#>    Intersection Hypothesis       Test    p <=        c * Critical * Alpha Reject
#>               1         A1 parametric 0.01 <= 1.106458 *      0.5 *  0.05   TRUE
#>               1         A2 parametric 0.02 <= 1.106458 *      0.5 *  0.05   TRUE
#>               1         B1      simes 0.03 <=                 0.0 *  0.05  FALSE
#>               1         B2      simes 0.05 <=                 0.0 *  0.05  FALSE
#>               2         A1 parametric 0.01 <= 1.106458 *      0.5 *  0.05   TRUE
#>               2         A2 parametric 0.02 <= 1.106458 *      0.5 *  0.05   TRUE
#>               2         B1      simes 0.03 <=                 0.0 *  0.05  FALSE
#>               3         A1 parametric 0.01 <= 1.106458 *      0.5 *  0.05   TRUE
#>               3         A2 parametric 0.02 <= 1.106458 *      0.5 *  0.05   TRUE
#>               3         B2      simes 0.05 <=                 0.0 *  0.05  FALSE
#>               4         A1 parametric 0.01 <= 1.106458 *      0.5 *  0.05   TRUE
#>               4         A2 parametric 0.02 <= 1.106458 *      0.5 *  0.05   TRUE
#>               5         A1 parametric 0.01 <=        1 *      0.5 *  0.05   TRUE
#>               5         B1      simes 0.03 <=                 0.0 *  0.05  FALSE
#>               5         B2      simes 0.05 <=                 0.5 *  0.05  FALSE
#>               6         A1 parametric 0.01 <=        1 *      1.0 *  0.05   TRUE
#>               6         B1      simes 0.03 <=                 0.0 *  0.05  FALSE
#>               7         A1 parametric 0.01 <=        1 *      0.5 *  0.05   TRUE
#>               7         B2      simes 0.05 <=                 0.5 *  0.05  FALSE
#>               8         A1 parametric 0.01 <=        1 *      1.0 *  0.05   TRUE
#>               9         A2 parametric 0.02 <=        1 *      0.5 *  0.05   TRUE
#>               9         B1      simes 0.03 <=                 0.5 *  0.05  FALSE
#>               9         B2      simes 0.05 <=                 0.5 *  0.05  FALSE
#>              10         A2 parametric 0.02 <=        1 *      0.5 *  0.05   TRUE
#>              10         B1      simes 0.03 <=                 0.5 *  0.05  FALSE
#>              11         A2 parametric 0.02 <=        1 *      1.0 *  0.05   TRUE
#>              11         B2      simes 0.05 <=                 0.0 *  0.05  FALSE
#>              12         A2 parametric 0.02 <=        1 *      1.0 *  0.05   TRUE
#>              13         B1      simes 0.03 <=                 0.5 *  0.05  FALSE
#>              13         B2      simes 0.05 <=                 1.0 *  0.05   TRUE
#>              14         B1      simes 0.03 <=                 1.0 *  0.05   TRUE
#>              15         B2      simes 0.05 <=                 1.0 *  0.05   TRUE
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
  marginal_power = c(1, 1, 1, 1)
)
#> 
#> Test parameters ----------------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.500000
#>   A2: 0.500000
#>   B1: 0.000000
#>   B2: 0.000000
#> 
#>   --- Transition weights ---
#>            A1       A2       B1       B2
#>   A1 0.000000 0.000000 1.000000 0.000000
#>   A2 0.000000 0.000000 0.000000 1.000000
#>   B1 0.000000 1.000000 0.000000 0.000000
#>   B2 1.000000 0.000000 0.000000 0.000000
#> 
#>   Global alpha = 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Simulation parameters ----------------------------------------------------------
#>   Testing 100,000 simulations with multivariate normal params:
#> 
#>                   A1 A2 B1 B2
#>   Marginal power:  1  1  1  1
#> 
#>   Correlation:    A1 A2 B1 B2
#>                A1  1  0  0  0
#>                A2  0  1  0  0
#>                B1  0  0  1  0
#>                B2  0  0  0  1
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.17102 0.16981 0.03003 0.02983
#> 
#>         Expected rejections: 0.40069
#>   Power to reject 1 or more: 0.30706
#>         Power to reject all: 0.00332
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.487245 0.039462 0.237048 0.278554  FALSE  FALSE  FALSE  FALSE
#>   0.033752 0.252295 0.004860 0.007874  FALSE  FALSE  FALSE  FALSE
#>   0.418167 0.395485 0.106901 0.302197  FALSE  FALSE  FALSE  FALSE
#>   0.277895 0.164969 0.022008 0.062993  FALSE  FALSE  FALSE  FALSE
#>   0.081146 0.239575 0.565388 0.021711  FALSE  FALSE  FALSE  FALSE
#>   0.007596 0.071909 0.554441 0.573774   TRUE  FALSE  FALSE  FALSE
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
  marginal_power = c(1, 1, 1, 1)
)
#> 
#> Test parameters ----------------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.500000
#>   A2: 0.500000
#>   B1: 0.000000
#>   B2: 0.000000
#> 
#>   --- Transition weights ---
#>            A1       A2       B1       B2
#>   A1 0.000000 0.500000 0.500000 0.000000
#>   A2 0.500000 0.000000 0.000000 0.500000
#>   B1 0.000000 1.000000 0.000000 0.000000
#>   B2 1.000000 0.000000 0.000000 0.000000
#> 
#>   Global alpha = 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Simulation parameters ----------------------------------------------------------
#>   Testing 100,000 simulations with multivariate normal params:
#> 
#>                   A1 A2 B1 B2
#>   Marginal power:  1  1  1  1
#> 
#>   Correlation:    A1 A2 B1 B2
#>                A1  1  0  0  0
#>                A2  0  1  0  0
#>                B1  0  0  1  0
#>                B2  0  0  0  1
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.17609 0.17668 0.02264 0.02268
#> 
#>         Expected rejections: 0.39809
#>   Power to reject 1 or more: 0.30741
#>         Power to reject all: 0.00301
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.187090 0.297671 0.143046 0.187417  FALSE  FALSE  FALSE  FALSE
#>   0.357566 0.450327 0.004242 0.269982  FALSE  FALSE  FALSE  FALSE
#>   0.086783 0.484388 0.421100 0.158553  FALSE  FALSE  FALSE  FALSE
#>   0.075160 0.151046 0.927364 0.413286  FALSE  FALSE  FALSE  FALSE
#>   0.154559 0.303063 0.405742 0.316448  FALSE  FALSE  FALSE  FALSE
#>   0.048938 0.212898 0.537415 0.000170  FALSE  FALSE  FALSE  FALSE
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
  marginal_power = c(1, 1, 1, 1),
  test_types = c("s", "p"),
  test_groups = list(1:2, 3:4),
  test_corr = diag(4)
)
#> 
#> Test parameters ----------------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.500000
#>   A2: 0.500000
#>   B1: 0.000000
#>   B2: 0.000000
#> 
#>   --- Transition weights ---
#>            A1       A2       B1       B2
#>   A1 0.000000 0.500000 0.500000 0.000000
#>   A2 0.500000 0.000000 0.000000 0.500000
#>   B1 0.000000 1.000000 0.000000 0.000000
#>   B2 1.000000 0.000000 0.000000 0.000000
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
#>                   A1 A2 B1 B2
#>   Marginal power:  1  1  1  1
#> 
#>   Correlation:    A1 A2 B1 B2
#>                A1  1  0  0  0
#>                A2  0  1  0  0
#>                B1  0  0  1  0
#>                B2  0  0  0  1
#> 
#> Power calculation --------------------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.18154 0.18339 0.02362 0.02389
#> 
#>         Expected rejections: 0.41244
#>   Power to reject 1 or more: 0.3154
#>         Power to reject all: 0.00366
#> 
#> Simulation details -------------------------------------------------------------
#>   p_sim_A1 p_sim_A2 p_sim_B1 p_sim_B2 rej_A1 rej_A2 rej_B1 rej_B2
#>   0.674925 0.004111 0.356191 0.872622  FALSE   TRUE  FALSE  FALSE
#>   0.074545 0.060858 0.309452 0.047561  FALSE  FALSE  FALSE  FALSE
#>   0.000938 0.065281 0.012272 0.050659   TRUE  FALSE   TRUE  FALSE
#>   0.179105 0.586872 0.233590 0.278176  FALSE  FALSE  FALSE  FALSE
#>   0.031682 0.033470 0.214807 0.093020   TRUE   TRUE  FALSE  FALSE
#>   0.103165 0.014152 0.502712 0.024165  FALSE   TRUE  FALSE  FALSE
#>   ...
```

## Related work

These methods were originally implemented in R after the 2011 paper in
the [gMCP package](https://github.com/kornl/gMCP), which is still
available on CRAN today. There is also a lighter version of gMCP
implemented in [gMCPmini](https://github.com/allenzhuaz/gMCPmini) and
its successor, [gMCPLite](https://github.com/Merck/gMCPLite). These two
contain only a subset of the original functionality, but they remove the
rJava dependency and add plotting functionality based on ggplot2.

However, because development has ceased on the original package, we hope
to re-implement the methods with a clearer distinction between weighting
procedures and test procedures; with fewer dependencies, in particular
shedding the Java dependency; with the simpler, more transparent S3
class framework; and with improvements to the accuracy of the parametric
and Simes test methods.

A portion of Simes testing is also implemented in the lrstat package
(`install.packages("lrstat")`) with similar speed to graphicalMCP.
