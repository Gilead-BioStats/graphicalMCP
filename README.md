
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
|                                 | **Graphs** are so central that two of their core qualities get their own common variable names: Hypothesis names, and number of hypotheses                                                            |                                       | `hyp_names`, `num_hyps`         |                                 |
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
#> A1: 0.5
#> A2: 0.5
#> B1: 0.0
#> B2: 0.0
#> 
#> --- Transition weights ---
#>     A1 A2 B1 B2
#>  A1  0  0  1  0
#>  A2  0  0  0  1
#>  B1  0  1  0  0
#>  B2  1  0  0  0
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
#> A1: 0.5
#> A2: 0.5
#> B1: 0.0
#> B2: 0.0
#> 
#> --- Transition weights ---
#>     A1 A2 B1 B2
#>  A1  0  0  1  0
#>  A2  0  0  0  1
#>  B1  0  1  0  0
#>  B2  1  0  0  0
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
#> A1: 0.5
#> A2: 0.0
#> B1: 0.0
#> B2: 0.5
#> 
#> --- Transition weights ---
#>     A1 A2 B1 B2
#>  A1  0  0  0  1
#>  A2  0  0  0  0
#>  B1  0  0  0  0
#>  B2  1  0  0  0
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
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.5
#>   A2: 0.5
#>   B1: 0.0
#>   B2: 0.0
#> 
#>   --- Transition weights ---
#>      A1 A2 B1 B2
#>   A1  0  0  1  0
#>   A2  0  0  0  1
#>   B1  0  1  0  0
#>   B2  1  0  0  0
#> 
#>   Alpha = 0.05
#> 
#>                          A1   A2   B1   B2
#>   Unadjusted p-values: 0.01 0.02 0.03 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1         0.02   TRUE
#>           A2         0.04   TRUE
#>           B1         0.06  FALSE
#>           B2         0.06  FALSE
#> 
#>   Updated graph after rejections
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.0
#>   A2: 0.0
#>   B1: 0.5
#>   B2: 0.5
#> 
#>   --- Transition weights ---
#>      A1 A2 B1 B2
#>   A1  0  0  0  0
#>   A2  0  0  0  0
#>   B1  0  0  0  1
#>   B2  0  0  1  0
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
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.5
#>   A2: 0.5
#>   B1: 0.0
#>   B2: 0.0
#> 
#>   --- Transition weights ---
#>      A1 A2 B1 B2
#>   A1  0  0  1  0
#>   A2  0  0  0  1
#>   B1  0  1  0  0
#>   B2  1  0  0  0
#> 
#>   Alpha = 0.05
#> 
#>                          A1   A2   B1   B2
#>   Unadjusted p-values: 0.01 0.02 0.03 0.05
#> 
#>   Correlation matrix:     A1  A2  B1  B2
#>                       A1 1.0 0.5  NA  NA
#>                       A2 0.5 1.0  NA  NA
#>                       B1  NA  NA  NA  NA
#>                       B2  NA  NA  NA  NA
#> 
#>   Test types
#>   parametric: (A1-A2)
#>        simes: (B1-B2)
#> 
#> Test summary ($outputs) --------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1         0.02   TRUE
#>           A2         0.04   TRUE
#>           B1         0.05   TRUE
#>           B2         0.05   TRUE
#> 
#>   Updated graph after rejections
#> 
#>   --- Hypothesis weights ---
#>   A1: 0
#>   A2: 0
#>   B1: 0
#>   B2: 0
#> 
#>   --- Transition weights ---
#>      A1 A2 B1 B2
#>   A1  0  0  0  0
#>   A2  0  0  0  0
#>   B1  0  0  0  0
#>   B2  0  0  0  0
#> 
#> Test details - Adjusted p ($details) -------------------------------------------
#>             A1        A2        B1        B2 adj_p_grp1 adj_p_grp2 adj_p_inter
#>   1  0.5000000 0.5000000 0.0000000 0.0000000  0.0187061  1.0000000   0.0187061
#>   2  0.5000000 0.5000000 0.0000000        NA  0.0187061  1.0000000   0.0187061
#>   3  0.5000000 0.5000000        NA 0.0000000  0.0187061  1.0000000   0.0187061
#>   4  0.5000000 0.5000000        NA        NA  0.0187061  1.0000000   0.0187061
#>   5  0.5000000        NA 0.0000000 0.5000000  0.0200000  0.1000000   0.0200000
#>   6  1.0000000        NA 0.0000000        NA  0.0100000  1.0000000   0.0100000
#>   7  0.5000000        NA        NA 0.5000000  0.0200000  0.1000000   0.0200000
#>   8  1.0000000        NA        NA        NA  0.0100000  1.0000000   0.0100000
#>   9         NA 0.5000000 0.5000000 0.0000000  0.0400000  0.0600000   0.0400000
#>   10        NA 0.5000000 0.5000000        NA  0.0400000  0.0600000   0.0400000
#>      reject
#>   1    TRUE
#>   2    TRUE
#>   3    TRUE
#>   4    TRUE
#>   5    TRUE
#>   6    TRUE
#>   7    TRUE
#>   8    TRUE
#>   9    TRUE
#>   10   TRUE
#>   ... (Use `rows = <xx>` for more)
#> 
#> Test details - Critical values ($critical) -------------------------------------
#>    Intersection Hypothesis       Test    p <= c_value * Critical * Alpha Reject
#>               1         A1 parametric 0.01 <= 1.10646 *      0.5 *  0.05   TRUE
#>               1         A2 parametric 0.02 <= 1.10646 *      0.5 *  0.05   TRUE
#>               1         B1      simes 0.03 <=      NA        0.0 *  0.05  FALSE
#>               1         B2      simes 0.05 <=      NA        0.0 *  0.05  FALSE
#>               2         A1 parametric 0.01 <= 1.10646 *      0.5 *  0.05   TRUE
#>               2         A2 parametric 0.02 <= 1.10646 *      0.5 *  0.05   TRUE
#>               2         B1      simes 0.03 <=      NA        0.0 *  0.05  FALSE
#>               3         A1 parametric 0.01 <= 1.10646 *      0.5 *  0.05   TRUE
#>               3         A2 parametric 0.02 <= 1.10646 *      0.5 *  0.05   TRUE
#>               3         B2      simes 0.05 <=      NA        0.0 *  0.05  FALSE
#>   ... (Use `rows = <xx>` for more)
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
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.5
#>   A2: 0.5
#>   B1: 0.0
#>   B2: 0.0
#> 
#>   --- Transition weights ---
#>      A1 A2 B1 B2
#>   A1  0  0  1  0
#>   A2  0  0  0  1
#>   B1  0  1  0  0
#>   B2  1  0  0  0
#> 
#>   Alpha = 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Simulation parameters ($inputs) ------------------------------------------------
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
#> Power calculation ($power) -----------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.17124 0.17214 0.02899 0.02980
#> 
#>         Expected rejections: 0.40217
#>   Power to reject 1 or more: 0.30936
#>         Power to reject all: 0.00285
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
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.5
#>   A2: 0.5
#>   B1: 0.0
#>   B2: 0.0
#> 
#>   --- Transition weights ---
#>       A1  A2  B1  B2
#>   A1 0.0 0.5 0.5 0.0
#>   A2 0.5 0.0 0.0 0.5
#>   B1 0.0 1.0 0.0 0.0
#>   B2 1.0 0.0 0.0 0.0
#> 
#>   Alpha = 0.05
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Simulation parameters ($inputs) ------------------------------------------------
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
#> Power calculation ($power) -----------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.17503 0.17726 0.02219 0.02228
#> 
#>         Expected rejections: 0.39676
#>   Power to reject 1 or more: 0.3058
#>         Power to reject all: 0.00301
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
#> Test parameters ($inputs) ------------------------------------------------------
#>   Initial graph
#> 
#>   --- Hypothesis weights ---
#>   A1: 0.5
#>   A2: 0.5
#>   B1: 0.0
#>   B2: 0.0
#> 
#>   --- Transition weights ---
#>       A1  A2  B1  B2
#>   A1 0.0 0.5 0.5 0.0
#>   A2 0.5 0.0 0.0 0.5
#>   B1 0.0 1.0 0.0 0.0
#>   B2 1.0 0.0 0.0 0.0
#> 
#>   Alpha = 0.05
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
#> Simulation parameters ($inputs) ------------------------------------------------
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
#> Power calculation ($power) -----------------------------------------------------
#>                                   A1      A2      B1      B2
#>        Power to reject each: 0.18319 0.18232 0.02351 0.02336
#> 
#>         Expected rejections: 0.41238
#>   Power to reject 1 or more: 0.3163
#>         Power to reject all: 0.00328
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
