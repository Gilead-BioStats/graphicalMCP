
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

<table style="width:99%;">
<colgroup>
<col style="width: 9%" />
<col style="width: 57%" />
<col style="width: 11%" />
<col style="width: 9%" />
<col style="width: 9%" />
</colgroup>
<thead>
<tr class="header">
<th>Entity</th>
<th>Definition</th>
<th>Aliases</th>
<th>Variable(s)</th>
<th>Related</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><strong>Graph</strong></td>
<td>A set of nodes and edges representing a potential clinical trial
design</td>
<td></td>
<td><code>graph</code></td>
<td>Hypotheses, Transitions</td>
</tr>
<tr class="even">
<td></td>
<td><strong>Graphs</strong> are so central that two of their core
qualities get their own common variable names: Hypothesis names, and
number of hypotheses</td>
<td></td>
<td><code>hyp_names</code>, <code>num_hyps</code></td>
<td></td>
</tr>
<tr class="odd">
<td><strong>Hypotheses</strong></td>
<td>The weighted nodes in a <strong>graph</strong>. Each node represents
a null hypothesis, and its weight the local significance level.</td>
<td>weights, hypothesis weights</td>
<td><code>hypotheses</code></td>
<td>Weighting strategy, Transitions</td>
</tr>
<tr class="even">
<td><strong>Transitions</strong></td>
<td>The weighted edges in a <strong>graph</strong>. Each edge defines
how to propagate local significance when a source node is deleted.</td>
<td></td>
<td><code>transitions</code></td>
<td>Hypotheses</td>
</tr>
<tr class="odd">
<td><strong>Intersection</strong> <strong>hypothesis</strong></td>
<td>A subset of <strong>hypotheses</strong> from a
<strong>graph</strong>. Plural often implies all such subsets.</td>
<td>intersection,sub-graphs, closure</td>
<td><code>intersections</code></td>
<td>Weighting strategy</td>
</tr>
<tr class="even">
<td><strong>Weighting strategy</strong></td>
<td>The set of all <strong>intersections</strong> and their
<strong>weights</strong> according to Algorithm 1 in Bretz et al
(2011)</td>
<td>intersection weights, closure weights</td>
<td><code>weighting_strategy</code></td>
<td></td>
</tr>
<tr class="odd">
<td><strong>Critical values</strong></td>
<td><p>The set of <strong>weights</strong>, adjusted according to a
testing algorithm:</p>
<ul>
<li>Bonferroni: No change</li>
<li>Simes: Sum weights for hypotheses with smaller p-values</li>
<li>Parametric: Multiply weights by c-value, based on the joint
distribution</li>
</ul></td>
<td></td>
<td><code>critical_values</code></td>
<td></td>
</tr>
<tr class="even">
<td><strong>P-values</strong></td>
<td>The set of p-values from a real or simulated clinical trial</td>
<td></td>
<td><code>p</code></td>
<td>Adjusted &amp; ordered p-values</td>
</tr>
<tr class="odd">
<td><strong>Ordered p-values</strong></td>
<td><strong>P-values</strong> sorted from smallest to largest</td>
<td></td>
<td><code>ordered_p</code></td>
<td>(Adjusted) P-values</td>
</tr>
<tr class="even">
<td><strong>Adjusted p-values</strong></td>
<td><strong>P-values</strong> that have been divided by <strong>critical
values</strong>, allowing direct comparison to <strong>alpha</strong> to
determine significance</td>
<td></td>
<td><code>adjusted_p</code></td>
<td>(Ordered) P-values</td>
</tr>
<tr class="odd">
<td><strong>Significance level</strong></td>
<td>The threshold chosen for results of a clinical trial to be
considered significant</td>
<td></td>
<td><code>alpha</code></td>
<td>P-values</td>
</tr>
<tr class="even">
<td><strong>Test types</strong></td>
<td>A specification of which testing algorithm to use - Bonferroni,
Simes, and parametric are supported</td>
<td>tests</td>
<td><code>test_types</code></td>
<td>Testing strategy</td>
</tr>
<tr class="odd">
<td><strong>Test groups</strong></td>
<td>A partition of nodes in a <strong>graph</strong> specifying which
<strong>hypotheses</strong> should be tested together</td>
<td>groups</td>
<td><code>groups</code>, <code>test_groups</code></td>
<td>Testing strategy</td>
</tr>
<tr class="even">
<td><strong>Testing strategy</strong></td>
<td><strong>Test types</strong> and <strong>test groups</strong>
combined</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr class="odd">
<td><strong>Marginal power</strong></td>
<td>The mean of each null <strong>hypothesis</strong> in the underlying
multivariate normal distribution of the null hypotheses</td>
<td></td>
<td><code>marginal_power</code></td>
<td>Correlation matrix</td>
</tr>
<tr class="even">
<td><strong>Correlation matrix</strong></td>
<td>Specification of correlations between <strong>hypotheses</strong>.
Together with <strong>marginal power</strong>, this specifies the (known
or assumed) underlying multivariate normal distribution of the null
<strong>hypotheses</strong>.</td>
<td></td>
<td><code>corr</code>, <code>test_corr</code>,
<code>sim_corr</code></td>
<td>Marginal power</td>
</tr>
<tr class="odd">
<td><strong>Success</strong></td>
<td>A specification of which null <strong>hypotheses</strong> must be
rejected to consider a clinical trial a success</td>
<td></td>
<td><code>sim_success</code></td>
<td></td>
</tr>
<tr class="even">
<td><strong>Power</strong></td>
<td>Under a given <strong>graph</strong>, <strong>testing
strategy</strong>, <strong>significance level</strong>, and underlying
distribution, the estimated likelihood that a particular combination of
null hypotheses will be rejected</td>
<td></td>
<td><code>power_*</code></td>
<td>Success</td>
</tr>
</tbody>
</table>

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
#>        Power to reject each: 0.17087 0.16880 0.02986 0.02975
#> 
#>         Expected rejections: 0.39928
#>   Power to reject 1 or more: 0.30602
#>         Power to reject all: 0.00302
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
#>        Power to reject each: 0.17924 0.17745 0.02310 0.02287
#> 
#>         Expected rejections: 0.40266
#>   Power to reject 1 or more: 0.31006
#>         Power to reject all: 0.00331
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
#>        Power to reject each: 0.18161 0.18280 0.02334 0.02384
#> 
#>         Expected rejections: 0.41159
#>   Power to reject 1 or more: 0.31572
#>         Power to reject all: 0.00352
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
