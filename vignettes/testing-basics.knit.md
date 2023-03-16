---
title: "Testing basics"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Testing basics}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
library(graphicalMCP)
library(gMCP)
#> 
#> Attaching package: 'gMCP'
#> The following objects are masked from 'package:graphicalMCP':
#> 
#>     fallback, rqmvnorm
```

## Testing parameters

Start testing with the example graph from the README, a parallel gate-keeping procedure graph.


```r
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
par_gate <- create_graph(hypotheses, transitions, names)

pvals <- c(.024, .01, .026, .027)

par_gate
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

This graph can be tested most simply with the default weighted Bonferroni test. When testing at the global alpha level 0.05, we can reject hypotheses A1 and A2, but not B1 or B2.


```r
test_graph(par_gate, p = pvals, alpha = .05)
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Test types
#>   bonferroni: (A1-A2-B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1        0.048   TRUE
#>           A2        0.020   TRUE
#>           B1        0.052  FALSE
#>           B2        0.052  FALSE
```

The results of the weighted Simes test are equivalent to weighted Bonferroni in some situations. The power of the Simes test becomes apparent when multiple p-values fall below the global alpha level, but above their local alpha in some intersection(s). In the following case, B1 & B2 are rejected in the Bonferroni testing procedure for intersection `B1 ∩ B2` because the p-value is greater than `α * w` for each hypothesis in that case. However, the Simes test rejects `B1 ∩ B2` because the weight from B1 is added to the weight for B2.


```r
test_graph(par_gate, p = pvals, alpha = .05, test_types = "s")
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Test types
#>   simes: (A1-A2-B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1        0.027   TRUE
#>           A2        0.020   TRUE
#>           B1        0.027   TRUE
#>           B2        0.027   TRUE
```

If a correlation matrix for the test statistics is partially or fully known, a parametric test can be used for any subsets whose correlation matrix is fully known. Here B1 & B2 get a `c` value calculated that boosts their testing threshold slightly higher. 


```r
corr1 <- matrix(nrow = 4, ncol = 4)
corr1[3:4, 3:4] <- .5
diag(corr1) <- 1

test_graph(par_gate,
  p = pvals,
  alpha = .05,
  groups = list(1, 2, 3:4),
  test_types = c("b", "b", "p"),
  corr = corr1
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Correlation matrix:    A1 A2  B1  B2
#>                       A1  1 NA  NA  NA
#>                       A2 NA  1  NA  NA
#>                       B1 NA NA 1.0 0.5
#>                       B2 NA NA 0.5 1.0
#> 
#>   Test types
#>   bonferroni: (A1)
#>   bonferroni: (A2)
#>   parametric: (B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1        0.048   TRUE
#>           A2        0.020   TRUE
#>           B1        0.048   TRUE
#>           B2        0.048   TRUE
```

 ~~The parametric test reduces to Bonferroni when there is no correlation between any test statistics.~~ (This doesn't look like it's true, actually)


```r
corr2 <- diag(4)

test_graph(
  par_gate,
  p = pvals,
  alpha = .05,
  corr = diag(4),
  test_types = "p"
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Correlation matrix:    A1 A2 B1 B2
#>                       A1  1  0  0  0
#>                       A2  0  1  0  0
#>                       B1  0  0  1  0
#>                       B2  0  0  0  1
#> 
#>   Test types
#>   parametric: (A1-A2-B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1     0.047424   TRUE
#>           A2     0.019900   TRUE
#>           B1     0.051324  FALSE
#>           B2     0.051324  FALSE
```

The null case of this is when a parametric group is size 1 (Each correlation matrix is a 1x1 with value 1)


```r
corr3 <- matrix(nrow = 4, ncol = 4)
diag(corr3) <- 1

test_graph(
  par_gate,
  p = pvals,
  alpha = .05,
  corr = corr3, # Correlation matrix doesn't matter when each group is size 1
  groups = list(1, 2, 3, 4),
  test_types = rep("p", 4)
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Correlation matrix:    A1 A2 B1 B2
#>                       A1  1 NA NA NA
#>                       A2 NA  1 NA NA
#>                       B1 NA NA  1 NA
#>                       B2 NA NA NA  1
#> 
#>   Test types
#>   parametric: (A1)
#>   parametric: (A2)
#>   parametric: (B1)
#>   parametric: (B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1        0.048   TRUE
#>           A2        0.020   TRUE
#>           B1        0.052  FALSE
#>           B2        0.052  FALSE
```

Using different test types on different parts of a graph is supported.


```r
test_graph(
  par_gate,
  p = pvals,
  alpha = .05,
  corr = corr1,
  groups = list(1:2, 3:4),
  test_types = c("s", "p")
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Correlation matrix:    A1 A2  B1  B2
#>                       A1  1 NA  NA  NA
#>                       A2 NA  1  NA  NA
#>                       B1 NA NA 1.0 0.5
#>                       B2 NA NA 0.5 1.0
#> 
#>   Test types
#>        simes: (A1-A2)
#>   parametric: (B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1        0.048   TRUE
#>           A2        0.020   TRUE
#>           B1        0.048   TRUE
#>           B2        0.048   TRUE
```

There are two different testing methods - one which tests each hypothesis with the `p <= (c *) w * α` method, and another which calculates adjusted p-values. The adjusted p-values method is much more efficient, so it is the standard method. Additional details about the adjusted p-values calculation can be seen by setting `verbose = TRUE`.


```r
test_graph(
  par_gate,
  p = pvals,
  alpha = .05,
  corr = corr1,
  groups = list(1:2, 3:4),
  test_types = c("s", "p"),
  verbose = TRUE
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Correlation matrix:    A1 A2  B1  B2
#>                       A1  1 NA  NA  NA
#>                       A2 NA  1  NA  NA
#>                       B1 NA NA 1.0 0.5
#>                       B2 NA NA 0.5 1.0
#> 
#>   Test types
#>        simes: (A1-A2)
#>   parametric: (B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1        0.048   TRUE
#>           A2        0.020   TRUE
#>           B1        0.048   TRUE
#>           B2        0.048   TRUE
#> 
#> Test details - Adjusted p ------------------------------------------------------
#>       A1  A2  B1  B2 padj_grp1 padj_grp2 p_adj_inter res
#>   1  0.5 0.5 0.0 0.0     0.020  1.000000    0.020000   1
#>   2  0.5 0.5 0.0  NA     0.020  1.000000    0.020000   1
#>   3  0.5 0.5  NA 0.0     0.020  1.000000    0.020000   1
#>   4  0.5 0.5  NA  NA     0.020  1.000000    0.020000   1
#>   5  0.5  NA 0.0 0.5     0.048  0.054000    0.048000   1
#>   6  1.0  NA 0.0  NA     0.024  1.000000    0.024000   1
#>   7  0.5  NA  NA 0.5     0.048  0.054000    0.048000   1
#>   8  1.0  NA  NA  NA     0.024  1.000000    0.024000   1
#>   9   NA 0.5 0.5 0.0     0.020  0.052000    0.020000   1
#>   10  NA 0.5 0.5  NA     0.020  0.052000    0.020000   1
#>   11  NA 1.0  NA 0.0     0.010  1.000000    0.010000   1
#>   12  NA 1.0  NA  NA     0.010  1.000000    0.010000   1
#>   13  NA  NA 0.5 0.5     1.000  0.047118    0.047118   1
#>   14  NA  NA 1.0  NA     1.000  0.026000    0.026000   1
#>   15  NA  NA  NA 1.0     1.000  0.027000    0.027000   1
```

The critical value method tests every hypothesis in the closure. Setting `critical = TRUE` displays the values used in each of these tests. This can provide more detailed information about what caused a hypothesis to fail than the adjusted p-values. However, it comes at a significant cost in computation time.


```r
test_graph(
  par_gate,
  p = pvals,
  alpha = .05,
  corr = corr1,
  groups = list(1:2, 3:4),
  test_types = c("s", "p"),
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
#>                           A1   A2    B1    B2
#>   Unadjusted p-values: 0.024 0.01 0.026 0.027
#> 
#>   Correlation matrix:    A1 A2  B1  B2
#>                       A1  1 NA  NA  NA
#>                       A2 NA  1  NA  NA
#>                       B1 NA NA 1.0 0.5
#>                       B2 NA NA 0.5 1.0
#> 
#>   Test types
#>        simes: (A1-A2)
#>   parametric: (B1-B2)
#> 
#> Global test summary ------------------------------------------------------------
#>   Hypothesis Adj. P-value Reject
#>           A1        0.048   TRUE
#>           A2        0.020   TRUE
#>           B1        0.048   TRUE
#>           B2        0.048   TRUE
#> 
#> Test details - Adjusted p ------------------------------------------------------
#>       A1  A2  B1  B2 padj_grp1 padj_grp2 p_adj_inter res
#>   1  0.5 0.5 0.0 0.0     0.020  1.000000    0.020000   1
#>   2  0.5 0.5 0.0  NA     0.020  1.000000    0.020000   1
#>   3  0.5 0.5  NA 0.0     0.020  1.000000    0.020000   1
#>   4  0.5 0.5  NA  NA     0.020  1.000000    0.020000   1
#>   5  0.5  NA 0.0 0.5     0.048  0.054000    0.048000   1
#>   6  1.0  NA 0.0  NA     0.024  1.000000    0.024000   1
#>   7  0.5  NA  NA 0.5     0.048  0.054000    0.048000   1
#>   8  1.0  NA  NA  NA     0.024  1.000000    0.024000   1
#>   9   NA 0.5 0.5 0.0     0.020  0.052000    0.020000   1
#>   10  NA 0.5 0.5  NA     0.020  0.052000    0.020000   1
#>   11  NA 1.0  NA 0.0     0.010  1.000000    0.010000   1
#>   12  NA 1.0  NA  NA     0.010  1.000000    0.010000   1
#>   13  NA  NA 0.5 0.5     1.000  0.047118    0.047118   1
#>   14  NA  NA 1.0  NA     1.000  0.026000    0.026000   1
#>   15  NA  NA  NA 1.0     1.000  0.027000    0.027000   1
#> 
#> Test details - Critical values -------------------------------------------------
#>    intersection hypothesis       test     p <=        c *   w * alpha   res
#>               1         A1      simes 0.024 <=            1.0 *  0.05  TRUE
#>               1         A2      simes 0.010 <=            0.5 *  0.05  TRUE
#>               1         B1 parametric 0.026 <=        1 * 0.0 *  0.05 FALSE
#>               1         B2 parametric 0.027 <=        1 * 0.0 *  0.05 FALSE
#>               2         A1      simes 0.024 <=            1.0 *  0.05  TRUE
#>               2         A2      simes 0.010 <=            0.5 *  0.05  TRUE
#>               2         B1 parametric 0.026 <=        1 * 0.0 *  0.05 FALSE
#>               3         A1      simes 0.024 <=            1.0 *  0.05  TRUE
#>               3         A2      simes 0.010 <=            0.5 *  0.05  TRUE
#>               3         B2 parametric 0.027 <=        1 * 0.0 *  0.05 FALSE
#>               4         A1      simes 0.024 <=            1.0 *  0.05  TRUE
#>               4         A2      simes 0.010 <=            0.5 *  0.05  TRUE
#>               5         A1      simes 0.024 <=            0.5 *  0.05  TRUE
#>               5         B1 parametric 0.026 <=        1 * 0.0 *  0.05 FALSE
#>               5         B2 parametric 0.027 <=        1 * 0.5 *  0.05 FALSE
#>               6         A1      simes 0.024 <=            1.0 *  0.05  TRUE
#>               6         B1 parametric 0.026 <=        1 * 0.0 *  0.05 FALSE
#>               7         A1      simes 0.024 <=            0.5 *  0.05  TRUE
#>               7         B2 parametric 0.027 <=        1 * 0.5 *  0.05 FALSE
#>               8         A1      simes 0.024 <=            1.0 *  0.05  TRUE
#>               9         A2      simes 0.010 <=            0.5 *  0.05  TRUE
#>               9         B1 parametric 0.026 <=        1 * 0.5 *  0.05 FALSE
#>               9         B2 parametric 0.027 <=        1 * 0.0 *  0.05 FALSE
#>              10         A2      simes 0.010 <=            0.5 *  0.05  TRUE
#>              10         B1 parametric 0.026 <=        1 * 0.5 *  0.05 FALSE
#>              11         A2      simes 0.010 <=            1.0 *  0.05  TRUE
#>              11         B2 parametric 0.027 <=        1 * 0.0 *  0.05 FALSE
#>              12         A2      simes 0.010 <=            1.0 *  0.05  TRUE
#>              13         B1 parametric 0.026 <= 1.106486 * 0.5 *  0.05  TRUE
#>              13         B2 parametric 0.027 <= 1.106486 * 0.5 *  0.05  TRUE
#>              14         B1 parametric 0.026 <=        1 * 1.0 *  0.05  TRUE
#>              15         B2 parametric 0.027 <=        1 * 1.0 *  0.05  TRUE
```

## Performance

`test_graph()` always calculates the full closure, even for Bonferroni testing. If optimal performance is needed, `bonferroni_sequential()` can be used, which uses the sequential testing shortcut. This causes a significant improvement in speed, and either method is faster than `gMCP::gMCP()`. However, using `gMCP::graphTest()` is the fastest option, as it's written primarily in C directly.








