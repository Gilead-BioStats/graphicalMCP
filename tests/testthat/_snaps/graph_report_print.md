# printing Bonferroni/Simes closure test

    Code
      test_graph_closure(par_gate, rep(0.01, 4), test_types = "s")
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.500000
        H2: 0.500000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 1.000000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 1.000000 0.000000 0.000000
        H4 1.000000 0.000000 0.000000 0.000000
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        simes: (H1-H2-H3-H4)
      
      Test summary -------------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.01   TRUE
                H2         0.01   TRUE
                H3         0.01   TRUE
                H4         0.01   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 0.000000 0.000000
        H2 0.000000 0.000000 0.000000 0.000000
        H3 0.000000 0.000000 0.000000 0.000000
        H4 0.000000 0.000000 0.000000 0.000000

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), verbose = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.500000
        H2: 0.500000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 1.000000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 1.000000 0.000000 0.000000
        H4 1.000000 0.000000 0.000000 0.000000
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Test summary -------------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 0.000000 0.000000
        H2 0.000000 0.000000 0.000000 0.000000
        H3 0.000000 0.000000 0.000000 0.000000
        H4 0.000000 0.000000 0.000000 0.000000
      
      Test details - Adjusted p ------------------------------------------------------
            H1  H2  H3  H4 padj_grp1 p_adj_inter res
        1  0.5 0.5 0.0 0.0      0.02        0.02   1
        2  0.5 0.5 0.0  NA      0.02        0.02   1
        3  0.5 0.5  NA 0.0      0.02        0.02   1
        4  0.5 0.5  NA  NA      0.02        0.02   1
        5  0.5  NA 0.0 0.5      0.02        0.02   1
        6  1.0  NA 0.0  NA      0.01        0.01   1
        7  0.5  NA  NA 0.5      0.02        0.02   1
        8  1.0  NA  NA  NA      0.01        0.01   1
        9   NA 0.5 0.5 0.0      0.02        0.02   1
        10  NA 0.5 0.5  NA      0.02        0.02   1
        11  NA 1.0  NA 0.0      0.01        0.01   1
        12  NA 1.0  NA  NA      0.01        0.01   1
        13  NA  NA 0.5 0.5      0.02        0.02   1
        14  NA  NA 1.0  NA      0.01        0.01   1
        15  NA  NA  NA 1.0      0.01        0.01   1

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), critical = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.500000
        H2: 0.500000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 1.000000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 1.000000 0.000000 0.000000
        H4 1.000000 0.000000 0.000000 0.000000
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Test summary -------------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 0.000000 0.000000
        H2 0.000000 0.000000 0.000000 0.000000
        H3 0.000000 0.000000 0.000000 0.000000
        H4 0.000000 0.000000 0.000000 0.000000
      
      Test details - Critical values -------------------------------------------------
         Intersection Hypothesis       Test    p <= Critical * Alpha Reject
                   NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H4 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H1 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H4 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H1 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                   NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H2 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                   NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                   NA         H2 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H4 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                   NA         H3 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                   NA         H4 bonferroni 0.01 <=      1.0 * 0.025   TRUE
         intersection
                    1
                    1
                    1
                    1
                    2
                    2
                    2
                    3
                    3
                    3
                    4
                    4
                    5
                    5
                    5
                    6
                    6
                    7
                    7
                    8
                    9
                    9
                    9
                   10
                   10
                   11
                   11
                   12
                   13
                   13
                   14
                   15

# printing parametric closure test

    Code
      test_graph_closure(par_gate, rep(0.01, 4), test_types = "p", corr = diag(4))
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.500000
        H2: 0.500000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 1.000000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 1.000000 0.000000 0.000000
        H4 1.000000 0.000000 0.000000 0.000000
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1-H2-H3-H4)
      
      Test summary -------------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1       0.0199   TRUE
                H2       0.0199   TRUE
                H3       0.0199   TRUE
                H4       0.0199   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 0.000000 0.000000
        H2 0.000000 0.000000 0.000000 0.000000
        H3 0.000000 0.000000 0.000000 0.000000
        H4 0.000000 0.000000 0.000000 0.000000

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), groups = list(1:2, 3:4), test_types = c(
        "p", "s"), corr = diag(4), critical = TRUE, verbose = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.500000
        H2: 0.500000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 1.000000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 1.000000 0.000000 0.000000
        H4 1.000000 0.000000 0.000000 0.000000
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1-H2)
             simes: (H3-H4)
      
      Test summary -------------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 0.000000 0.000000
        H2 0.000000 0.000000 0.000000 0.000000
        H3 0.000000 0.000000 0.000000 0.000000
        H4 0.000000 0.000000 0.000000 0.000000
      
      Test details - Adjusted p ------------------------------------------------------
            H1  H2  H3  H4 padj_grp1 padj_grp2 p_adj_inter res
        1  0.5 0.5 0.0 0.0    0.0199      1.00      0.0199   1
        2  0.5 0.5 0.0  NA    0.0199      1.00      0.0199   1
        3  0.5 0.5  NA 0.0    0.0199      1.00      0.0199   1
        4  0.5 0.5  NA  NA    0.0199      1.00      0.0199   1
        5  0.5  NA 0.0 0.5    0.0200      0.02      0.0200   1
        6  1.0  NA 0.0  NA    0.0100      1.00      0.0100   1
        7  0.5  NA  NA 0.5    0.0200      0.02      0.0200   1
        8  1.0  NA  NA  NA    0.0100      1.00      0.0100   1
        9   NA 0.5 0.5 0.0    0.0200      0.02      0.0200   1
        10  NA 0.5 0.5  NA    0.0200      0.02      0.0200   1
        11  NA 1.0  NA 0.0    0.0100      1.00      0.0100   1
        12  NA 1.0  NA  NA    0.0100      1.00      0.0100   1
        13  NA  NA 0.5 0.5    1.0000      0.01      0.0100   1
        14  NA  NA 1.0  NA    1.0000      0.01      0.0100   1
        15  NA  NA  NA 1.0    1.0000      0.01      0.0100   1
      
      Test details - Critical values -------------------------------------------------
         Intersection Hypothesis       Test    p <=        c * Critical * Alpha Reject
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 0.0 * 0.025  FALSE
                   NA         H4      simes 0.01 <=                 0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H4      simes 0.01 <=                 0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H1 parametric 0.01 <=        1 *      0.5 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 0.5 * 0.025   TRUE
                   NA         H4      simes 0.01 <=                 0.5 * 0.025   TRUE
                   NA         H1 parametric 0.01 <=        1 *      1.0 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <=        1 *      0.5 * 0.025   TRUE
                   NA         H4      simes 0.01 <=                 0.5 * 0.025   TRUE
                   NA         H1 parametric 0.01 <=        1 *      1.0 * 0.025   TRUE
                   NA         H2 parametric 0.01 <=        1 *      0.5 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 0.5 * 0.025   TRUE
                   NA         H4      simes 0.01 <=                 0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <=        1 *      0.5 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <=        1 *      1.0 * 0.025   TRUE
                   NA         H4      simes 0.01 <=                 0.0 * 0.025  FALSE
                   NA         H2 parametric 0.01 <=        1 *      1.0 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 1.0 * 0.025   TRUE
                   NA         H4      simes 0.01 <=                 1.0 * 0.025   TRUE
                   NA         H3      simes 0.01 <=                 1.0 * 0.025   TRUE
                   NA         H4      simes 0.01 <=                 1.0 * 0.025   TRUE
         intersection
                    1
                    1
                    1
                    1
                    2
                    2
                    2
                    3
                    3
                    3
                    4
                    4
                    5
                    5
                    5
                    6
                    6
                    7
                    7
                    8
                    9
                    9
                    9
                   10
                   10
                   11
                   11
                   12
                   13
                   13
                   14
                   15

---

    Code
      test_graph_closure(par_gate, rep(0.01, 4), groups = list(1:2, 3:4), test_types = c(
        "p", "p"), corr = diag(4), critical = TRUE, verbose = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.500000
        H2: 0.500000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 1.000000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 1.000000 0.000000 0.000000
        H4 1.000000 0.000000 0.000000 0.000000
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1-H2)
        parametric: (H3-H4)
      
      Test summary -------------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 0.000000 0.000000
        H2 0.000000 0.000000 0.000000 0.000000
        H3 0.000000 0.000000 0.000000 0.000000
        H4 0.000000 0.000000 0.000000 0.000000
      
      Test details - Adjusted p ------------------------------------------------------
            H1  H2  H3  H4 padj_grp1 padj_grp2 p_adj_inter res
        1  0.5 0.5 0.0 0.0    0.0199    1.0000      0.0199   1
        2  0.5 0.5 0.0  NA    0.0199    1.0000      0.0199   1
        3  0.5 0.5  NA 0.0    0.0199    1.0000      0.0199   1
        4  0.5 0.5  NA  NA    0.0199    1.0000      0.0199   1
        5  0.5  NA 0.0 0.5    0.0200    0.0200      0.0200   1
        6  1.0  NA 0.0  NA    0.0100    1.0000      0.0100   1
        7  0.5  NA  NA 0.5    0.0200    0.0200      0.0200   1
        8  1.0  NA  NA  NA    0.0100    1.0000      0.0100   1
        9   NA 0.5 0.5 0.0    0.0200    0.0200      0.0200   1
        10  NA 0.5 0.5  NA    0.0200    0.0200      0.0200   1
        11  NA 1.0  NA 0.0    0.0100    1.0000      0.0100   1
        12  NA 1.0  NA  NA    0.0100    1.0000      0.0100   1
        13  NA  NA 0.5 0.5    1.0000    0.0199      0.0199   1
        14  NA  NA 1.0  NA    1.0000    0.0100      0.0100   1
        15  NA  NA  NA 1.0    1.0000    0.0100      0.0100   1
      
      Test details - Critical values -------------------------------------------------
         Intersection Hypothesis       Test    p <=        c * Critical * Alpha Reject
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H4 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H4 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H1 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H4 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H1 parametric 0.01 <= 1.000000 *      1.0 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H1 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H4 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H1 parametric 0.01 <= 1.000000 *      1.0 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H4 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H2 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.000000 *      0.5 * 0.025   TRUE
                   NA         H2 parametric 0.01 <= 1.000000 *      1.0 * 0.025   TRUE
                   NA         H4 parametric 0.01 <= 1.000000 *      0.0 * 0.025  FALSE
                   NA         H2 parametric 0.01 <= 1.000000 *      1.0 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H4 parametric 0.01 <= 1.006329 *      0.5 * 0.025   TRUE
                   NA         H3 parametric 0.01 <= 1.000000 *      1.0 * 0.025   TRUE
                   NA         H4 parametric 0.01 <= 1.000000 *      1.0 * 0.025   TRUE
         intersection
                    1
                    1
                    1
                    1
                    2
                    2
                    2
                    3
                    3
                    3
                    4
                    4
                    5
                    5
                    5
                    6
                    6
                    7
                    7
                    8
                    9
                    9
                    9
                   10
                   10
                   11
                   11
                   12
                   13
                   13
                   14
                   15

# printing Bonferroni sequential results

    Code
      test_graph_shortcut(simple_successive_1(), rep(0.01, 4))
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.500000
        H2: 0.500000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 1.000000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 1.000000 0.000000 0.000000
        H4 1.000000 0.000000 0.000000 0.000000
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Test summary -------------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Updated graph after rejections
      
        --- Hypothesis weights ---
        H1: 0.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.000000 0.000000 0.000000
        H2 0.000000 0.000000 0.000000 0.000000
        H3 0.000000 0.000000 0.000000 0.000000
        H4 0.000000 0.000000 0.000000 0.000000

# additional printing options for graph report

    Code
      print(test_graph_closure(par_gate, rep(0.01, 4), verbose = TRUE, critical = TRUE),
      precison = 4, indent = 4)
    Output
      
      Test parameters ----------------------------------------------------------------
          Initial graph
      
          --- Hypothesis weights ---
          H1: 0.500000
          H2: 0.500000
          H3: 0.000000
          H4: 0.000000
      
          --- Transition weights ---
                   H1       H2       H3       H4
          H1 0.000000 0.000000 1.000000 0.000000
          H2 0.000000 0.000000 0.000000 1.000000
          H3 0.000000 1.000000 0.000000 0.000000
          H4 1.000000 0.000000 0.000000 0.000000
      
          Alpha = 0.025
      
                                 H1   H2   H3   H4
          Unadjusted p-values: 0.01 0.01 0.01 0.01
      
          Test types
          bonferroni: (H1-H2-H3-H4)
      
      Test summary -------------------------------------------------------------------
          Hypothesis Adj. P-value Reject
                  H1         0.02   TRUE
                  H2         0.02   TRUE
                  H3         0.02   TRUE
                  H4         0.02   TRUE
      
          Updated graph after rejections
      
          --- Hypothesis weights ---
          H1: 0.000000
          H2: 0.000000
          H3: 0.000000
          H4: 0.000000
      
          --- Transition weights ---
                   H1       H2       H3       H4
          H1 0.000000 0.000000 0.000000 0.000000
          H2 0.000000 0.000000 0.000000 0.000000
          H3 0.000000 0.000000 0.000000 0.000000
          H4 0.000000 0.000000 0.000000 0.000000
      
      Test details - Adjusted p ------------------------------------------------------
              H1  H2  H3  H4 padj_grp1 p_adj_inter res
          1  0.5 0.5 0.0 0.0      0.02        0.02   1
          2  0.5 0.5 0.0  NA      0.02        0.02   1
          3  0.5 0.5  NA 0.0      0.02        0.02   1
          4  0.5 0.5  NA  NA      0.02        0.02   1
          5  0.5  NA 0.0 0.5      0.02        0.02   1
          6  1.0  NA 0.0  NA      0.01        0.01   1
          7  0.5  NA  NA 0.5      0.02        0.02   1
          8  1.0  NA  NA  NA      0.01        0.01   1
          9   NA 0.5 0.5 0.0      0.02        0.02   1
          10  NA 0.5 0.5  NA      0.02        0.02   1
          11  NA 1.0  NA 0.0      0.01        0.01   1
          12  NA 1.0  NA  NA      0.01        0.01   1
          13  NA  NA 0.5 0.5      0.02        0.02   1
          14  NA  NA 1.0  NA      0.01        0.01   1
          15  NA  NA  NA 1.0      0.01        0.01   1
      
      Test details - Critical values -------------------------------------------------
           Intersection Hypothesis       Test    p <= Critical * Alpha Reject
                     NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H4 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H1 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H1 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H4 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H1 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                     NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H2 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H2 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                     NA         H4 bonferroni 0.01 <=      0.0 * 0.025  FALSE
                     NA         H2 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H4 bonferroni 0.01 <=      0.5 * 0.025   TRUE
                     NA         H3 bonferroni 0.01 <=      1.0 * 0.025   TRUE
                     NA         H4 bonferroni 0.01 <=      1.0 * 0.025   TRUE
           intersection
                      1
                      1
                      1
                      1
                      2
                      2
                      2
                      3
                      3
                      3
                      4
                      4
                      5
                      5
                      5
                      6
                      6
                      7
                      7
                      8
                      9
                      9
                      9
                     10
                     10
                     11
                     11
                     12
                     13
                     13
                     14
                     15

---

    Code
      print(test_graph_shortcut(simple_successive_1(), rep(0.01, 4), critical = TRUE),
      precision = 7, indent = 9)
    Output
      
      Test parameters ----------------------------------------------------------------
               Initial graph
      
               --- Hypothesis weights ---
               H1: 0.5000000
               H2: 0.5000000
               H3: 0.0000000
               H4: 0.0000000
      
               --- Transition weights ---
                         H1        H2        H3        H4
               H1 0.0000000 0.0000000 1.0000000 0.0000000
               H2 0.0000000 0.0000000 0.0000000 1.0000000
               H3 0.0000000 1.0000000 0.0000000 0.0000000
               H4 1.0000000 0.0000000 0.0000000 0.0000000
      
               Alpha = 0.025
      
                                      H1   H2   H3   H4
               Unadjusted p-values: 0.01 0.01 0.01 0.01
      
               Test types
               bonferroni: (H1-H2-H3-H4)
      
      Test summary -------------------------------------------------------------------
               Hypothesis Adj. P-value Reject
                       H1         0.02   TRUE
                       H2         0.02   TRUE
                       H3         0.02   TRUE
                       H4         0.02   TRUE
      
               Updated graph after rejections
      
               --- Hypothesis weights ---
               H1: 0.0000000
               H2: 0.0000000
               H3: 0.0000000
               H4: 0.0000000
      
               --- Transition weights ---
                         H1        H2        H3        H4
               H1 0.0000000 0.0000000 0.0000000 0.0000000
               H2 0.0000000 0.0000000 0.0000000 0.0000000
               H3 0.0000000 0.0000000 0.0000000 0.0000000
               H4 0.0000000 0.0000000 0.0000000 0.0000000
      
      Test details - Critical values -------------------------------------------------
                step Hypothesis    p <= Critical * Alpha Reject
                   1         H1 0.01 <=      0.5 * 0.025   TRUE
                   2         H2 0.01 <=      0.5 * 0.025   TRUE
                   3         H3 0.01 <=      0.5 * 0.025   TRUE
                   4         H4 0.01 <=      1.0 * 0.025   TRUE

