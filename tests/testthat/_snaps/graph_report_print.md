# printing Bonferroni/Simes closure test

    Code
      test_graph(par_gate, rep(0.01, 4), test_types = "s")
    Output
      
      Test parameters ----------------------------------------------------------------
        An initial graph
        
        --- Hypothesis weights ---
        H1: 0.5000
        H2: 0.5000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.0000 1.0000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 1.0000 0.0000 0.0000
        H4 1.0000 0.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        simes: (H1-H2-H3-H4)
      
      Global test summary ------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.01   TRUE
                H2         0.01   TRUE
                H3         0.01   TRUE
                H4         0.01   TRUE

---

    Code
      test_graph(par_gate, rep(0.01, 4), verbose = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        An initial graph
        
        --- Hypothesis weights ---
        H1: 0.5000
        H2: 0.5000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.0000 1.0000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 1.0000 0.0000 0.0000
        H4 1.0000 0.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Global test summary ------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
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
      test_graph(par_gate, rep(0.01, 4), critical = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        An initial graph
        
        --- Hypothesis weights ---
        H1: 0.5000
        H2: 0.5000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.0000 1.0000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 1.0000 0.0000 0.0000
        H4 1.0000 0.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Global test summary ------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
      Test details - Critical values -------------------------------------------------
         intersection hypothesis       test    p <=   w * alpha   res
                    1         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    1         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    1         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                    1         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                    2         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    2         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    2         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                    3         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    3         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    3         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                    4         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    4         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    5         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    5         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                    5         H4 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    6         H1 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                    6         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                    7         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    7         H4 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    8         H1 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                    9         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    9         H3 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                    9         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                   10         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                   10         H3 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                   11         H2 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                   11         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                   12         H2 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                   13         H3 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                   13         H4 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                   14         H3 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                   15         H4 bonferroni 0.01 <= 1.0 *  0.05  TRUE

# printing parametric closure test

    Code
      test_graph(par_gate, rep(0.01, 4), test_types = "p", corr = diag(4))
    Output
      
      Test parameters ----------------------------------------------------------------
        An initial graph
        
        --- Hypothesis weights ---
        H1: 0.5000
        H2: 0.5000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.0000 1.0000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 1.0000 0.0000 0.0000
        H4 1.0000 0.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1-H2-H3-H4)
      
      Global test summary ------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1       0.0199   TRUE
                H2       0.0199   TRUE
                H3       0.0199   TRUE
                H4       0.0199   TRUE

---

    Code
      test_graph(par_gate, rep(0.01, 4), groups = list(1:2, 3:4), test_types = c("p",
        "s"), corr = diag(4), critical = TRUE, verbose = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        An initial graph
        
        --- Hypothesis weights ---
        H1: 0.5000
        H2: 0.5000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.0000 1.0000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 1.0000 0.0000 0.0000
        H4 1.0000 0.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
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
      
      Global test summary ------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
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
         intersection hypothesis       test    p <=        c *   w * alpha   res
                    1         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    1         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    1         H3      simes 0.01 <=            0.0 *  0.05 FALSE
                    1         H4      simes 0.01 <=            0.0 *  0.05 FALSE
                    2         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    2         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    2         H3      simes 0.01 <=            0.0 *  0.05 FALSE
                    3         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    3         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    3         H4      simes 0.01 <=            0.0 *  0.05 FALSE
                    4         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    4         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    5         H1 parametric 0.01 <=        1 * 0.5 *  0.05  TRUE
                    5         H3      simes 0.01 <=            0.5 *  0.05  TRUE
                    5         H4      simes 0.01 <=            0.5 *  0.05  TRUE
                    6         H1 parametric 0.01 <=        1 * 1.0 *  0.05  TRUE
                    6         H3      simes 0.01 <=            0.0 *  0.05 FALSE
                    7         H1 parametric 0.01 <=        1 * 0.5 *  0.05  TRUE
                    7         H4      simes 0.01 <=            0.5 *  0.05  TRUE
                    8         H1 parametric 0.01 <=        1 * 1.0 *  0.05  TRUE
                    9         H2 parametric 0.01 <=        1 * 0.5 *  0.05  TRUE
                    9         H3      simes 0.01 <=            0.5 *  0.05  TRUE
                    9         H4      simes 0.01 <=            0.5 *  0.05  TRUE
                   10         H2 parametric 0.01 <=        1 * 0.5 *  0.05  TRUE
                   10         H3      simes 0.01 <=            0.5 *  0.05  TRUE
                   11         H2 parametric 0.01 <=        1 * 1.0 *  0.05  TRUE
                   11         H4      simes 0.01 <=            0.0 *  0.05 FALSE
                   12         H2 parametric 0.01 <=        1 * 1.0 *  0.05  TRUE
                   13         H3      simes 0.01 <=            1.0 *  0.05  TRUE
                   13         H4      simes 0.01 <=            1.0 *  0.05  TRUE
                   14         H3      simes 0.01 <=            1.0 *  0.05  TRUE
                   15         H4      simes 0.01 <=            1.0 *  0.05  TRUE

---

    Code
      test_graph(par_gate, rep(0.01, 4), groups = list(1:2, 3:4), test_types = c("p",
        "p"), corr = diag(4), critical = TRUE, verbose = TRUE)
    Output
      
      Test parameters ----------------------------------------------------------------
        An initial graph
        
        --- Hypothesis weights ---
        H1: 0.5000
        H2: 0.5000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.0000 1.0000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 1.0000 0.0000 0.0000
        H4 1.0000 0.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
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
      
      Global test summary ------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
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
         intersection hypothesis       test    p <=        c *   w * alpha   res
                    1         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    1         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    1         H3 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                    1         H4 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                    2         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    2         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    2         H3 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                    3         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    3         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    3         H4 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                    4         H1 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    4         H2 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                    5         H1 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                    5         H3 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                    5         H4 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                    6         H1 parametric 0.01 <= 1.000000 * 1.0 *  0.05  TRUE
                    6         H3 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                    7         H1 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                    7         H4 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                    8         H1 parametric 0.01 <= 1.000000 * 1.0 *  0.05  TRUE
                    9         H2 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                    9         H3 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                    9         H4 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                   10         H2 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                   10         H3 parametric 0.01 <= 1.000000 * 0.5 *  0.05  TRUE
                   11         H2 parametric 0.01 <= 1.000000 * 1.0 *  0.05  TRUE
                   11         H4 parametric 0.01 <= 1.000000 * 0.0 *  0.05 FALSE
                   12         H2 parametric 0.01 <= 1.000000 * 1.0 *  0.05  TRUE
                   13         H3 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                   13         H4 parametric 0.01 <= 1.012827 * 0.5 *  0.05  TRUE
                   14         H3 parametric 0.01 <= 1.000000 * 1.0 *  0.05  TRUE
                   15         H4 parametric 0.01 <= 1.000000 * 1.0 *  0.05  TRUE

# printing Bonferroni sequential results

    Code
      bonferroni_sequential(simple_successive_1(), rep(0.01, 4))
    Output
      
      Test parameters ----------------------------------------------------------------
        An initial graph
        
        --- Hypothesis weights ---
        H1: 0.5000
        H2: 0.5000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.0000 1.0000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 1.0000 0.0000 0.0000
        H4 1.0000 0.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Global test summary ------------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE

# additional printing options for graph report

    Code
      print(test_graph(par_gate, rep(0.01, 4), verbose = TRUE, critical = TRUE),
      precison = 4, indent = 4)
    Output
      
      Test parameters ----------------------------------------------------------------
          An initial graph
          
          --- Hypothesis weights ---
          H1: 0.5000
          H2: 0.5000
          H3: 0.0000
          H4: 0.0000
          
          --- Transition weights ---
                 H1     H2     H3     H4
          H1 0.0000 0.0000 1.0000 0.0000
          H2 0.0000 0.0000 0.0000 1.0000
          H3 0.0000 1.0000 0.0000 0.0000
          H4 1.0000 0.0000 0.0000 0.0000
      
          Global alpha = 0.05
      
                                 H1   H2   H3   H4
          Unadjusted p-values: 0.01 0.01 0.01 0.01
      
          Test types
          bonferroni: (H1-H2-H3-H4)
      
      Global test summary ------------------------------------------------------------
          Hypothesis Adj. P-value Reject
                  H1         0.02   TRUE
                  H2         0.02   TRUE
                  H3         0.02   TRUE
                  H4         0.02   TRUE
      
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
           intersection hypothesis       test    p <=   w * alpha   res
                      1         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      1         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      1         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                      1         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                      2         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      2         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      2         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                      3         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      3         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      3         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                      4         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      4         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      5         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      5         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                      5         H4 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      6         H1 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                      6         H3 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                      7         H1 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      7         H4 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      8         H1 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                      9         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      9         H3 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                      9         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                     10         H2 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                     10         H3 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                     11         H2 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                     11         H4 bonferroni 0.01 <= 0.0 *  0.05 FALSE
                     12         H2 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                     13         H3 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                     13         H4 bonferroni 0.01 <= 0.5 *  0.05  TRUE
                     14         H3 bonferroni 0.01 <= 1.0 *  0.05  TRUE
                     15         H4 bonferroni 0.01 <= 1.0 *  0.05  TRUE

---

    Code
      print(bonferroni_sequential(simple_successive_1(), rep(0.01, 4), critical = TRUE),
      precision = 7, indent = 9)
    Output
      
      Test parameters ----------------------------------------------------------------
               An initial graph
               
               --- Hypothesis weights ---
               H1: 0.5000
               H2: 0.5000
               H3: 0.0000
               H4: 0.0000
               
               --- Transition weights ---
                      H1     H2     H3     H4
               H1 0.0000 0.0000 1.0000 0.0000
               H2 0.0000 0.0000 0.0000 1.0000
               H3 0.0000 1.0000 0.0000 0.0000
               H4 1.0000 0.0000 0.0000 0.0000
      
               Global alpha = 0.05
      
                                      H1   H2   H3   H4
               Unadjusted p-values: 0.01 0.01 0.01 0.01
      
               Test types
               bonferroni: (H1-H2-H3-H4)
      
      Global test summary ------------------------------------------------------------
               Hypothesis Adj. P-value Reject
                       H1         0.02   TRUE
                       H2         0.02   TRUE
                       H3         0.02   TRUE
                       H4         0.02   TRUE
      
      Test details - Critical values -------------------------------------------------
                step hypothesis       test    p <=   w * alpha  res
                   1         H1 bonferroni 0.01 <= 0.5 *  0.05 TRUE
                   2         H2 bonferroni 0.01 <= 0.5 *  0.05 TRUE
                   3         H3 bonferroni 0.01 <= 0.5 *  0.05 TRUE
                   4         H4 bonferroni 0.01 <= 1.0 *  0.05 TRUE

