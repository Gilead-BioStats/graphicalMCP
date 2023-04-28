# printing bare minimum

    Code
      test_graph(par_gate, rep(0.01, 4))
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

---

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

