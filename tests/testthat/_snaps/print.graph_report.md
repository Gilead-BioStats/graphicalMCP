# printing Bonferroni/Simes closure test

    Code
      graph_test_closure(par_gate, rep(0.01, 4), test_types = "s")
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        simes: (H1, H2, H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.01   TRUE
                H2         0.01   TRUE
                H3         0.01   TRUE
                H4         0.01   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3: NA
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA NA NA
        H4 NA NA NA NA
      

---

    Code
      graph_test_closure(par_gate, rep(0.01, 4), verbose = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1, H2, H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3: NA
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA NA NA
        H4 NA NA NA NA
      
      Adjusted p details ($details) --------------------------------------------------
        Intersection   H1   H2   H3   H4 adj_p_grp1 adj_p_inter reject_intersection
                   1 0.50 0.50 0.00 0.00       0.02        0.02                TRUE
                   2 0.50 0.50 0.00   NA       0.02        0.02                TRUE
                   3 0.50 0.50   NA 0.00       0.02        0.02                TRUE
                   4 0.50 0.50   NA   NA       0.02        0.02                TRUE
                   5 0.50   NA 0.00 0.50       0.02        0.02                TRUE
                   6 1.00   NA 0.00   NA       0.01        0.01                TRUE
                   7 0.50   NA   NA 0.50       0.02        0.02                TRUE
                   8 1.00   NA   NA   NA       0.01        0.01                TRUE
                   9   NA 0.50 0.50 0.00       0.02        0.02                TRUE
                  10   NA 0.50 0.50   NA       0.02        0.02                TRUE
        ... (Use `print(x, rows = <nn>)` for more)
      

---

    Code
      graph_test_closure(par_gate, rep(0.01, 4), test_values = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1, H2, H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3: NA
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA NA NA
        H4 NA NA NA NA
      
      Detailed test values ($test_values) --------------------------------------------
        Intersection Hypothesis       Test    p <= Weight * Alpha Inequality_holds
                   1         H1 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                   1         H2 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                   1         H3 bonferroni 0.01 <=    0.0 * 0.025            FALSE
                   1         H4 bonferroni 0.01 <=    0.0 * 0.025            FALSE
                   2         H1 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                   2         H2 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                   2         H3 bonferroni 0.01 <=    0.0 * 0.025            FALSE
                   3         H1 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                   3         H2 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                   3         H4 bonferroni 0.01 <=    0.0 * 0.025            FALSE
        ... (Use `print(x, rows = <nn>)` for more)
      

# printing parametric closure test

    Code
      graph_test_closure(par_gate, rep(0.01, 4), test_types = "p", test_corr = list(
        diag(4)))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0  0  0
                            H2  0  1  0  0
                            H3  0  0  1  0
                            H4  0  0  0  1
      
        Test types
        parametric: (H1, H2, H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1       0.0199   TRUE
                H2       0.0199   TRUE
                H3       0.0199   TRUE
                H4       0.0199   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3: NA
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA NA NA
        H4 NA NA NA NA
      

---

    Code
      graph_test_closure(par_gate, rep(0.01, 4), test_groups = list(1:2, 3:4),
      test_types = c("p", "s"), test_corr = list(diag(2), NA), test_values = TRUE,
      verbose = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2
                            H1  1  0
                            H2  0  1
      
        Test types
        parametric: (H1, H2)
             simes: (H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3: NA
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA NA NA
        H4 NA NA NA NA
      
      Adjusted p details ($details) --------------------------------------------------
        Intersection     H1     H2     H3     H4 adj_p_grp1 adj_p_grp2 adj_p_inter
                   1 0.5000 0.5000 0.0000 0.0000     0.0199     1.0000      0.0199
                   2 0.5000 0.5000 0.0000     NA     0.0199     1.0000      0.0199
                   3 0.5000 0.5000     NA 0.0000     0.0199     1.0000      0.0199
                   4 0.5000 0.5000     NA     NA     0.0199     1.0000      0.0199
                   5 0.5000     NA 0.0000 0.5000     0.0200     0.0200      0.0200
                   6 1.0000     NA 0.0000     NA     0.0100     1.0000      0.0100
                   7 0.5000     NA     NA 0.5000     0.0200     0.0200      0.0200
                   8 1.0000     NA     NA     NA     0.0100     1.0000      0.0100
                   9     NA 0.5000 0.5000 0.0000     0.0200     0.0200      0.0200
                  10     NA 0.5000 0.5000     NA     0.0200     0.0200      0.0200
        reject_intersection
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
        ... (Use `print(x, rows = <nn>)` for more)
      
      Detailed test values ($test_values) --------------------------------------------
        Intersection Hypothesis       Test    p <= c_value * Weight * Alpha
                   1         H1 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   1         H2 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   1         H3      simes 0.01 <=              0.0 * 0.025
                   1         H4      simes 0.01 <=              0.0 * 0.025
                   2         H1 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   2         H2 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   2         H3      simes 0.01 <=              0.0 * 0.025
                   3         H1 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   3         H2 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   3         H4      simes 0.01 <=              0.0 * 0.025
        Inequality_holds
                    TRUE
                    TRUE
                   FALSE
                   FALSE
                    TRUE
                    TRUE
                   FALSE
                    TRUE
                    TRUE
                   FALSE
        ... (Use `print(x, rows = <nn>)` for more)
      

---

    Code
      graph_test_closure(par_gate, rep(0.01, 4), test_groups = list(1:2, 3:4),
      test_types = c("p", "p"), test_corr = list(diag(2), diag(2)), test_values = TRUE,
      verbose = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Correlation matrix:    H1 H2 H3 H4
                            H1  1  0 NA NA
                            H2  0  1 NA NA
                            H3 NA NA  1  0
                            H4 NA NA  0  1
      
        Test types
        parametric: (H1, H2)
        parametric: (H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3: NA
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA NA NA
        H4 NA NA NA NA
      
      Adjusted p details ($details) --------------------------------------------------
        Intersection     H1     H2     H3     H4 adj_p_grp1 adj_p_grp2 adj_p_inter
                   1 0.5000 0.5000 0.0000 0.0000     0.0199     1.0000      0.0199
                   2 0.5000 0.5000 0.0000     NA     0.0199     1.0000      0.0199
                   3 0.5000 0.5000     NA 0.0000     0.0199     1.0000      0.0199
                   4 0.5000 0.5000     NA     NA     0.0199     1.0000      0.0199
                   5 0.5000     NA 0.0000 0.5000     0.0200     0.0200      0.0200
                   6 1.0000     NA 0.0000     NA     0.0100     1.0000      0.0100
                   7 0.5000     NA     NA 0.5000     0.0200     0.0200      0.0200
                   8 1.0000     NA     NA     NA     0.0100     1.0000      0.0100
                   9     NA 0.5000 0.5000 0.0000     0.0200     0.0200      0.0200
                  10     NA 0.5000 0.5000     NA     0.0200     0.0200      0.0200
        reject_intersection
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
                       TRUE
        ... (Use `print(x, rows = <nn>)` for more)
      
      Detailed test values ($test_values) --------------------------------------------
        Intersection Hypothesis       Test    p <= c_value * Weight * Alpha
                   1         H1 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   1         H2 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   1         H3 parametric 0.01 <=   1.000 *    0.0 * 0.025
                   1         H4 parametric 0.01 <=   1.000 *    0.0 * 0.025
                   2         H1 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   2         H2 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   2         H3 parametric 0.01 <=   1.000 *    0.0 * 0.025
                   3         H1 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   3         H2 parametric 0.01 <=   1.006 *    0.5 * 0.025
                   3         H4 parametric 0.01 <=   1.000 *    0.0 * 0.025
        Inequality_holds
                    TRUE
                    TRUE
                   FALSE
                   FALSE
                    TRUE
                    TRUE
                   FALSE
                    TRUE
                    TRUE
                   FALSE
        ... (Use `print(x, rows = <nn>)` for more)
      

# printing Bonferroni sequential results

    Code
      graph_test_shortcut(simple_successive_1(), rep(0.01, 4))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.5
        H3: 0.0
        H4: 0.0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  0  1  0
        H2  0  0  0  1
        H3  0  1  0  0
        H4  1  0  0  0
      
        Alpha = 0.025
      
                               H1   H2   H3   H4
        Unadjusted p-values: 0.01 0.01 0.01 0.01
      
        Test types
        bonferroni: (H1, H2, H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1         0.02   TRUE
                H2         0.02   TRUE
                H3         0.02   TRUE
                H4         0.02   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3: NA
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA NA NA
        H4 NA NA NA NA
      

# additional printing options for graph report

    Code
      print(graph_test_closure(par_gate, rep(0.01, 4), verbose = TRUE, test_values = TRUE),
      precison = 4, indent = 4)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
          Initial graph
      
          --- Hypothesis weights ---
          H1: 0.5
          H2: 0.5
          H3: 0.0
          H4: 0.0
      
          --- Transition weights ---
             H1 H2 H3 H4
          H1  0  0  1  0
          H2  0  0  0  1
          H3  0  1  0  0
          H4  1  0  0  0
      
          Alpha = 0.025
      
                                 H1   H2   H3   H4
          Unadjusted p-values: 0.01 0.01 0.01 0.01
      
          Test types
          bonferroni: (H1, H2, H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
          Hypothesis Adj. P-value Reject
                  H1         0.02   TRUE
                  H2         0.02   TRUE
                  H3         0.02   TRUE
                  H4         0.02   TRUE
      
          Final updated graph after removing rejected hypotheses
      
          --- Hypothesis weights ---
          H1: NA
          H2: NA
          H3: NA
          H4: NA
      
          --- Transition weights ---
             H1 H2 H3 H4
          H1 NA NA NA NA
          H2 NA NA NA NA
          H3 NA NA NA NA
          H4 NA NA NA NA
      
      Adjusted p details ($details) --------------------------------------------------
          Intersection   H1   H2   H3   H4 adj_p_grp1 adj_p_inter reject_intersection
                     1 0.50 0.50 0.00 0.00       0.02        0.02                TRUE
                     2 0.50 0.50 0.00   NA       0.02        0.02                TRUE
                     3 0.50 0.50   NA 0.00       0.02        0.02                TRUE
                     4 0.50 0.50   NA   NA       0.02        0.02                TRUE
                     5 0.50   NA 0.00 0.50       0.02        0.02                TRUE
                     6 1.00   NA 0.00   NA       0.01        0.01                TRUE
                     7 0.50   NA   NA 0.50       0.02        0.02                TRUE
                     8 1.00   NA   NA   NA       0.01        0.01                TRUE
                     9   NA 0.50 0.50 0.00       0.02        0.02                TRUE
                    10   NA 0.50 0.50   NA       0.02        0.02                TRUE
          ... (Use `print(x, rows = <nn>)` for more)
      
      Detailed test values ($test_values) --------------------------------------------
          Intersection Hypothesis       Test    p <= Weight * Alpha Inequality_holds
                     1         H1 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                     1         H2 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                     1         H3 bonferroni 0.01 <=    0.0 * 0.025            FALSE
                     1         H4 bonferroni 0.01 <=    0.0 * 0.025            FALSE
                     2         H1 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                     2         H2 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                     2         H3 bonferroni 0.01 <=    0.0 * 0.025            FALSE
                     3         H1 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                     3         H2 bonferroni 0.01 <=    0.5 * 0.025             TRUE
                     3         H4 bonferroni 0.01 <=    0.0 * 0.025            FALSE
          ... (Use `print(x, rows = <nn>)` for more)
      

---

    Code
      print(graph_test_shortcut(simple_successive_1(), rep(0.01, 4), verbose = TRUE,
      test_values = TRUE), precision = 7, indent = 9)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
               Initial graph
      
               --- Hypothesis weights ---
               H1: 0.5
               H2: 0.5
               H3: 0.0
               H4: 0.0
      
               --- Transition weights ---
                  H1 H2 H3 H4
               H1  0  0  1  0
               H2  0  0  0  1
               H3  0  1  0  0
               H4  1  0  0  0
      
               Alpha = 0.025
      
                                      H1   H2   H3   H4
               Unadjusted p-values: 0.01 0.01 0.01 0.01
      
               Test types
               bonferroni: (H1, H2, H3, H4)
      
      Test summary ($outputs) --------------------------------------------------------
               Hypothesis Adj. P-value Reject
                       H1         0.02   TRUE
                       H2         0.02   TRUE
                       H3         0.02   TRUE
                       H4         0.02   TRUE
      
               Final updated graph after removing rejected hypotheses
      
               --- Hypothesis weights ---
               H1: NA
               H2: NA
               H3: NA
               H4: NA
      
               --- Transition weights ---
                  H1 H2 H3 H4
               H1 NA NA NA NA
               H2 NA NA NA NA
               H3 NA NA NA NA
               H4 NA NA NA NA
      
      Rejection sequence details ($details) ------------------------------------------
               Initial graph
      
               --- Hypothesis weights ---
               H1: 0.5
               H2: 0.5
               H3: 0.0
               H4: 0.0
      
               --- Transition weights ---
                  H1 H2 H3 H4
               H1  0  0  1  0
               H2  0  0  0  1
               H3  0  1  0  0
               H4  1  0  0  0
      
                        Step 1: Updated graph after removing hypothesis H1
      
                        --- Hypothesis weights ---
                        H1:  NA
                        H2: 0.5
                        H3: 0.5
                        H4: 0.0
      
                        --- Transition weights ---
                           H1 H2 H3 H4
                        H1 NA NA NA NA
                        H2 NA  0  0  1
                        H3 NA  1  0  0
                        H4 NA  0  1  0
      
                                 Step 2: Updated graph after removing hypotheses H1, H2
      
                                 --- Hypothesis weights ---
                                 H1:  NA
                                 H2:  NA
                                 H3: 0.5
                                 H4: 0.5
      
                                 --- Transition weights ---
                                    H1 H2 H3 H4
                                 H1 NA NA NA NA
                                 H2 NA NA NA NA
                                 H3 NA NA  0  1
                                 H4 NA NA  1  0
      
                                          Step 3: Updated graph after removing hypotheses H1, H2, H3
      
                                          --- Hypothesis weights ---
                                          H1: NA
                                          H2: NA
                                          H3: NA
                                          H4:  1
      
                                          --- Transition weights ---
                                             H1 H2 H3 H4
                                          H1 NA NA NA NA
                                          H2 NA NA NA NA
                                          H3 NA NA NA NA
                                          H4 NA NA NA  0
      
                                                   Step 4: Updated graph after removing hypotheses H1, H2, H3, H4
      
                                                   --- Hypothesis weights ---
                                                   H1: NA
                                                   H2: NA
                                                   H3: NA
                                                   H4: NA
      
                                                   --- Transition weights ---
                                                      H1 H2 H3 H4
                                                   H1 NA NA NA NA
                                                   H2 NA NA NA NA
                                                   H3 NA NA NA NA
                                                   H4 NA NA NA NA
      
               Final updated graph after removing rejected hypotheses
      
               --- Hypothesis weights ---
               H1: NA
               H2: NA
               H3: NA
               H4: NA
      
               --- Transition weights ---
                  H1 H2 H3 H4
               H1 NA NA NA NA
               H2 NA NA NA NA
               H3 NA NA NA NA
               H4 NA NA NA NA
      
      Detailed test values ($test_values) --------------------------------------------
               Step Hypothesis    p <= Weight * Alpha Inequality_holds
                  1         H1 0.01 <=    0.5 * 0.025             TRUE
                  2         H2 0.01 <=    0.5 * 0.025             TRUE
                  3         H3 0.01 <=    0.5 * 0.025             TRUE
                  4         H4 0.01 <=    1.0 * 0.025             TRUE
      

---

    Code
      print(graph_test_shortcut(complex_example_1(), 5:0 / 200, verbose = TRUE,
      test_values = TRUE))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.0
        H3: 0.0
        H4: 0.5
        H5: 0.0
        H6: 0.0
      
        --- Transition weights ---
               H1     H2     H3     H4     H5     H6
        H1 0.0000 0.5000 0.5000 0.0000 0.0000 0.0000
        H2 0.0000 0.0000 1.0000 0.0000 0.0000 0.0000
        H3 0.0000 0.9999 0.0000 0.0001 0.0000 0.0000
        H4 0.0000 0.0000 0.0000 0.0000 0.5000 0.5000
        H5 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000
        H6 0.0001 0.0000 0.0000 0.0000 0.9999 0.0000
      
        Alpha = 0.025
      
                                H1    H2    H3    H4    H5    H6
        Unadjusted p-values: 0.025 0.020 0.015 0.010 0.005 0.000
      
        Test types
        bonferroni: (H1, H2, H3, H4, H5, H6)
      
      Test summary ($outputs) --------------------------------------------------------
        Hypothesis Adj. P-value Reject
                H1        0.025   TRUE
                H2        0.030  FALSE
                H3        0.030  FALSE
                H4        0.020   TRUE
                H5        0.020   TRUE
                H6        0.020   TRUE
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1:  NA
        H2: 0.5
        H3: 0.5
        H4:  NA
        H5:  NA
        H6:  NA
      
        --- Transition weights ---
           H1 H2 H3 H4 H5 H6
        H1 NA NA NA NA NA NA
        H2 NA  0  1 NA NA NA
        H3 NA  1  0 NA NA NA
        H4 NA NA NA NA NA NA
        H5 NA NA NA NA NA NA
        H6 NA NA NA NA NA NA
      
      Rejection sequence details ($details) ------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 0.5
        H2: 0.0
        H3: 0.0
        H4: 0.5
        H5: 0.0
        H6: 0.0
      
        --- Transition weights ---
               H1     H2     H3     H4     H5     H6
        H1 0.0000 0.5000 0.5000 0.0000 0.0000 0.0000
        H2 0.0000 0.0000 1.0000 0.0000 0.0000 0.0000
        H3 0.0000 0.9999 0.0000 0.0001 0.0000 0.0000
        H4 0.0000 0.0000 0.0000 0.0000 0.5000 0.5000
        H5 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000
        H6 0.0001 0.0000 0.0000 0.0000 0.9999 0.0000
      
          Step 1: Updated graph after removing hypothesis H4
      
          --- Hypothesis weights ---
          H1: 0.50
          H2: 0.00
          H3: 0.00
          H4:   NA
          H5: 0.25
          H6: 0.25
      
          --- Transition weights ---
                  H1      H2      H3      H4      H5      H6
          H1 0.00000 0.50000 0.50000      NA 0.00000 0.00000
          H2 0.00000 0.00000 1.00000      NA 0.00000 0.00000
          H3 0.00000 0.99990 0.00000      NA 0.00005 0.00005
          H4      NA      NA      NA      NA      NA      NA
          H5 0.00000 0.00000 0.00000      NA 0.00000 1.00000
          H6 0.00010 0.00000 0.00000      NA 0.99990 0.00000
      
            Step 2: Updated graph after removing hypotheses H4, H6
      
            --- Hypothesis weights ---
            H1: 0.5
            H2: 0.0
            H3: 0.0
            H4:  NA
            H5: 0.5
            H6:  NA
      
            --- Transition weights ---
                        H1          H2          H3          H4          H5
            H1 0.000000000 0.500000000 0.500000000          NA 0.000000000
            H2 0.000000000 0.000000000 1.000000000          NA 0.000000000
            H3 0.000000005 0.999900000 0.000000000          NA 0.000099995
            H4          NA          NA          NA          NA          NA
            H5 1.000000000 0.000000000 0.000000000          NA 0.000000000
            H6          NA          NA          NA          NA          NA
                H6
                NA
                NA
                NA
                NA
                NA
                NA
      
              Step 3: Updated graph after removing hypotheses H4, H6, H5
      
              --- Hypothesis weights ---
              H1:  1
              H2:  0
              H3:  0
              H4: NA
              H5: NA
              H6: NA
      
              --- Transition weights ---
                     H1     H2     H3     H4     H5     H6
              H1 0.0000 0.5000 0.5000     NA     NA     NA
              H2 0.0000 0.0000 1.0000     NA     NA     NA
              H3 0.0001 0.9999 0.0000     NA     NA     NA
              H4     NA     NA     NA     NA     NA     NA
              H5     NA     NA     NA     NA     NA     NA
              H6     NA     NA     NA     NA     NA     NA
      
                Step 4: Updated graph after removing hypotheses H4, H6, H5, H1
      
                --- Hypothesis weights ---
                H1:  NA
                H2: 0.5
                H3: 0.5
                H4:  NA
                H5:  NA
                H6:  NA
      
                --- Transition weights ---
                   H1 H2 H3 H4 H5 H6
                H1 NA NA NA NA NA NA
                H2 NA  0  1 NA NA NA
                H3 NA  1  0 NA NA NA
                H4 NA NA NA NA NA NA
                H5 NA NA NA NA NA NA
                H6 NA NA NA NA NA NA
      
        Final updated graph after removing rejected hypotheses
      
        --- Hypothesis weights ---
        H1:  NA
        H2: 0.5
        H3: 0.5
        H4:  NA
        H5:  NA
        H6:  NA
      
        --- Transition weights ---
           H1 H2 H3 H4 H5 H6
        H1 NA NA NA NA NA NA
        H2 NA  0  1 NA NA NA
        H3 NA  1  0 NA NA NA
        H4 NA NA NA NA NA NA
        H5 NA NA NA NA NA NA
        H6 NA NA NA NA NA NA
      
      Detailed test values ($test_values) --------------------------------------------
        Step Hypothesis     p <= Weight * Alpha Inequality_holds
           1         H4 0.010 <=   0.50 * 0.025             TRUE
           2         H6 0.000 <=   0.25 * 0.025             TRUE
           3         H5 0.005 <=   0.50 * 0.025             TRUE
           4         H1 0.025 <=   1.00 * 0.025             TRUE
           5         H3 0.015 <=   0.50 * 0.025            FALSE
           5         H2 0.020 <=   0.50 * 0.025            FALSE
      
