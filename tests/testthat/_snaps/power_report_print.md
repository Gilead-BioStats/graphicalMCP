# printing Bonferroni power - sequential & closure

    Code
      calculate_power(g, sim_seed = 51223)
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 1.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 0.500000 0.500000 0.000000
        H2 0.000000 0.000000 0.000000 1.000000
        H3 0.000000 0.500000 0.000000 0.500000
        H4 0.000000 1.000000 0.000000 0.000000
      
        Global alpha = 0.05
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                        H1 H2 H3 H4
        Marginal power:  0  0  0  0
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation --------------------------------------------------------------
                                     H1 H2 H3 H4
             Power to reject each: 0.04  0  0  0
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
        p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
        0.271645 0.573772 0.755581 0.025752  FALSE  FALSE  FALSE  FALSE
        0.248220 0.085146 0.607819 0.747548  FALSE  FALSE  FALSE  FALSE
        0.692455 0.017272 0.886710 0.924565  FALSE  FALSE  FALSE  FALSE
        0.731529 0.508456 0.483131 0.659896  FALSE  FALSE  FALSE  FALSE
        0.663103 0.167377 0.350021 0.654562  FALSE  FALSE  FALSE  FALSE
        0.453997 0.109558 0.131049 0.253593  FALSE  FALSE  FALSE  FALSE
        ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223), indent = 6, precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1.000
            H2: 0.000
            H3: 0.000
            H4: 0.000
      
            --- Transition weights ---
                  H1    H2    H3    H4
            H1 0.000 0.500 0.500 0.000
            H2 0.000 0.000 0.000 1.000
            H3 0.000 0.500 0.000 0.500
            H4 0.000 1.000 0.000 0.000
      
            Global alpha = 0.05
      
            Test types
            bonferroni: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                            H1 H2 H3 H4
            Marginal power:  0  0  0  0
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation --------------------------------------------------------------
                                         H1 H2 H3 H4
                 Power to reject each: 0.04  0  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
               0.272    0.574    0.756    0.026  FALSE  FALSE  FALSE  FALSE
               0.248    0.085    0.608    0.748  FALSE  FALSE  FALSE  FALSE
               0.692    0.017    0.887    0.925  FALSE  FALSE  FALSE  FALSE
               0.732    0.508    0.483    0.660  FALSE  FALSE  FALSE  FALSE
               0.663    0.167    0.350    0.655  FALSE  FALSE  FALSE  FALSE
               0.454    0.110    0.131    0.254  FALSE  FALSE  FALSE  FALSE
            ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223, force_closure = TRUE), indent = 6,
      precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1.000
            H2: 0.000
            H3: 0.000
            H4: 0.000
      
            --- Transition weights ---
                  H1    H2    H3    H4
            H1 0.000 0.500 0.500 0.000
            H2 0.000 0.000 0.000 1.000
            H3 0.000 0.500 0.000 0.500
            H4 0.000 1.000 0.000 0.000
      
            Global alpha = 0.05
      
            Test types
            bonferroni: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                            H1 H2 H3 H4
            Marginal power:  0  0  0  0
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation --------------------------------------------------------------
                                         H1 H2 H3 H4
                 Power to reject each: 0.04  0  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
               0.272    0.574    0.756    0.026  FALSE  FALSE  FALSE  FALSE
               0.248    0.085    0.608    0.748  FALSE  FALSE  FALSE  FALSE
               0.692    0.017    0.887    0.925  FALSE  FALSE  FALSE  FALSE
               0.732    0.508    0.483    0.660  FALSE  FALSE  FALSE  FALSE
               0.663    0.167    0.350    0.655  FALSE  FALSE  FALSE  FALSE
               0.454    0.110    0.131    0.254  FALSE  FALSE  FALSE  FALSE
            ...
      

# printing Simes power

    Code
      calculate_power(g, sim_seed = 51223, test_types = "s")
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
      
        Global alpha = 0.05
      
        Test types
        simes: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                        H1 H2 H3 H4
        Marginal power:  0  0  0  0
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation --------------------------------------------------------------
                                     H1   H2 H3 H4
             Power to reject each: 0.02 0.02  0  0
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
        p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
        0.271645 0.573772 0.755581 0.025752  FALSE  FALSE  FALSE  FALSE
        0.248220 0.085146 0.607819 0.747548  FALSE  FALSE  FALSE  FALSE
        0.692455 0.017272 0.886710 0.924565  FALSE   TRUE  FALSE  FALSE
        0.731529 0.508456 0.483131 0.659896  FALSE  FALSE  FALSE  FALSE
        0.663103 0.167377 0.350021 0.654562  FALSE  FALSE  FALSE  FALSE
        0.453997 0.109558 0.131049 0.253593  FALSE  FALSE  FALSE  FALSE
        ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223, test_types = "s"), indent = 6,
      precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 0.500
            H2: 0.500
            H3: 0.000
            H4: 0.000
      
            --- Transition weights ---
                  H1    H2    H3    H4
            H1 0.000 0.000 1.000 0.000
            H2 0.000 0.000 0.000 1.000
            H3 0.000 1.000 0.000 0.000
            H4 1.000 0.000 0.000 0.000
      
            Global alpha = 0.05
      
            Test types
            simes: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                            H1 H2 H3 H4
            Marginal power:  0  0  0  0
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation --------------------------------------------------------------
                                         H1   H2 H3 H4
                 Power to reject each: 0.02 0.02  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
               0.272    0.574    0.756    0.026  FALSE  FALSE  FALSE  FALSE
               0.248    0.085    0.608    0.748  FALSE  FALSE  FALSE  FALSE
               0.692    0.017    0.887    0.925  FALSE   TRUE  FALSE  FALSE
               0.732    0.508    0.483    0.660  FALSE  FALSE  FALSE  FALSE
               0.663    0.167    0.350    0.655  FALSE  FALSE  FALSE  FALSE
               0.454    0.110    0.131    0.254  FALSE  FALSE  FALSE  FALSE
            ...
      

# printing parametric power

    Code
      calculate_power(g, sim_seed = 51223, test_types = "p", test_corr = diag(4))
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 1.000000
        H2: 0.000000
        H3: 0.000000
        H4: 0.000000
      
        --- Transition weights ---
                 H1       H2       H3       H4
        H1 0.000000 1.000000 0.000000 0.000000
        H2 0.000000 0.000000 1.000000 0.000000
        H3 0.000000 0.000000 0.000000 1.000000
        H4 0.000000 0.000000 0.000000 0.000000
      
        Global alpha = 0.05
      
        Parametric testing correlation:    H1 H2 H3 H4
                                        H1  1  0  0  0
                                        H2  0  1  0  0
                                        H3  0  0  1  0
                                        H4  0  0  0  1
      
        Test types
        parametric: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                        H1 H2 H3 H4
        Marginal power:  0  0  0  0
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation --------------------------------------------------------------
                                     H1 H2 H3 H4
             Power to reject each: 0.04  0  0  0
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
        p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
        0.271645 0.573772 0.755581 0.025752  FALSE  FALSE  FALSE  FALSE
        0.248220 0.085146 0.607819 0.747548  FALSE  FALSE  FALSE  FALSE
        0.692455 0.017272 0.886710 0.924565  FALSE  FALSE  FALSE  FALSE
        0.731529 0.508456 0.483131 0.659896  FALSE  FALSE  FALSE  FALSE
        0.663103 0.167377 0.350021 0.654562  FALSE  FALSE  FALSE  FALSE
        0.453997 0.109558 0.131049 0.253593  FALSE  FALSE  FALSE  FALSE
        ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223, test_types = "p", test_corr = diag(4)),
      indent = 6, precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1.000
            H2: 0.000
            H3: 0.000
            H4: 0.000
      
            --- Transition weights ---
                  H1    H2    H3    H4
            H1 0.000 1.000 0.000 0.000
            H2 0.000 0.000 1.000 0.000
            H3 0.000 0.000 0.000 1.000
            H4 0.000 0.000 0.000 0.000
      
            Global alpha = 0.05
      
            Parametric testing correlation:    H1 H2 H3 H4
                                            H1  1  0  0  0
                                            H2  0  1  0  0
                                            H3  0  0  1  0
                                            H4  0  0  0  1
      
            Test types
            parametric: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                            H1 H2 H3 H4
            Marginal power:  0  0  0  0
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation --------------------------------------------------------------
                                         H1 H2 H3 H4
                 Power to reject each: 0.04  0  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
               0.272    0.574    0.756    0.026  FALSE  FALSE  FALSE  FALSE
               0.248    0.085    0.608    0.748  FALSE  FALSE  FALSE  FALSE
               0.692    0.017    0.887    0.925  FALSE  FALSE  FALSE  FALSE
               0.732    0.508    0.483    0.660  FALSE  FALSE  FALSE  FALSE
               0.663    0.167    0.350    0.655  FALSE  FALSE  FALSE  FALSE
               0.454    0.110    0.131    0.254  FALSE  FALSE  FALSE  FALSE
            ...
      

# printing blended power

    Code
      print(calculate_power(g, 0.0254871, list(4:3, c(6, 1), c(2, 5)), c("b", "s",
        "p"), t_corr, 1328, pi / seq(0.3, 2.8, by = 0.5), s_corr, list(function(.) .[
        1] || .[5] || .[6], function(.) .[2] && (.[5] || .[6])), 51223), indent = 0,
      precision = 10)
    Output
      
      Test parameters ----------------------------------------------------------------
      Initial graph
      
      --- Hypothesis weights ---
      H1: 0.1666666667
      H2: 0.1666666667
      H3: 0.1666666667
      H4: 0.1666666667
      H5: 0.1666666667
      H6: 0.1666666667
      
      --- Transition weights ---
                    H1           H2           H3           H4           H5
       H1 0.0000000000 0.2000000000 0.2000000000 0.2000000000 0.2000000000
       H2 0.2000000000 0.0000000000 0.2000000000 0.2000000000 0.2000000000
       H3 0.2000000000 0.2000000000 0.0000000000 0.2000000000 0.2000000000
       H4 0.2000000000 0.2000000000 0.2000000000 0.0000000000 0.2000000000
       H5 0.2000000000 0.2000000000 0.2000000000 0.2000000000 0.0000000000
       H6 0.2000000000 0.2000000000 0.2000000000 0.2000000000 0.2000000000
                 H6
       0.2000000000
       0.2000000000
       0.2000000000
       0.2000000000
       0.2000000000
       0.0000000000
      
      Global alpha = 0.0254871
      
       Parametric testing correlation:           H1        H2        H3        H4
                                       H1 1.0000000 0.7853982 0.7853982 0.7853982
                                       H2 0.7853982 1.0000000 0.7853982 0.7853982
                                       H3 0.7853982 0.7853982 1.0000000 0.7853982
                                       H4 0.7853982 0.7853982 0.7853982 1.0000000
                                       H5 0.7853982 0.7853982 0.7853982 0.7853982
                                       H6 0.7853982 0.7853982 0.7853982 0.7853982
              H5        H6
       0.7853982 0.7853982
       0.7853982 0.7853982
       0.7853982 0.7853982
       0.7853982 0.7853982
       1.0000000 0.7853982
       0.7853982 1.0000000
      
      Test types
      bonferroni: (H4-H3)
           simes: (H6-H1)
      parametric: (H2-H5)
      
      Simulation parameters ----------------------------------------------------------
      Testing 1,328 simulations - random seed 51223 & multivariate normal params:
      
                            H1       H2      H3       H4      H5       H6
      Marginal power: 10.47198 3.926991 2.41661 1.745329 1.36591 1.121997
      
       Correlation:           H1        H2        H3        H4        H5        H6
                    H1 1.0000000 0.6283185 0.6283185 0.6283185 0.6283185 0.6283185
                    H2 0.6283185 1.0000000 0.6283185 0.6283185 0.6283185 0.6283185
                    H3 0.6283185 0.6283185 1.0000000 0.6283185 0.6283185 0.6283185
                    H4 0.6283185 0.6283185 0.6283185 1.0000000 0.6283185 0.6283185
                    H5 0.6283185 0.6283185 0.6283185 0.6283185 1.0000000 0.6283185
                    H6 0.6283185 0.6283185 0.6283185 0.6283185 0.6283185 1.0000000
      
      Power calculation --------------------------------------------------------------
                                 H1        H2        H3        H4        H5        H6
           Power to reject each:  1 0.9224398 0.4856928 0.2823795 0.1822289 0.1362952
      
            Expected rejections: 3.009036
      Power to reject 1 or more: 1
            Power to reject all: 0.09638554
      
                                   .[1] || .[5] || .[6] .[2] && (.[5] || .[6])
          Probability of success:                     1              0.2115964
      
      Simulation details -------------------------------------------------------------
       p_sim_H1     p_sim_H2     p_sim_H3     p_sim_H4     p_sim_H5    p_sim_H6
              0 0.0000013050 0.0019860128 0.0000672064 0.0038347547 0.002229351
              0 0.0011396108 0.0504200678 0.0051636505 0.4366626910 0.588240099
              0 0.0000712973 0.0098515982 0.0832408855 0.1589378659 0.054841743
              0 0.0000048222 0.0006243574 0.0006082713 0.0026349721 0.011701741
              0 0.0000000000 0.0000857302 0.0000000124 0.0000000524 0.008079884
              0 0.0001605458 0.0233254582 0.1180986407 0.1956347397 0.553643562
       rej_H1 rej_H2 rej_H3 rej_H4 rej_H5 rej_H6
         TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
         TRUE   TRUE  FALSE   TRUE  FALSE  FALSE
         TRUE   TRUE  FALSE  FALSE  FALSE  FALSE
         TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
         TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
         TRUE   TRUE  FALSE  FALSE  FALSE  FALSE
      ...
      

