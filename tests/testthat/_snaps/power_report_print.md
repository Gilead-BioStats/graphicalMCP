# printing Bonferroni power - sequential & closure

    Code
      calculate_power(g, sim_seed = 51223)
    Output
      
      Test parameters ----------------------------------------------------------------
        Initial graph
        
        --- Hypothesis weights ---
        H1: 1.0000
        H2: 0.0000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 0.5000 0.5000 0.0000
        H2 0.0000 0.0000 0.0000 1.0000
        H3 0.0000 0.5000 0.0000 0.5000
        H4 0.0000 1.0000 0.0000 0.0000
      
        Global alpha = 0.05
      
        Test types
        bonferroni: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                          H1 H2 H3 H4
        Simulation means:  0  0  0  0
      
        Simulation covariance:    H1 H2 H3 H4
                               H1  1  0  0  0
                               H2  0  1  0  0
                               H3  0  0  1  0
                               H4  0  0  0  1
      
        Success is defined as rejecting any of [H1, H2]
      
      Power calculation --------------------------------------------------------------
                                     H1 H2 H3 H4
             Power to reject each: 0.04  0  0  0
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
           Probability of success: 0.04
      
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
            H1: 1.0000
            H2: 0.0000
            H3: 0.0000
            H4: 0.0000
            
            --- Transition weights ---
                   H1     H2     H3     H4
            H1 0.0000 0.5000 0.5000 0.0000
            H2 0.0000 0.0000 0.0000 1.0000
            H3 0.0000 0.5000 0.0000 0.5000
            H4 0.0000 1.0000 0.0000 0.0000
      
            Global alpha = 0.05
      
            Test types
            bonferroni: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                              H1 H2 H3 H4
            Simulation means:  0  0  0  0
      
            Simulation covariance:    H1 H2 H3 H4
                                   H1  1  0  0  0
                                   H2  0  1  0  0
                                   H3  0  0  1  0
                                   H4  0  0  0  1
      
            Success is defined as rejecting any of [H1, H2]
      
      Power calculation --------------------------------------------------------------
                                         H1 H2 H3 H4
                 Power to reject each: 0.04  0  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
               Probability of success: 0.04
      
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
            H1: 1.0000
            H2: 0.0000
            H3: 0.0000
            H4: 0.0000
            
            --- Transition weights ---
                   H1     H2     H3     H4
            H1 0.0000 0.5000 0.5000 0.0000
            H2 0.0000 0.0000 0.0000 1.0000
            H3 0.0000 0.5000 0.0000 0.5000
            H4 0.0000 1.0000 0.0000 0.0000
      
            Global alpha = 0.05
      
            Test types
            bonferroni: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                              H1 H2 H3 H4
            Simulation means:  0  0  0  0
      
            Simulation covariance:    H1 H2 H3 H4
                                   H1  1  0  0  0
                                   H2  0  1  0  0
                                   H3  0  0  1  0
                                   H4  0  0  0  1
      
            Success is defined as rejecting any of [H1, H2]
      
      Power calculation --------------------------------------------------------------
                                         H1 H2 H3 H4
                 Power to reject each: 0.04  0  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
               Probability of success: 0.04
      
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
      
        Test types
        simes: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
        Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                          H1 H2 H3 H4
        Simulation means:  0  0  0  0
      
        Simulation covariance:    H1 H2 H3 H4
                               H1  1  0  0  0
                               H2  0  1  0  0
                               H3  0  0  1  0
                               H4  0  0  0  1
      
        Success is defined as rejecting any of [H1, H2]
      
      Power calculation --------------------------------------------------------------
                                     H1   H2 H3 H4
             Power to reject each: 0.02 0.02  0  0
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
           Probability of success: 0.04
      
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
      
            Test types
            simes: (H1-H2-H3-H4)
      
      Simulation parameters ----------------------------------------------------------
            Testing 100 simulations - random seed 51223 & multivariate normal params:
      
                              H1 H2 H3 H4
            Simulation means:  0  0  0  0
      
            Simulation covariance:    H1 H2 H3 H4
                                   H1  1  0  0  0
                                   H2  0  1  0  0
                                   H3  0  0  1  0
                                   H4  0  0  0  1
      
            Success is defined as rejecting any of [H1, H2]
      
      Power calculation --------------------------------------------------------------
                                         H1   H2 H3 H4
                 Power to reject each: 0.02 0.02  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
               Probability of success: 0.04
      
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
        H1: 1.0000
        H2: 0.0000
        H3: 0.0000
        H4: 0.0000
        
        --- Transition weights ---
               H1     H2     H3     H4
        H1 0.0000 1.0000 0.0000 0.0000
        H2 0.0000 0.0000 1.0000 0.0000
        H3 0.0000 0.0000 0.0000 1.0000
        H4 0.0000 0.0000 0.0000 0.0000
      
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
        Simulation means:  0  0  0  0
      
        Simulation covariance:    H1 H2 H3 H4
                               H1  1  0  0  0
                               H2  0  1  0  0
                               H3  0  0  1  0
                               H4  0  0  0  1
      
        Success is defined as rejecting any of [H1, H2]
      
      Power calculation --------------------------------------------------------------
                                     H1 H2 H3 H4
             Power to reject each: 0.04  0  0  0
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
           Probability of success: 0.04
      
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
            H1: 1.0000
            H2: 0.0000
            H3: 0.0000
            H4: 0.0000
            
            --- Transition weights ---
                   H1     H2     H3     H4
            H1 0.0000 1.0000 0.0000 0.0000
            H2 0.0000 0.0000 1.0000 0.0000
            H3 0.0000 0.0000 0.0000 1.0000
            H4 0.0000 0.0000 0.0000 0.0000
      
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
            Simulation means:  0  0  0  0
      
            Simulation covariance:    H1 H2 H3 H4
                                   H1  1  0  0  0
                                   H2  0  1  0  0
                                   H3  0  0  1  0
                                   H4  0  0  0  1
      
            Success is defined as rejecting any of [H1, H2]
      
      Power calculation --------------------------------------------------------------
                                         H1 H2 H3 H4
                 Power to reject each: 0.04  0  0  0
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
               Probability of success: 0.04
      
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
        "p"), t_corr, 1328, pi / seq(0.3, 2.8, by = 0.5), s_corr, c(1, 5, 6), 51223),
      indent = 0, precision = 10)
    Warning <simpleWarning>
      sigma is numerically not positive semidefinite
    Output
      
      Test parameters ----------------------------------------------------------------
      Initial graph
      
      --- Hypothesis weights ---
      H1: 0.1667
      H2: 0.1667
      H3: 0.1667
      H4: 0.1667
      H5: 0.1667
      H6: 0.1667
      
      --- Transition weights ---
             H1     H2     H3     H4     H5     H6
      H1 0.0000 0.2000 0.2000 0.2000 0.2000 0.2000
      H2 0.2000 0.0000 0.2000 0.2000 0.2000 0.2000
      H3 0.2000 0.2000 0.0000 0.2000 0.2000 0.2000
      H4 0.2000 0.2000 0.2000 0.0000 0.2000 0.2000
      H5 0.2000 0.2000 0.2000 0.2000 0.0000 0.2000
      H6 0.2000 0.2000 0.2000 0.2000 0.2000 0.0000
      
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
      Simulation means: 10.47198 3.926991 2.41661 1.745329 1.36591 1.121997
      
       Simulation covariance:          H1       H2       H3       H4       H5
                              H1 1.000000 1.047198 1.047198 1.047198 1.047198
                              H2 1.047198 1.000000 1.047198 1.047198 1.047198
                              H3 1.047198 1.047198 1.000000 1.047198 1.047198
                              H4 1.047198 1.047198 1.047198 1.000000 1.047198
                              H5 1.047198 1.047198 1.047198 1.047198 1.000000
                              H6 1.047198 1.047198 1.047198 1.047198 1.047198
             H6
       1.047198
       1.047198
       1.047198
       1.047198
       1.047198
       1.000000
      
      Success is defined as rejecting any of [H1, H5, H6]
      
      Power calculation --------------------------------------------------------------
                                 H1        H2        H3        H4        H5        H6
           Power to reject each:  1 0.9164157 0.4781627 0.2756024 0.2048193 0.2048193
      
            Expected rejections: 3.079819
      Power to reject 1 or more: 1
            Power to reject all: 0.2048193
         Probability of success: 1
      
      Simulation details -------------------------------------------------------------
       p_sim_H1     p_sim_H2     p_sim_H3     p_sim_H4     p_sim_H5     p_sim_H6
              0 0.0000000214 0.0000361885 0.0004885258 0.0017634943 0.0037507434
              0 0.0009517561 0.0554112930 0.1779446580 0.2932872009 0.3821288658
              0 0.0000944166 0.0131037073 0.0603497904 0.1205133360 0.1765710188
              0 0.0000000875 0.0001020906 0.0011730862 0.0038713757 0.0077778986
              0 0.0000000000 0.0000000068 0.0000002770 0.0000018532 0.0000058451
              0 0.0008426908 0.0515158779 0.1687668824 0.2810820164 0.3685418740
       rej_H1 rej_H2 rej_H3 rej_H4 rej_H5 rej_H6
         TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
         TRUE   TRUE  FALSE  FALSE  FALSE  FALSE
         TRUE   TRUE  FALSE  FALSE  FALSE  FALSE
         TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
         TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
         TRUE   TRUE  FALSE  FALSE  FALSE  FALSE
      ...
      

