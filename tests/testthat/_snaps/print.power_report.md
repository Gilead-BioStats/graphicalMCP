# printing Bonferroni power - sequential

    Code
      graph_calculate_power(g, sim_n = 5, verbose = TRUE)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 1
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
            H1  H2  H3  H4
        H1 0.0 0.5 0.5 0.0
        H2 0.0 0.0 0.0 1.0
        H3 0.0 0.5 0.0 0.5
        H4 0.0 1.0 0.0 0.0
      
        Alpha = 0.025
      
        Test types
        bonferroni: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
        Testing 5 simulations with multivariate normal params:
      
                           H1    H2    H3    H4
        Marginal power: 0.025 0.025 0.025 0.025
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                    H1 H2 H3 H4
                       Local power:  0  0  0  0
      
        Expected no. of rejections: 0
         Power to reject 1 or more: 0
               Power to reject all: 0
      
      Simulation details ($details) --------------------------------------------------
          p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
           0.27164  0.57377  0.75558  0.02575  FALSE  FALSE  FALSE  FALSE
           0.24822  0.08515  0.60782  0.74755  FALSE  FALSE  FALSE  FALSE
           0.69245  0.01727  0.88671  0.92456  FALSE  FALSE  FALSE  FALSE
           0.73153  0.50846  0.48313  0.65990  FALSE  FALSE  FALSE  FALSE
           0.66310  0.16738  0.35002  0.65456  FALSE  FALSE  FALSE  FALSE
      

---

    Code
      print(graph_calculate_power(g), indent = 6, precision = 3)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1
            H2: 0
            H3: 0
            H4: 0
      
            --- Transition weights ---
                H1  H2  H3  H4
            H1 0.0 0.5 0.5 0.0
            H2 0.0 0.0 0.0 1.0
            H3 0.0 0.5 0.0 0.5
            H4 0.0 1.0 0.0 0.0
      
            Alpha = 0.025
      
            Test types
            bonferroni: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
            Testing 100 simulations with multivariate normal params:
      
                               H1    H2    H3    H4
            Marginal power: 0.025 0.025 0.025 0.025
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                          H1   H2   H3   H4
                           Local power: 0.02 0.00 0.00 0.00
      
            Expected no. of rejections: 0.02
             Power to reject 1 or more: 0.02
                   Power to reject all: 0
      

# printing Simes power

    Code
      graph_calculate_power(g, test_types = "s")
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
      
        Test types
        simes: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
        Testing 100 simulations with multivariate normal params:
      
                           H1    H2    H3    H4
        Marginal power: 0.025 0.025 0.025 0.025
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                      H1   H2   H3   H4
                       Local power: 0.01 0.01 0.00 0.00
      
        Expected no. of rejections: 0.02
         Power to reject 1 or more: 0.02
               Power to reject all: 0
      

---

    Code
      print(graph_calculate_power(g, test_types = "s"), indent = 6, precision = 3)
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
      
            Test types
            simes: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
            Testing 100 simulations with multivariate normal params:
      
                               H1    H2    H3    H4
            Marginal power: 0.025 0.025 0.025 0.025
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                          H1   H2   H3   H4
                           Local power: 0.01 0.01 0.00 0.00
      
            Expected no. of rejections: 0.02
             Power to reject 1 or more: 0.02
                   Power to reject all: 0
      

# printing parametric power

    Code
      graph_calculate_power(g, test_types = "p", test_corr = list(diag(4)))
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
        Initial graph
      
        --- Hypothesis weights ---
        H1: 1
        H2: 0
        H3: 0
        H4: 0
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1  0  1  0  0
        H2  0  0  1  0
        H3  0  0  0  1
        H4  0  0  0  0
      
        Alpha = 0.025
      
        Parametric testing correlation:    H1 H2 H3 H4
                                        H1  1  0  0  0
                                        H2  0  1  0  0
                                        H3  0  0  1  0
                                        H4  0  0  0  1
      
        Test types
        parametric: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
        Testing 100 simulations with multivariate normal params:
      
                           H1    H2    H3    H4
        Marginal power: 0.025 0.025 0.025 0.025
      
        Correlation:    H1 H2 H3 H4
                     H1  1  0  0  0
                     H2  0  1  0  0
                     H3  0  0  1  0
                     H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                      H1   H2   H3   H4
                       Local power: 0.02 0.00 0.00 0.00
      
        Expected no. of rejections: 0.02
         Power to reject 1 or more: 0.02
               Power to reject all: 0
      

---

    Code
      print(graph_calculate_power(g, test_types = "p", test_corr = list(diag(4))),
      indent = 6, precision = 3)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
            Initial graph
      
            --- Hypothesis weights ---
            H1: 1
            H2: 0
            H3: 0
            H4: 0
      
            --- Transition weights ---
               H1 H2 H3 H4
            H1  0  1  0  0
            H2  0  0  1  0
            H3  0  0  0  1
            H4  0  0  0  0
      
            Alpha = 0.025
      
            Parametric testing correlation:    H1 H2 H3 H4
                                            H1  1  0  0  0
                                            H2  0  1  0  0
                                            H3  0  0  1  0
                                            H4  0  0  0  1
      
            Test types
            parametric: (H1, H2, H3, H4)
      
      Simulation parameters ($inputs) ------------------------------------------------
            Testing 100 simulations with multivariate normal params:
      
                               H1    H2    H3    H4
            Marginal power: 0.025 0.025 0.025 0.025
      
            Correlation:    H1 H2 H3 H4
                         H1  1  0  0  0
                         H2  0  1  0  0
                         H3  0  0  1  0
                         H4  0  0  0  1
      
      Power calculation ($power) -----------------------------------------------------
                                          H1   H2   H3   H4
                           Local power: 0.02 0.00 0.00 0.00
      
            Expected no. of rejections: 0.02
             Power to reject 1 or more: 0.02
                   Power to reject all: 0
      

# printing blended power

    Code
      print(graph_calculate_power(graph = g, alpha = 0.0254871, power_marginal = pi /
        seq(0.3, 2.8, by = 0.5) / 11, test_groups = list(4:3, c(6, 1), c(2, 5)),
      test_types = c("b", "s", "p"), test_corr = list(NA, NA, t_corr[c(2, 5), c(2, 5)]),
      sim_n = 1328, sim_corr = s_corr, sim_success = list(function(.) .[1] || .[5] ||
        .[6], function(.) .[2] && (.[5] || .[6])), verbose = TRUE), indent = 0,
      precision = 10)
    Output
      
      Test parameters ($inputs) ------------------------------------------------------
      Initial graph
      
      --- Hypothesis weights ---
      H1: 0.1666666667
      H2: 0.1666666667
      H3: 0.1666666667
      H4: 0.1666666667
      H5: 0.1666666667
      H6: 0.1666666667
      
      --- Transition weights ---
           H1  H2  H3  H4  H5  H6
       H1 0.0 0.2 0.2 0.2 0.2 0.2
       H2 0.2 0.0 0.2 0.2 0.2 0.2
       H3 0.2 0.2 0.0 0.2 0.2 0.2
       H4 0.2 0.2 0.2 0.0 0.2 0.2
       H5 0.2 0.2 0.2 0.2 0.0 0.2
       H6 0.2 0.2 0.2 0.2 0.2 0.0
      
      Alpha = 0.0254871
      
       Parametric testing correlation:              H2           H5
                                       H2 1.0000000000 0.7853981634
                                       H5 0.7853981634 1.0000000000
      
      Test types
      bonferroni: (H4, H3)
           simes: (H6, H1)
      parametric: (H2, H5)
      
      Simulation parameters ($inputs) ------------------------------------------------
      Testing 1,328 simulations with multivariate normal params:
      
                                H1           H2           H3           H4
      Marginal power: 0.9519977738 0.3569991652 0.2196917940 0.1586662956
                                H5           H6
      Marginal power: 0.1241736227 0.1019997615
      
       Correlation:              H1           H2           H3           H4
                    H1 1.0000000000 0.7853981634 0.7853981634 0.7853981634
                    H2 0.7853981634 1.0000000000 0.7853981634 0.7853981634
                    H3 0.7853981634 0.7853981634 1.0000000000 0.7853981634
                    H4 0.7853981634 0.7853981634 0.7853981634 1.0000000000
                    H5 0.7853981634 0.7853981634 0.7853981634 0.7853981634
                    H6 0.7853981634 0.7853981634 0.7853981634 0.7853981634
                 H5           H6
       0.7853981634 0.7853981634
       0.7853981634 0.7853981634
       0.7853981634 0.7853981634
       0.7853981634 0.7853981634
       1.0000000000 0.7853981634
       0.7853981634 1.0000000000
      
      Power calculation ($power) -----------------------------------------------------
                                             H1            H2            H3
                     Local power: 0.84036144578 0.20256024096 0.08960843373
                                             H4            H5            H6
                     Local power: 0.07304216867 0.06325301205 0.05346385542
      
      Expected no. of rejections: 1.322289157
       Power to reject 1 or more: 0.8403614458
             Power to reject all: 0.0406626506
      
              Success measure         Power
         .[1] || .[5] || .[6] 0.84036144578
       .[2] && (.[5] || .[6]) 0.07003012048
      
      Simulation details ($details) --------------------------------------------------
              p_sim_H1        p_sim_H2        p_sim_H3        p_sim_H4
       3.030776739e-07 4.792257190e-03 2.563333001e-02 1.613563087e-03
       1.799993896e-03 2.425644224e-01 3.566996361e-01 8.808114728e-02
       7.012987714e-04 7.503335761e-02 1.441254616e-01 2.660807415e-01
       1.256356378e-06 1.035116345e-02 1.600108848e-02 7.478702995e-03
       6.917640824e-11 1.003429108e-06 1.046010311e-03 1.065818840e-06
       2.362041390e-03 1.307269410e-01 2.604735863e-01 3.769023080e-01
       1.080855471e-05 2.206046776e-02 5.881555020e-03 7.515552106e-02
       1.699215401e-05 2.547859799e-03 5.850381205e-03 6.483741341e-03
       1.056104200e-04 1.082619625e-01 2.076903618e-01 2.763834889e-01
       4.608025515e-04 1.373739092e-01 2.457022772e-01 1.873977665e-01
              p_sim_H5        p_sim_H6 rej_H1 rej_H2 rej_H3 rej_H4 rej_H5 rej_H6
       1.376906715e-02 8.017449511e-03   TRUE   TRUE  FALSE   TRUE  FALSE   TRUE
       6.334791645e-01 7.130173989e-01   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
       3.205256009e-01 1.605471054e-01   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
       1.289967157e-02 2.864565452e-02   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
       1.435089782e-06 5.607954111e-03   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
       4.211837866e-01 6.863684924e-01   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
       5.133590888e-02 3.726628011e-02   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
       1.791560792e-03 1.110089048e-02   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
       3.199693067e-01 6.455150468e-01   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
       1.767959161e-01 1.487829955e-01   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE
      ... (Use `print(x, rows = <nn>)` for more)
      

