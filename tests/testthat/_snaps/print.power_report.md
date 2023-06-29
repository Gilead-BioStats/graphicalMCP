# printing Bonferroni power - sequential & closure

    Code
      calculate_power(g, sim_seed = 51223)
    Output
      
      Test parameters ----------------------------------------------------------------
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
      
        Alpha = 0.05
      
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
                                     H1   H2   H3   H4
             Power to reject each: 0.04 0.00 0.00 0.00
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
        p_sim_H1  p_sim_H2  p_sim_H3  p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
       0.2716446 0.5737722 0.7555814 0.0257523  FALSE  FALSE  FALSE  FALSE
       0.2482202 0.0851463 0.6078186 0.7475477  FALSE  FALSE  FALSE  FALSE
       0.6924547 0.0172719 0.8867099 0.9245648  FALSE  FALSE  FALSE  FALSE
       0.7315287 0.5084556 0.4831309 0.6598959  FALSE  FALSE  FALSE  FALSE
       0.6631030 0.1673771 0.3500209 0.6545621  FALSE  FALSE  FALSE  FALSE
       0.4539974 0.1095576 0.1310492 0.2535933  FALSE  FALSE  FALSE  FALSE
        ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223), indent = 6, precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
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
      
            Alpha = 0.05
      
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
                                         H1   H2   H3   H4
                 Power to reject each: 0.04 0.00 0.00 0.00
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
              0.2716   0.5738   0.7556   0.0258  FALSE  FALSE  FALSE  FALSE
              0.2482   0.0851   0.6078   0.7475  FALSE  FALSE  FALSE  FALSE
              0.6925   0.0173   0.8867   0.9246  FALSE  FALSE  FALSE  FALSE
              0.7315   0.5085   0.4831   0.6599  FALSE  FALSE  FALSE  FALSE
              0.6631   0.1674   0.3500   0.6546  FALSE  FALSE  FALSE  FALSE
              0.4540   0.1096   0.1310   0.2536  FALSE  FALSE  FALSE  FALSE
            ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223, force_closure = TRUE), indent = 6,
      precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
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
      
            Alpha = 0.05
      
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
                                         H1   H2   H3   H4
                 Power to reject each: 0.04 0.00 0.00 0.00
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
              0.2716   0.5738   0.7556   0.0258  FALSE  FALSE  FALSE  FALSE
              0.2482   0.0851   0.6078   0.7475  FALSE  FALSE  FALSE  FALSE
              0.6925   0.0173   0.8867   0.9246  FALSE  FALSE  FALSE  FALSE
              0.7315   0.5085   0.4831   0.6599  FALSE  FALSE  FALSE  FALSE
              0.6631   0.1674   0.3500   0.6546  FALSE  FALSE  FALSE  FALSE
              0.4540   0.1096   0.1310   0.2536  FALSE  FALSE  FALSE  FALSE
            ...
      

# printing Simes power

    Code
      calculate_power(g, sim_seed = 51223, test_types = "s")
    Output
      
      Test parameters ----------------------------------------------------------------
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
      
        Alpha = 0.05
      
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
                                     H1   H2   H3   H4
             Power to reject each: 0.02 0.02 0.00 0.00
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
        p_sim_H1  p_sim_H2  p_sim_H3  p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
       0.2716446 0.5737722 0.7555814 0.0257523  FALSE  FALSE  FALSE  FALSE
       0.2482202 0.0851463 0.6078186 0.7475477  FALSE  FALSE  FALSE  FALSE
       0.6924547 0.0172719 0.8867099 0.9245648  FALSE   TRUE  FALSE  FALSE
       0.7315287 0.5084556 0.4831309 0.6598959  FALSE  FALSE  FALSE  FALSE
       0.6631030 0.1673771 0.3500209 0.6545621  FALSE  FALSE  FALSE  FALSE
       0.4539974 0.1095576 0.1310492 0.2535933  FALSE  FALSE  FALSE  FALSE
        ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223, test_types = "s"), indent = 6,
      precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
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
      
            Alpha = 0.05
      
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
                                         H1   H2   H3   H4
                 Power to reject each: 0.02 0.02 0.00 0.00
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
              0.2716   0.5738   0.7556   0.0258  FALSE  FALSE  FALSE  FALSE
              0.2482   0.0851   0.6078   0.7475  FALSE  FALSE  FALSE  FALSE
              0.6925   0.0173   0.8867   0.9246  FALSE   TRUE  FALSE  FALSE
              0.7315   0.5085   0.4831   0.6599  FALSE  FALSE  FALSE  FALSE
              0.6631   0.1674   0.3500   0.6546  FALSE  FALSE  FALSE  FALSE
              0.4540   0.1096   0.1310   0.2536  FALSE  FALSE  FALSE  FALSE
            ...
      

# printing parametric power

    Code
      calculate_power(g, sim_seed = 51223, test_types = "p", test_corr = diag(4))
    Output
      
      Test parameters ----------------------------------------------------------------
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
      
        Alpha = 0.05
      
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
                                     H1   H2   H3   H4
             Power to reject each: 0.04 0.00 0.00 0.00
      
              Expected rejections: 0.04
        Power to reject 1 or more: 0.04
              Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
        p_sim_H1  p_sim_H2  p_sim_H3  p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
       0.2716446 0.5737722 0.7555814 0.0257523  FALSE  FALSE  FALSE  FALSE
       0.2482202 0.0851463 0.6078186 0.7475477  FALSE  FALSE  FALSE  FALSE
       0.6924547 0.0172719 0.8867099 0.9245648  FALSE  FALSE  FALSE  FALSE
       0.7315287 0.5084556 0.4831309 0.6598959  FALSE  FALSE  FALSE  FALSE
       0.6631030 0.1673771 0.3500209 0.6545621  FALSE  FALSE  FALSE  FALSE
       0.4539974 0.1095576 0.1310492 0.2535933  FALSE  FALSE  FALSE  FALSE
        ...
      

---

    Code
      print(calculate_power(g, sim_seed = 51223, test_types = "p", test_corr = diag(4)),
      indent = 6, precision = 3)
    Output
      
      Test parameters ----------------------------------------------------------------
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
      
            Alpha = 0.05
      
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
                                         H1   H2   H3   H4
                 Power to reject each: 0.04 0.00 0.00 0.00
      
                  Expected rejections: 0.04
            Power to reject 1 or more: 0.04
                  Power to reject all: 0
      
      Simulation details -------------------------------------------------------------
            p_sim_H1 p_sim_H2 p_sim_H3 p_sim_H4 rej_H1 rej_H2 rej_H3 rej_H4
              0.2716   0.5738   0.7556   0.0258  FALSE  FALSE  FALSE  FALSE
              0.2482   0.0851   0.6078   0.7475  FALSE  FALSE  FALSE  FALSE
              0.6925   0.0173   0.8867   0.9246  FALSE  FALSE  FALSE  FALSE
              0.7315   0.5085   0.4831   0.6599  FALSE  FALSE  FALSE  FALSE
              0.6631   0.1674   0.3500   0.6546  FALSE  FALSE  FALSE  FALSE
              0.4540   0.1096   0.1310   0.2536  FALSE  FALSE  FALSE  FALSE
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
           H1  H2  H3  H4  H5  H6
       H1 0.0 0.2 0.2 0.2 0.2 0.2
       H2 0.2 0.0 0.2 0.2 0.2 0.2
       H3 0.2 0.2 0.0 0.2 0.2 0.2
       H4 0.2 0.2 0.2 0.0 0.2 0.2
       H5 0.2 0.2 0.2 0.2 0.0 0.2
       H6 0.2 0.2 0.2 0.2 0.2 0.0
      
      Alpha = 0.0254871
      
       Parametric testing correlation:              H1           H2           H3
                                       H1 1.0000000000 0.7853981634 0.7853981634
                                       H2 0.7853981634 1.0000000000 0.7853981634
                                       H3 0.7853981634 0.7853981634 1.0000000000
                                       H4 0.7853981634 0.7853981634 0.7853981634
                                       H5 0.7853981634 0.7853981634 0.7853981634
                                       H6 0.7853981634 0.7853981634 0.7853981634
                 H4           H5           H6
       0.7853981634 0.7853981634 0.7853981634
       0.7853981634 0.7853981634 0.7853981634
       0.7853981634 0.7853981634 0.7853981634
       1.0000000000 0.7853981634 0.7853981634
       0.7853981634 1.0000000000 0.7853981634
       0.7853981634 0.7853981634 1.0000000000
      
      Test types
      bonferroni: (H4-H3)
           simes: (H6-H1)
      parametric: (H2-H5)
      
      Simulation parameters ----------------------------------------------------------
      Testing 1,328 simulations - random seed 51223 & multivariate normal params:
      
                                H1           H2           H3           H4
      Marginal power: 10.471975512  3.926990817  2.416609734  1.745329252
                                H5           H6
      Marginal power:  1.365909849  1.121997376
      
       Correlation:              H1           H2           H3           H4
                    H1 1.0000000000 0.6283185307 0.6283185307 0.6283185307
                    H2 0.6283185307 1.0000000000 0.6283185307 0.6283185307
                    H3 0.6283185307 0.6283185307 1.0000000000 0.6283185307
                    H4 0.6283185307 0.6283185307 0.6283185307 1.0000000000
                    H5 0.6283185307 0.6283185307 0.6283185307 0.6283185307
                    H6 0.6283185307 0.6283185307 0.6283185307 0.6283185307
                 H5           H6
       0.6283185307 0.6283185307
       0.6283185307 0.6283185307
       0.6283185307 0.6283185307
       0.6283185307 0.6283185307
       1.0000000000 0.6283185307
       0.6283185307 1.0000000000
      
      Power calculation --------------------------------------------------------------
                                           H1           H2           H3           H4
           Power to reject each: 1.0000000000 0.9224397590 0.4856927711 0.2823795181
                                           H5           H6
           Power to reject each: 0.1822289157 0.1362951807
      
            Expected rejections: 3.009036145
      Power to reject 1 or more: 1
            Power to reject all: 0.09638554217
      
                                   .[1] || .[5] || .[6] .[2] && (.[5] || .[6])
          Probability of success:          1.0000000000           0.2115963855
      
      Simulation details -------------------------------------------------------------
              p_sim_H1        p_sim_H2        p_sim_H3        p_sim_H4
       4.563762322e-32 1.304967090e-06 1.986012764e-03 6.720642049e-05
       3.943645821e-23 1.139610848e-03 5.042006780e-02 5.163650525e-03
       8.855848936e-24 7.129726936e-05 9.851598164e-03 8.324088549e-02
       1.212914948e-30 4.822190438e-06 6.243573730e-04 6.082713066e-04
       5.992374781e-39 1.983489601e-12 8.573019478e-05 1.242712361e-08
       1.366248847e-22 1.605457751e-04 2.332545819e-02 1.180986407e-01
              p_sim_H5        p_sim_H6 rej_H1 rej_H2 rej_H3 rej_H4 rej_H5 rej_H6
       3.834754655e-03 2.229351519e-03   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
       4.366626910e-01 5.882400991e-01   TRUE   TRUE  FALSE   TRUE  FALSE  FALSE
       1.589378659e-01 5.484174340e-02   TRUE   TRUE  FALSE  FALSE  FALSE  FALSE
       2.634972137e-03 1.170174107e-02   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
       5.237022700e-08 8.079883817e-03   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
       1.956347397e-01 5.536435621e-01   TRUE   TRUE  FALSE  FALSE  FALSE  FALSE
      ...
      

