# snapshot print method

    Code
      graph_update(g, integer(0))
    Output
      Initial and final graphs -------------------------------------------------------
      
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
      
      Updated graph after deleting no hypotheses
      
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
      
      Deletion sequence ($intermediate_graphs) ---------------------------------------
      
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
      
        Final updated graph after removing deleted hypotheses
      
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
      

---

    Code
      graph_update(g, c(FALSE, FALSE, FALSE, TRUE))
    Output
      Initial and final graphs -------------------------------------------------------
      
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
      
      Updated graph after deleting hypothesis 4
      
      --- Hypothesis weights ---
      H1: 0.5
      H2: 0.5
      H3: 0.0
      H4:  NA
      
      --- Transition weights ---
          H1 H2 H3 H4
       H1  0  0  1 NA
       H2  1  0  0 NA
       H3  0  1  0 NA
       H4 NA NA NA NA

---

    Code
      graph_update(g, c(1, 2, 4))
    Output
      Initial and final graphs -------------------------------------------------------
      
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
      
      Updated graph after deleting hypotheses 1, 2, 4
      
      --- Hypothesis weights ---
      H1: NA
      H2: NA
      H3:  1
      H4: NA
      
      --- Transition weights ---
          H1 H2 H3 H4
       H1 NA NA NA NA
       H2 NA NA NA NA
       H3 NA NA  0 NA
       H4 NA NA NA NA
      
      Deletion sequence ($intermediate_graphs) ---------------------------------------
      
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
      
          Step 1: Updated graph after removing hypothesis 1
      
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
      
            Step 2: Updated graph after removing hypotheses 1, 2
      
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
      
              Step 3: Updated graph after removing hypotheses 1, 2, 4
      
              --- Hypothesis weights ---
              H1: NA
              H2: NA
              H3:  1
              H4: NA
      
              --- Transition weights ---
                 H1 H2 H3 H4
              H1 NA NA NA NA
              H2 NA NA NA NA
              H3 NA NA  0 NA
              H4 NA NA NA NA
      
        Final updated graph after removing deleted hypotheses
      
        --- Hypothesis weights ---
        H1: NA
        H2: NA
        H3:  1
        H4: NA
      
        --- Transition weights ---
           H1 H2 H3 H4
        H1 NA NA NA NA
        H2 NA NA NA NA
        H3 NA NA  0 NA
        H4 NA NA NA NA
      

