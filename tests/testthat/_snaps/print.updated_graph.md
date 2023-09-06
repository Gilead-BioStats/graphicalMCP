# snapshot print method

    Code
      graph_update(g, c(FALSE, FALSE, FALSE, TRUE))
    Output
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

