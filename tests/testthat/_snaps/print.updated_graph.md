# snapshot print method

    Code
      update_graph(g, c(TRUE, TRUE, TRUE, FALSE))
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
      
      --------------------------------------------------------------------------------
      
      --- Hypotheses kept ---
         H1   H2   H3    H4
       TRUE TRUE TRUE FALSE
      
      --------------------------------------------------------------------------------
      
      Updated graph
      
      --- Hypothesis weights ---
      H1: 0.5
      H2: 0.5
      H3: 0.0
      H4: 0.0
      
      --- Transition weights ---
          H1 H2 H3 H4
       H1  0  0  1  0
       H2  1  0  0  0
       H3  0  1  0  0
       H4  0  0  0  0

