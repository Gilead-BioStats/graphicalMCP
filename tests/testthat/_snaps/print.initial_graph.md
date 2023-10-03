# snapshot print method

    Code
      graph_create(c(0.5, 0.5), matrix(c(0, 1, 1, 0), nrow = 2))
    Output
      Initial graph
      
      --- Hypothesis weights ---
      H1: 0.5
      H2: 0.5
      
      --- Transition weights ---
          H1 H2
       H1  0  1
       H2  1  0

---

    Code
      graph_update(graph_create(1, matrix(0, nrow = 1)), TRUE)$updated_graph
    Output
      Updated graph
      
      --- Hypothesis weights ---
      H1: NA
      
      --- Transition weights ---
          H1
       H1 NA

