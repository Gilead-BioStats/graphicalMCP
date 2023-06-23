# snapshot print method

    Code
      create_graph(c(0.5, 0.5), matrix(c(0, 1, 1, 0), nrow = 2))
    Output
      Initial graph
      
      --- Hypothesis weights ---
      H1: 0.5000
      H2: 0.5000
      
      --- Transition weights ---
              H1     H2
       H1 0.0000 1.0000
       H2 1.0000 0.0000

---

    Code
      update_graph(create_graph(1, matrix(0, nrow = 1)), FALSE)$updated_graph
    Output
      Initial graph
      
      --- Hypothesis weights ---
      H1: 0.0000
      
      --- Transition weights ---
              H1
       H1 0.0000

