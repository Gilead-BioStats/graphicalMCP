# snapshot print method

    Code
      graph(c(0.5, 0.5), matrix(c(0, 1, 1, 0), nrow = 2))
    Output
      An MCP graph
      
      --- Hypothesis weights ---
      H1: (0.5000)
      H2: (0.5000)
      
      --- Transition weights ---
             H1     H2
      H1   --   1.0000
      H2 1.0000   --  

---

    Code
      update_graph(graph(1, matrix(0, nrow = 1)), FALSE)$updated_graph
    Output
      An empty MCP graph

