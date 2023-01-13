# 2 basic endpoints

    Code
      analyze_graph(g)
    Output
      graph is optimal
      [1] TRUE

---

    Code
      analyze_graph(g2)
    Output
      graph is sub-optimal:
      
         H1 H2 H3 H4   H1  H2   H3   H4
      3   0  0  1  1 0.00 0.0 5.00 0.50
      4   0  0  0  1 0.00 0.0 0.00 0.50
      5   0  0  1  0 0.00 0.0 5.50 0.00
      6   0  1  0  1 0.00 5.5 0.00 0.00
      7   0  1  0  0 0.00 5.5 0.00 0.00
      9   1  0  1  1 0.50 0.0 0.45 0.50
      10  1  0  0  1 0.50 0.0 0.00 0.95
      11  1  0  0  0 1.45 0.0 0.00 0.00
      12  1  0  1  0 1.00 0.0 0.45 0.00
      [1] FALSE

