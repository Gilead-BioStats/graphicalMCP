# 2 basic endpoints

    Code
      analyze_graph(g)
    Message <simpleMessage>
      graph is optimal
      
    Output
      [1] TRUE

---

    Code
      analyze_graph(g2)
    Message <simpleMessage>
      graph is sub-optimal:
      
      
    Output
         H1 H2 H3 H4   H1 H2   H3   H4
      3   0  0  1  1 0.00  0 0.50 0.45
      4   0  0  0  1 0.00  0 0.00 0.90
      5   0  0  1  0 0.00  0 0.95 0.00
      9   1  0  1  1 0.50  0 0.00 0.45
      10  1  0  0  1 0.50  0 0.00 0.45
      11  1  0  0  0 0.95  0 0.00 0.00
      12  1  0  1  0 0.95  0 0.00 0.00
      [1] FALSE

