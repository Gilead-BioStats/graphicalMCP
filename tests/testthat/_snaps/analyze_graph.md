# 2 basic endpoints

    Code
      analyze_graph(g)
    Output
      $is_optimal
      [1] TRUE
      
      $reason
      NULL
      
      $offending
      $offending$subgraphs
           H1 H2 H3 H4 H1 H2 H3 H4
      
      $offending$connectivity
      [1] rows cols
      <0 rows> (or 0-length row.names)
      
      

---

    Code
      analyze_graph(g2)
    Output
      $is_optimal
      [1] FALSE
      
      $reason
      [1] "subgraphs"
      
      $offending
      $offending$subgraphs
         H1 H2 H3 H4   H1 H2   H3   H4
      1   0  0  0  1 0.00  0 0.00 0.90
      2   0  0  1  0 0.00  0 0.95 0.00
      3   0  0  1  1 0.00  0 0.50 0.45
      8   1  0  0  0 0.95  0 0.00 0.00
      9   1  0  0  1 0.50  0 0.00 0.45
      10  1  0  1  0 0.95  0 0.00 0.00
      11  1  0  1  1 0.50  0 0.00 0.45
      
      $offending$connectivity
      [1] rows cols
      <0 rows> (or 0-length row.names)
      
      

