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
      5   1  0  1  1 0.50  0 0.00 0.45
      6   1  0  1  0 0.95  0 0.00 0.00
      7   1  0  0  1 0.50  0 0.00 0.45
      8   1  0  0  0 0.95  0 0.00 0.00
      13  0  0  1  1 0.00  0 0.50 0.45
      14  0  0  1  0 0.00  0 0.95 0.00
      15  0  0  0  1 0.00  0 0.00 0.90
      
      $offending$connectivity
      [1] rows cols
      <0 rows> (or 0-length row.names)
      
      

