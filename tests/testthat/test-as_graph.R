if (requireNamespace("gMCP", quietly = TRUE)) {
  test_that("round-trip graph coercion - gMCP", {
    g <- random_graph(11)

    expect_equal(g, as_initial_graph(as_graphMCP(g)))
  })
}

if (requireNamespace("igraph", quietly = TRUE)) {
  test_that("round-trip graph coercion - igraph", {
    g <- random_graph(11)

    expect_equal(g, as_initial_graph(as_igraph(g)))
  })
}
