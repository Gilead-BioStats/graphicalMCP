if (requireNamespace("gMCP", quietly = TRUE)) {
  test_that("round-trip graph coercion", {
    g <- random_graph(11)

    expect_equal(g, as_graph(as_gmcp_graph(g)))
  })
}
