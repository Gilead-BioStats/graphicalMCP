test_that("snapshot print method", {
  expect_snapshot(graph_create(c(.5, .5), matrix(c(0, 1, 1, 0), nrow = 2)))
  expect_snapshot(
    graph_update(graph_create(1, matrix(0, nrow = 1)), FALSE)$updated_graph
  )
})
