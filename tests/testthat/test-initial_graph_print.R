test_that("snapshot print method", {
  expect_snapshot(create_graph(c(.5, .5), matrix(c(0, 1, 1, 0), nrow = 2)))
  expect_snapshot(
    update_graph(create_graph(1, matrix(0, nrow = 1)), FALSE)$updated_graph
  )
})
