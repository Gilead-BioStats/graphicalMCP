test_that("snapshot print method", {
  expect_snapshot(graph(c(.5, .5), matrix(c(0, 1, 1, 0), nrow = 2)))
})
