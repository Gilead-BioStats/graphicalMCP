hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- graph_create(hypotheses, transitions, names)

test_that("snapshot print method", {
  expect_snapshot(graph_update(g, integer(0)))

  expect_snapshot(graph_update(g, c(FALSE, FALSE, FALSE, TRUE)))
  expect_snapshot(graph_update(g, c(1, 2, 4)))
})
