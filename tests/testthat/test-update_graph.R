hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- create_graph(hypotheses, transitions, names)

test_that("basic updating & structure", {
  expect_s3_class(update_graph(g, c(0, 0, 0, 1)), "updated_graph")
  expect_equal(update_graph(g, c(FALSE, FALSE, FALSE, TRUE))$initial_graph, g)
})

test_that("invalid input", {
  expect_error(update_graph(g, c(0, 1, 1)))
  expect_error(update_graph(g, c(0, 1, 1, 1.1)))
})
