hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- create_graph(hypotheses, transitions, names)

m6 <- matrix(1/5, nrow = 6, ncol = 6)
diag(m6) <- 0
bh6 <- create_graph(rep(1/6, 6), m6)

test_that("basic updating & structure", {
  expect_s3_class(update_graph(g, c(0, 0, 0, 1)), "updated_graph")
  expect_equal(update_graph(g, c(FALSE, FALSE, FALSE, TRUE))$initial_graph, g)
})

test_that("invalid input", {
  expect_error(update_graph(g, c(0, 1, 1)))
  expect_error(update_graph(g, c(0, 1, 1, "1")))
})

test_that("generate floating point differences", {
  expect_s3_class(
    updated_1 <- update_graph(bh6, c(T, T, T, F, F, F))$updated_graph,
    "initial_graph"
  )
  expect_s3_class(
    updated_2 <- update_graph(updated_1, c(T, T, F, F, F, F))$updated_graph,
    "initial_graph"
  )
  expect_s3_class(
    updated_3 <- update_graph(updated_2, c(T, F, F, F, F, F))$updated_graph,
    "initial_graph"
  )
  expect_false(
    all(sapply(rowSums(updated_1$transitions), function(x) x == 1 || x == 0))
  )
  expect_false(
    all(sapply(rowSums(updated_2$transitions), function(x) x == 1 || x == 0))
  )
  expect_false(all(updated_3$hypotheses == 1 | updated_2$hypotheses == 0))
})
