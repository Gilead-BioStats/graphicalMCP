hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- graph_create(hypotheses, transitions, names)

m6 <- matrix(1 / 5, nrow = 6, ncol = 6)
diag(m6) <- 0
bh6 <- graph_create(rep(1 / 6, 6), m6)

test_that("basic updating & structure", {
  expect_s3_class(graph_update(g, c(FALSE, FALSE, TRUE, TRUE)), "updated_graph")
  expect_equal(graph_update(g, c(FALSE, FALSE, FALSE, TRUE))$initial_graph, g)
  expect_equal(graph_update(g, c(1, 2, 3, 4))$initial_graph, g)
  expect_length(graph_update(g, c(FALSE, FALSE, TRUE, TRUE)), 4)
  expect_length(graph_update(g, 1:2), 4)
  expect_equal(
    attr(graph_update(g, c(FALSE, FALSE, TRUE, TRUE))$updated_graph, "title"),
    "Updated graph"
  )
  expect_equal(
    attr(graph_update(g, c(FALSE, FALSE, TRUE, TRUE))$updated_graph, "deleted"),
    3:4
  )
})

test_that("invalid input", {
  expect_error(graph_update(g, c(FALSE, TRUE, TRUE)))
  expect_error(graph_update(g, c(0, 1, 1, "1")))
  expect_error(graph_update(g, c(0, 1, 1, 1)))
  expect_error(graph_update(g, c(1, 2, 3, 3)))
  expect_error(graph_update(g, c(1, 2, 3, 5)))
})

test_that("generate floating point differences", {
  expect_s3_class(
    updated_1 <- graph_update(
      bh6,
      c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
    )$updated_graph,
    "initial_graph"
  )
  expect_s3_class(
    updated_2 <- graph_update(
      updated_1,
      c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
    )$updated_graph,
    "initial_graph"
  )
  expect_s3_class(
    updated_3 <- graph_update(
      updated_2,
      c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
    )$updated_graph,
    "initial_graph"
  )
  expect_false(
    all(
      vapply(
        rowSums(updated_1$transitions),
        function(x) x == 1 || x == 0,
        logical(1)
      )
    )
  )
  expect_false(
    all(
      vapply(
        rowSums(updated_2$transitions),
        function(x) x == 1 || x == 0,
        logical(1)
      )
    )
  )
  expect_false(all(updated_3$hypotheses == 1 | updated_2$hypotheses == 0))
})
