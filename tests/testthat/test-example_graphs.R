test_that("basic creation", {
  expect_equal(
    bonferroni_holm(3),
    structure(
      list(
        hypotheses = c(
          H1 = 0.333333333333333,
          H2 = 0.333333333333333,
          H3 = 0.333333333333333
        ),
        transitions = structure(
          c(0, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0),
          dim = c(3L, 3L),
          dimnames = list(c("H1", "H2", "H3"), c("H1", "H2", "H3"))
        )
      ),
      class = "initial_graph"
    )
  )
})
