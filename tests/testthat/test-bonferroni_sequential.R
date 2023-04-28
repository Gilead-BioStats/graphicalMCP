test_that("results match test_graph()", {
  rando <- random_graph(4)
  p <- pnorm(rnorm(4, 2), lower.tail = FALSE)

  expect_equal(
    bonferroni_sequential(rando, p),
    test_graph(rando, p),
    ignore_attr = TRUE
  )

  expect_equal(
    as.integer(bonferroni_sequential(rando, p)$outputs$rejected),
    bs_cpp(rando, p)
  )

  expect_false(
    is.null(bonferroni_sequential(rando, p, critical = TRUE)$critical)
  )

})

test_that("adjusted p-values are capped at 1", {
  expect_equal(
    bonferroni_sequential(random_graph(2), c(1, 1))$outputs$p_adj,
    c(1, 1),
    ignore_attr = TRUE
  )
})
