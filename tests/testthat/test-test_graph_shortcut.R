test_that("results match test_graph_closure()", {
  rando <- random_graph(4)
  p <- pnorm(rnorm(4, 2), lower.tail = FALSE)

  expect_equal(
    test_graph_shortcut(rando, p),
    test_graph_closure(rando, p),
    ignore_attr = TRUE
  )

  expect_equal(
    as.integer(test_graph_shortcut(rando, p)$outputs$rejected),
    test_graph_shortcut_cpp(rando, p)
  )

  expect_false(
    is.null(test_graph_shortcut(rando, p, critical = TRUE)$critical)
  )
})

test_that("adjusted p-values are capped at 1", {
  expect_equal(
    test_graph_shortcut(random_graph(2), c(1, 1))$outputs$p_adj,
    c(1, 1),
    ignore_attr = TRUE
  )
})

test_that("C++ sequential properly assigns new hypotheses each round", {
  g <- simple_successive_2()
  p <- c(
    3.51345772970616e-08,
    7.35572350934915e-06,
    3.31393509575894e-07,
    1.23186608577966e-05
  )

  expect_equal(
    as.logical(test_graph_shortcut_cpp(g, p)),
    unname(test_graph_closure(g, p)$outputs$rejected)
  )
})
