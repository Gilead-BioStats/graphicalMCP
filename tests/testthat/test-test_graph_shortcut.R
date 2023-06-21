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

  expect_s3_class(
    test_graph_shortcut(rando, p, critical = TRUE)$critical$results,
    "data.frame"
  )

  expect_type(
    test_graph_shortcut(rando, p, verbose = TRUE)$details$results,
    "list"
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

test_that("shortcut testing handles 0 cases", {
  g_zero_1 <- create_graph(c(.5, .5, 0), matrix(0, 3, 3))
  g_zero_2 <- create_graph(rep(0, 3), matrix(0, 3, 3))

  p_zero_1 <- c(1, 0, 0)
  p_zero_2 <- rep(0, 3)

  expect_error(
    test_graph_shortcut(g_zero_1, p_zero_1),
    regexp = "All weights and p-values are 0"
  )
  expect_error(
    test_graph_shortcut(g_zero_1, p_zero_1),
    regexp = "All weights and p-values are 0"
  )
  expect_error(
    test_graph_shortcut(g_zero_1, p_zero_1),
    regexp = "All weights and p-values are 0"
  )
  expect_error(
    test_graph_shortcut(g_zero_1, p_zero_1),
    regexp = "All weights and p-values are 0"
  )

  expect_equal(
    test_graph_shortcut(g_zero_2, rep(.001, 3))$outputs$p_adj,
    rep(1, 3),
    ignore_attr = TRUE
  )

  expect_equal(
    test_graph_shortcut(bonferroni_holm(3), p_zero_2)$outputs$p_adj,
    rep(0, 3),
    ignore_attr = TRUE
  )

  expect_no_error(test_graph_shortcut(bonferroni_holm(3), p_zero_2))
})

test_that("shortcut internal consistency", {
  rando <- random_graph(6)
  p <- pnorm(rnorm(6, 2), lower.tail = FALSE)

  shortcut_results <- test_graph_shortcut(rando, p, .025, TRUE, TRUE)

  expect_equal(
    shortcut_results$inputs$graph,
    shortcut_results$details$results[[1]],
    ignore_attr = TRUE
  )

  critical_sequence <- shortcut_results$critical$results$Hypothesis
  expect_equal(
    shortcut_results$inputs$p[critical_sequence],
    shortcut_results$critical$results$p,
    ignore_attr = TRUE
  )

  expect_equal(
    shortcut_results$inputs$alpha,
    shortcut_results$critical$results$Alpha[[1]],
    ignore_attr = TRUE
  )

  expect_equal(
    shortcut_results$outputs$rejected[critical_sequence],
    shortcut_results$critical$results$Reject,
    ignore_attr = TRUE
  )

  last_graph_index <- length(shortcut_results$details$results)
  expect_equal(
    shortcut_results$outputs$graph,
    shortcut_results$details$results[[last_graph_index]],
    ignore_attr = TRUE
  )
})
