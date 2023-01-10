hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0))
g_optimal <- graph(hypotheses, transitions)

g_valid <- g_optimal
g_valid$hypotheses[[1]] <- .4

g_invalid <- g_optimal
names(g_invalid$hypotheses) <- 1:4

test_that("check three main endpoints", {
  expect_equal(analyze_graph(g_optimal), "optimal")
  expect_equal(analyze_graph(g_valid), "valid")
  expect_equal(analyze_graph(g_invalid), "invalid")
})
