# graph examples ---------------------------------------------------------------

# trivial graph
g_1 <- matrix(0, nrow = 1)
w_1 <- 1

g_1_cnm <- g_1
g_1_rnm <- g_1
w_1_nm <- w_1
colnames(g_1_cnm) <- "col_node"
rownames(g_1_rnm) <- "row_node"
names(w_1_nm) <- "my_node"

# Bonferroni-Holm: 2
g_2 <- matrix(c(0, 1, 1, 0), nrow = 2, byrow = TRUE)
w_2 <- c(.5, .5)

# weights outside 0-1
w_under <- c(-1, .5)
w_over <- c(.5, 1.5)

# weights sum to > 1
w_sum_over <- c(.5, .75)

# Bonferroni-Holm: 3
g_3 <- matrix(c(0, .5, .5, .5, 0, .5, .5, .5, 0), nrow = 3, byrow = TRUE)
w_3 <- c(.33333333, .33333333, .33333333)

# transitions outside 0-1
g_under <- g_3
g_under[1, 2] <- -1

g_over <- g_3
g_over[1, 2] <- 1.5

# transition diagonal != 0
g_diag <- g_3
g_diag[1, ] <- c(.5, 0, .5)

# transitions sum to > 1
g_sum_over <- g_3
g_sum_over[1, ] <- c(0, .75, .75)

# tests ------------------------------------------------------------------------

test_that("create a trivial graph", {
  expect_true(is_mcp_graph(new_mcp_graph(g_1, w_1)))
})

test_that("transition/hypothesis weights must be numeric", {
  expect_error(new_mcp_graph(g_1, "1"))
  expect_error(new_mcp_graph(matrix("0"), w_1))
})

test_that("hypothesis weights range and sum", {
  expect_error(mcp_graph(g_2, w_under))
  expect_error(mcp_graph(g_2, w_over))
  expect_error(mcp_graph(g_2, w_sum_over))
})

test_that("transition weights range, sum, and diagonal", {
  expect_error(mcp_graph(g_under, w_3))
  expect_error(mcp_graph(g_over, w_3))
  expect_error(mcp_graph(g_diag, w_3))
  expect_error(mcp_graph(g_sum_over, w_3))
})

test_that("names validation", {
  expect_true(is_mcp_graph(mcp_graph(g_1, w_1, "node")))
  expect_warning(mcp_graph(g_1, w_1_nm, "node"))
  expect_error(mcp_graph(g_1_nm, w_1))
  expect_equal(names(mcp_graph(g_1, w_1)$hyp_wgts), "H1")
})
