hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- create_graph(hypotheses, transitions, names)
gw4 <- generate_weights(g)
gw4_gmcp <- gMCP::generateWeights(g$transitions, g$hypotheses)

bh10 <- bonferroni_holm(10)
gw10 <- generate_weights(bh10)
gw10_gmcp <- gMCP::generateWeights(bh10$transitions, bh10$hypotheses)

test_that("compare to gMCP", {
  expect_true(compare_gw(gw4, gw4_gmcp))
  expect_true(compare_gw(gw10, gw10_gmcp))
})
