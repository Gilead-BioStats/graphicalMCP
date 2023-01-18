hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- create_graph(hypotheses, transitions, names)

g2 <- g
g2$transitions["H2", "H4"] <- .9

test_that("2 basic endpoints", {
  expect_snapshot(analyze_graph(g))
  expect_snapshot(analyze_graph(g2))
})

test_that("floating point arithmetic for Bonferroni-Holm above n = 6", {
  expect_true(analyze_graph(bonferroni_holm(6)))
})
