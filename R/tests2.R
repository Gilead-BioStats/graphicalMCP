simple_bonferroni <- function(p, w, a) {
  paste(p, "<=", w, "*", a) |> cat(sep = "\n")
  cat("\n")
  p <= w * a
}

simple_parametric <- function(p, w, a, r = NULL) {
  c <- 1

  paste(p, "<=", c, "*", w, "*", a) |> cat(sep = "\n")
  cat("\n")
  p <= c * w * a
}

simple_simes <- function(p, w, a) {
  w <- cumsum(w[order(p)])
  paste(p, "<=", w, "*", a) |> cat(sep = "\n")
  cat("\n")
  p <= w * a
}
#
# # demonstrative examples -------------------------------------------------------
# # Parallel gate-keeping
# hypotheses <- c(0.5, 0.5, 0, 0)
# transitions <- rbind(
#   c(0, 0, 1, 0),
#   c(0, 0, 0, 1),
#   c(0, 1, 0, 0),
#   c(1, 0, 0, 0)
# )
# names <- c("A1", "A2", "B1", "B2")
# g_dose <- create_graph(hypotheses, transitions, names)
#
# p <- c(.024, .01, .026, .027)
#
# gw <- generate_weights(g_dose)
#
# # Row 13 is all 0
# apply(gw[, 5:8], 1, simple_bonferroni, p = p, a = .05) |> t() |> cbind(gw)
#
# apply(gw[, 5:8], 1, simple_parametric, p = p, a = .05) |> t() |> cbind(gw)
#
# # A bunch more passing
# apply(gw[, 5:8], 1, simple_simes, p = p, a = .05) |> t() |> cbind(gw)
#
# # Bonferroni
# bh <-
