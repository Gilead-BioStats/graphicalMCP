library(igraph)
devtools::load_all()

eps <- .0001

# ex 1 -------------------------------------------------------------------------
g1 <- complex_example_1()

igraph <- as_igraph(g1)

group_layout <- rbind(c(.15, .5), c(0, 0), c(.3, 0))

complex_layout <- rbind(
  t(t(group_layout) + c(0, 0)),
  t(t(group_layout) + c(.5, 0))
)

plot(
  g1,
  precision = 2,
  layout = complex_layout,
  edge_curves = c("pairs" = 1),
  vertex.size = 15,
  asp = .4,
  edge.label.cex = 1.2,
  edge.arrow.size = 1
  )

# ex 2 -------------------------------------------------------------------------
g2 <- complex_example_2()

igraph <- as_igraph(g2)

group_layout2 <- rbind(c(.3, .5), c(0, 0), c(.6, 0))

complex_layout2 <- rbind(
  t(t(group_layout2) + c(-1, 0)),
  t(t(group_layout2) + c(0, 0)),
  t(t(group_layout2) + c(1, 0))
)

complex_layout2[, 1] <- complex_layout2[, 1] / max(abs(complex_layout2[, 1]))
complex_layout2[, 2] <- complex_layout2[, 2] / max(abs(complex_layout2[, 2]))

plot(
  g2,
  precision = 2,
  layout = complex_layout2,
  edge_curves = c("pairs" = 1,
                  "H3|H7" = .05,
                  "H9|H1" = .05),
  vertex.size = 15,
  asp = .4,
  edge.label.cex = 1.2,
  edge.arrow.size = 1
)
