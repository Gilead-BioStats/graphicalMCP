library(igraph)
devtools::load_all()

eps <- .0001

g <- complex_example()

igraph <- as_igraph(g)

group_layout <- rbind(c(.3, .5), c(0, 0), c(.6, 0))

complex_layout <- rbind(
  t(t(group_layout) + c(-1, 0)),
  t(t(group_layout) + c(0, 0)),
  t(t(group_layout) + c(1, 0))
)

plot(
  g,
  precision = 2,
  layout = complex_layout,
  edge_curves = c("pairs" = 1,
                  "H3|H7" = .05,
                  "H9|H1" = .05),
  vertex.size = 15,
  margin = -.1,
  asp = .5,
  edge.label.cex = 1.2,
  edge.arrow.size = 1
)
