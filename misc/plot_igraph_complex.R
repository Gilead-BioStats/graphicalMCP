library(igraph)
devtools::load_all()

eps <- .0001

g <- complex_example(names = paste("DOSE", LETTERS[1:9]))

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
  edge_curves = c("H3|H7" = .05,
  #                 "H3|H4" = .1,
  #                 "H6|H1" = .1,
  #                 "H6|H7" = .1,
  #                 "H9|H4" = .1,
                  "H9|H1" = .1),
  vertex.size = 15,
  margin = -.1,
  asp = .4,
  edge.label.cex = 1.2,
  edge.arrow.size = 1
)

