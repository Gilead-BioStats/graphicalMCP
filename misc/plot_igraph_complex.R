library(igraph)
devtools::load_all()

eps <- .0001

weights <- c(rep(c(1 / 3, 0, 0), 3))
transitions <- rbind(
  c(0, .5, .5, 0, 0, 0, 0, 0, 0),                 # 1 --> 2 & 3
  c(0, 0, 1, 0, 0, 0, 0, 0, 0),                   # 2 --> 3
  c(0, 1 - eps, 0, eps, 0, 0, 0, 0, 0), # 3 --> 2, 3 - - > 4 & 7
  c(0, 0, 0, 0, .5, .5, 0, 0, 0),                 # 4 --> 5 & 6
  c(0, 0, 0, 0, 0, 1, 0, 0, 0),                   # 5 --> 6
  c(0, 0, 0, 0, 1 - eps, 0, eps, 0, 0), # 6 --> 5, 6 - - > 1 & 7
  c(0, 0, 0, 0, 0, 0, 0, .5, .5),                 # 7 --> 8 & 9
  c(0, 0, 0, 0, 0, 0, 0, 0, 1),                   # 8 --> 9
  c(eps, 0, 0, 0, 0, 0, 0, 1 - eps, 0)  # 9 --> 8, 9 - - > 1 & 4
)

g <- create_graph(weights, transitions)

# by name
# names_cross <- rev(expand.grid(
#   end = names(g$hypotheses),
#   start = names(g$hypotheses),
#   stringsAsFactors = FALSE
# ))
# # names_cross <- names_cross[order(names_cross$start), ]
#
# edge_rows <- apply(
#   names_cross,
#   1,
#   \(row) g$transitions[row[[1]], row[[2]]]
# ) != 0
#
# df_edges <- names_cross[edge_rows, ]

# by number
v_cross <- rev(expand.grid(
  end = seq_along(g$hypotheses),
  start = seq_along(g$hypotheses)
))

edge_rows <- apply(
  v_cross,
  1,
  \(row) g$transitions[row[[1]], row[[2]]]
) != 0

df_edges <- v_cross[edge_rows, ]

igraph <- make_directed_graph(t(df_edges))
igraph <- set_vertex_attr(igraph, "name", value = names(g$hypotheses))

vert_labels <- paste(
  names(V(igraph)),
  round(g$hypotheses[names(V(igraph))], 4),
  sep = "\n"
)
edge_labels <- round(diag(g$transitions[df_edges$start, df_edges$end]), 4)

group_layout <- rbind(c(.2, .5), c(0, 0), c(.4, 0))

complex_layout <- rbind(
  t(t(group_layout) + c(-1, 0)),
  t(t(group_layout) + c(0, 0)),
  t(t(group_layout) + c(1, 0))
)

# igraph structure:
# 1: cardinality
# 2: directed? named?
# 3: edge start (0-index)
# 4: edge end (0-index) - but there's weird naming (7th is 4 in this case)

curve <- rep(0, 15)
# curve[5] <- .3
# curve[6] <- -.1
# curve[16] <- -2
# curve[17] <- -2
curve[c(3, 4, 8, 9, 13, 15)] <- 1 # :4, 8:9, 13, 15
curve[14] <- -.7

igraph |>
  plot(
    # gex
    # layout = rbind(
    #   c(1, 2),
    #   c(1, 1),
    #   c(2, 2),
    #   c(2, 1)
    # ),
    # bh
    # layout = layout_in_circle,
    # wd
    layout = complex_layout,
    vertex.size = 10,
    # vertex.label = vert_labels,
    vertex.color = "#a069c4",
    vertex.label.color = "black",
    # vertex.label.dist = 3,
    # vertex.label.degree = rep(c(-pi/2, pi/2, pi/2), 3),
    # edge.color = "black",
    # edge.label = paste0("\n      ", edge_labels ),
    edge.label = edge_labels,
    edge.label.color = "black",
    # wd
    edge.curved = curve,
    # edge.curved = c(-0.5, -0.5, -1, -1.5, -1.5, -2.5)
    # edge.width = 5,
    # edge.label.y = .5,
    edge.arrow.size = .5,
    edge.arrow.width = 1,
    # rescale = FALSE
  )

