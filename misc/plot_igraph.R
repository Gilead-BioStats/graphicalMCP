library(igraph)

hab <- huque_etal()
wd <- fallback_improved_1(rep(1 / 3, 3))
bh <- bonferroni_holm(rep(1 / 9, 9))
gex <- simple_successive_1(names = c("Non-inferiority Low",
                                     "Non-inferiority High",
                                     "Superiority Low",
                                     "Superiority High"))
g <- hab

names_cross <- rev(expand.grid(
    end = names(g$hypotheses),
    start = names(g$hypotheses),
    stringsAsFactors = FALSE
))
# names_cross <- names_cross[order(names_cross$start), ]

edge_rows <- apply(
    names_cross,
    1,
    \(row) g$transitions[row[[1]], row[[2]]]
) != 0

df_edges <- names_cross[edge_rows, ]

igraph <- make_directed_graph(t(df_edges))

vert_labels <- paste(
    names(V(igraph)),
    round(g$hypotheses[names(V(igraph))], 4),
    sep = "\n"
)
edge_labels <- round(diag(g$transitions[df_edges$start, df_edges$end]), 4)

t(df_edges) |>
    make_directed_graph() |>
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
        layout = rbind(c(-1, 0), c(0, 0), c(1, 0), c(2, 0)),
        vertex.size = 30,
        vertex.label = vert_labels,
        vertex.color = "#a069c4",
        vertex.label.color = "black",
        # vertex.label.dist = 4,
        vertex.label.degree = c(pi/4, pi/4, -pi/4, -pi/4),
        edge.color = rep(c("black", "purple"), 6),
        edge.label = paste0("\n      ", edge_labels ),
        edge.label = edge_labels,
        edge.label.color = rep(c("black", "purple"), 6),
        # wd
        edge.curved = rep(-1, 6)
        # edge.curved = c(-0.5, -0.5, -1, -1.5, -1.5, -2.5)
        # edge.width = 5,
        # edge.label.y = .5,
        # edge.arrow.size = 2,
        # edge.arrow.width = 5
    )

