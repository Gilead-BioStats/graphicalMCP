library(igraph)

hab <- huque_alosh_bhore_2011()
wd <- wiens_dmitrienko_2005()
bh <- bonferroni_holm(9)
gex <- bretz_etal_2011(names = c("Non-inferiority Low", "Non-inferiority High", "Superiority Low", "Superiority High"))
g <- wd

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
    g$hypotheses[names(V(igraph))],
    sep = "\n"
)
edge_labels <- diag(g$transitions[df_edges$start, df_edges$end])

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
        layout = rbind(c(-1, 0), c(0, 0), c(1, 0)),
        # vertex.size = 30,
        vertex.label = vert_labels,
        vertex.color = "#a069c4",
        vertex.label.color = "black",
        vertex.label.dist = 4,
        vertex.label.degree = c(pi/4, pi/4, -pi/4),
        edge.color = "black",
        edge.label = paste0("      ", edge_labels ),
        edge.label.color = "black",
        # wd
        edge.curved = c(0, 0, -.5, -.5)
        # edge.width = 5,
        # edge.label.y = .5,
        # edge.arrow.size = 2,
        # edge.arrow.width = 5
    )
