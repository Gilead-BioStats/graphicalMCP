library(ggraph)
library(tidygraph)

data <- readRDS("StormGraph.RDS")

hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(c(0, 0, 1, 0),
                     c(0, 0, 0, 1),
                     c(0, 1, 0, 0),
                     c(1, 0, 0, 0))
g <- create_graph(hypotheses, transitions)
g

df_nodes <- data.frame(name = names(g$hypotheses), weight = g$hypotheses)
df_edges <- expand.grid(from = names(g$hypotheses), to = names(g$hypotheses))
df_edges$weight <- c(transitions)
df_edges <- df_edges[df_edges$weight != 0, ]

g_graph <- tbl_graph(df_nodes, df_edges)

ggraph(g_graph, layout = matrix(c(1, 10, 1, 10, 10, 10, 1, 1), nrow = 4)) +
    geom_edge_link(aes(label = weight), arrow = arrow(length = unit(.1, "inches"), type = "closed")) +
    geom_node_circle(aes(r = .1)) + coord_fixed() +
    geom_node_text(aes(label = name), repel = TRUE)

graph <- as_tbl_graph(highschool) %>%
    mutate(Popularity = centrality_degree(mode = 'in'))

# broken
# ggraph(graph, layout = 'kk') +
#     geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) +
#     geom_node_point(aes(size = Popularity)) +
#     facet_edges(~year) +
#     theme_graph(foreground = 'steelblue', fg_text_colour = 'white')

graph <- as_tbl_graph(
    data.frame(
        from = sample(5, 20, TRUE),
        to = sample(5, 20, TRUE),
        weight = runif(20)
    )
)

ggraph(graph, layout = 'fr', weights = exp(weight)) +
    geom_edge_link() +
    geom_node_point()

graph <- create_notable('zachary')

ggraph(graph, layout = 'fr') +
    geom_edge_link() +
    geom_node_point(aes(size = centrality_pagerank())) +
    theme(legend.position = 'bottom')
