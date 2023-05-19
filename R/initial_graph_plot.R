plot.initial_graph <- function(graph,
                               layout = igraph::layout.auto,
                               groups = list(graph$hypotheses),
                               groups_layout = NULL,
                               nrow = NULL,
                               ncol = NULL,
                               edge_curves = NULL,
                               precision = 4,
                               epsilon = .001) {
  graph_size <- length(graph$hypotheses)
  graph_names <- names(graph$hypotheses)

  # structure igraph object ----------------------------------------------------
  all_vert_cross <- rev(expand.grid(
    end = seq_along(graph$hypotheses),
    start = seq_along(graph$hypotheses)
  ))

  which_edge <- apply(
    all_vert_cross,
    1,
    function(row) graph$transitions[row[[1]], row[[2]]]
  ) != 0

  df_edges <- all_vert_cross[which_edge, ]

  graph_igraph <- make_directed_graph(t(df_edges))
  graph_igraph <- set_vertex_attr(graph_igraph, "name", value = graph_names)
  graph_igraph <- set_vertex_attr(graph_igraph, "label", value = graph_names)

  # make labels ----------------------------------------------------------------
  vert_labels <- paste(
    graph_names,
    round(graph$hypotheses, precision),
    sep = "\n"
  )
  edge_labels <- round(
    t(graph$transitions)[t(graph$transitions) > 0],
    precision
  )

  edge_labels[edge_labels <= epsilon & edge_labels != 0] <- "e"
  edge_labels[edge_labels >= 1 - epsilon & edge_labels != 1] <- "1 - e"

  # set curves -----------------------------------------------------------------
  curve <- rep(0, length(E(graph_igraph)))
  names(curve) <- attr(E(graph_igraph), "vnames")

  # vertex pairs connected in both directions should get a small default so
  # their edges don't overlap each other
  edge_pair_locs <- attr(E(graph_igraph), "vnames") %in% edge_pairs(graph)
  curve[edge_pair_locs] <- .25

  curve[names(edge_curves)] <- edge_curves

  # set layout -----------------------------------------------------------------
  if (!is.function(layout)) {
    if (layout == "grid") {
      if (is.null(nrow) && is.null(ncol)) {
        nrow <- ceiling(sqrt(graph_size))
        ncol <- ncol
      } else if(is.null(nrow)) {
        nrow <- ceiling(graph_size / ncol)
      } else if(is.null(ncol)) {
        nrow <- ceiling(graph_size / nrow)
      }

      layout <- matrix(
        nrow = graph_size,
        ncol = 2,
        dimnames = list(graph_names, c("x", "y"))
      )

      # [] removes extras
      layout[, "x"] <- rep(seq_len(ncol), nrow)[seq_len(graph_size)]
      layout[, "y"] <- do.call(
        c,
        lapply(rev(seq_len(nrow)), rep, ncol)
      )[seq_len(graph_size)]
    }
  }

  # draw! ----------------------------------------------------------------------
  plot(
    graph_igraph,

    layout = layout,

    # vertex.label = vert_labels,

    edge.label = edge_labels,
    edge.curved = curve,


    edge.arrow.size = .5,
    edge.arrow.width = 1
  )
}
