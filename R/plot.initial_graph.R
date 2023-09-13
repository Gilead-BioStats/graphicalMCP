#' The plot of an `initial_graph` includes the hypotheses and edges between
#' them, along with their weights
#'
#' @param x An initial graph as returned by [graph_create()]
#' @param ... Other arguments passed on to [igraph::plot.igraph()]
#' @param layout An igraph layout specification (See `?igraph.plotting`), or
#'   `"grid"`, which lays out hypotheses left-to-right, top-to-bottom with
#'   `nrow` rows and `ncol` columns.
#' @param nrow An integer scalar specifying the number of rows in the vertex
#'   grid. If row and columns counts are not specified, vertices will be laid
#'   out as close to a square as possible.
#' @param ncol An integer scalar specifying the number of columns in the vertex
#'   grid. If row and columns counts are not specified, vertices will be laid
#'   out as close to a square as possible.
#' @param edge_curves A named numeric vector specifying the curvature edges.
#'   Edge pairs (Where two vertices share an edge in each possible direction)
#'   are detected automatically and get 0.25 curvature. Adjust edges by adding
#'   an entry with name `"vertex1|vertex2`. Adjust default edge pairs curvature
#'   by adding an entry with name `"pairs"`.
#' @param precision An integer scalar specifying how many decimal places should
#'   be displayed for weights
#' @param eps A numeric scalar. Edge weights between 0 and `eps` will be
#'   displayed as \eqn{\epsilon}, and edge weights between `1 - eps` and 1 will
#'   be displayed as \eqn{1 - \epsilon}
#' @param background_color A character scalar specifying a background color for
#'   the whole plotting area. Passed directly to [graphics::par()] (`bg`)
#' @param margins A length 4 numeric vector specifying the margins for the plot.
#'   Defaults to all 0, since igraph plots tend to have large margins. It is
#'   passed directly to [graphics::par()] (`mar`)
#'
#' @return NULL, after plotting the graph
#' @export
#'
#' @examples
#' plot(simple_successive_2(), layout = "grid")
plot.initial_graph <- function(x,
                               ...,
                               layout = "grid",
                               nrow = NULL,
                               ncol = NULL,
                               edge_curves = NULL,
                               precision = 4,
                               eps = NULL,
                               background_color = "white",
                               margins = c(0, 0, 0, 0)) {
  graph_size <- length(x$hypotheses)
  graph_seq <- seq_along(x$hypotheses)

  graph_igraph <- as_igraph(x)

  v_attr <- igraph::vertex_attr(graph_igraph)
  e_attr <- igraph::edge_attr(graph_igraph)

  # make labels ----------------------------------------------------------------
  v_labels <- paste(v_attr$name, round(v_attr$weight, precision), sep = "\n")

  # very small edges should display as epsilon
  edge_labels <- e_attr$weight


  near_0 <- edge_labels <= eps & edge_labels != 0
  near_1 <- edge_labels >= 1 - eps & edge_labels != 1

  if (length(near_0) == 0 || length(near_1) == 0) {
    edge_labels <- round(edge_labels, precision)
  } else {
    edge_labels[!near_0 & !near_1] <-
      round(edge_labels[!near_0 & !near_1], precision)
  }

  if (!is.null(eps)) {
    edge_labels[near_0] <- expression(epsilon)
    edge_labels[near_1] <- expression(1 - epsilon)
  }

  # set curves -----------------------------------------------------------------
  curve <- rep(0, length(igraph::E(graph_igraph)))
  names(curve) <- attr(igraph::E(graph_igraph), "vnames")

  # vertex pairs connected in both directions should get a small default so
  # their edges don't overlap each other
  if (!is.null(edge_curves["pairs"])) {
    edge_pair_curve <- edge_curves["pairs"]
  } else {
    edge_pair_curve <- .25
  }

  edge_pair_locs <-
    attr(igraph::E(graph_igraph), "vnames") %in% edge_pairs(x)

  curve[edge_pair_locs] <- edge_pair_curve

  curve[names(edge_curves)] <- edge_curves

  # set layout -----------------------------------------------------------------
  if (!is.function(layout)) {
    if (!is.matrix(layout)) {
      if (layout == "grid") {
        if (is.null(nrow) && is.null(ncol)) {
          nrow <- ceiling(sqrt(graph_size))
          ncol <- nrow
        } else if (is.null(nrow)) {
          nrow <- ceiling(graph_size / ncol)
        } else if (is.null(ncol)) {
          ncol <- ceiling(graph_size / nrow)
        }

        # [] removes extras when grid is not full
        layout <- cbind(
          rep(seq_len(ncol), nrow)[graph_seq],
          vapply(rev(seq_len(nrow)), rep, integer(ncol), ncol)[graph_seq]
        )
      }
    }
  }

  graphics::par(mar = margins)
  graphics::par(bg = background_color)

  # draw! ----------------------------------------------------------------------
  igraph::plot.igraph(
    graph_igraph,
    ...,
    layout = layout,
    vertex.color = "#e8c2ff",
    vertex.label = v_labels,
    vertex.label.color = "black",
    vertex.size = 20,
    edge.label = edge_labels,
    edge.curved = curve,
    edge.arrow.size = 1,
    edge.arrow.width = 1,
    asp = 0
  )
}
