#' S3 plot method for class `initial_graph`
#'
#' The plot of an `initial_graph` translates the `hypotheses` into vertices and
#' `transitions` into edges to create a network plot. Vertices are labeled with
#' hypothesis names and weights, and edges are labeled with weights.
#'
#' There are a few values for [igraph::plot.igraph()] that get their defaults
#' changed for graphicalMCP. These values can still be changed by passing them
#' as arguments to `plot.initial_graph()`. Here are the new defaults:
#'   * vertex.color = "#e8c2ff",
#'   * vertex.label.color = "black",
#'   * vertex.size = 20,
#'   * edge.arrow.size = 1,
#'   * edge.arrow.width = 1,
#'   * asp = 0
#'
#' Neither graphicalMCP nor igraph does anything about overlapping edge labels.
#' If you run into this problem, and vertices can't practically be moved enough
#' to avoid collisions of edge labels, using edge curves can help. igraph puts
#' edge labels closer to the tail of an edge when an edge is straight, and
#' closer to the head of an edge when it's curved. By setting an edge's curve to
#' some very small value, an effectively straight edge can be shifted to a new
#' position.
#'
#' @param x An initial graph as returned by [graph_create()]
#' @param ... Other arguments passed on to [igraph::plot.igraph()]
#' @param layout An igraph layout specification (See `?igraph.plotting`), or
#'   `"grid"`, which lays out hypotheses left-to-right and top-to-bottom. `nrow`
#'   and `ncol` control the grid shape
#' @param nrow An integer scalar specifying the number of rows in the vertex
#'   grid. If row and column counts are not specified, vertices will be laid out
#'   as close to a square as possible.
#' @param ncol An integer scalar specifying the number of columns in the vertex
#'   grid. If row and column counts are not specified, vertices will be laid out
#'   as close to a square as possible.
#' @param edge_curves A named numeric vector specifying the curvature  of
#'   specific edges. Edge pairs (Where two vertices share an edge in each
#'   possible direction) are detected automatically and get 0.25 curvature.
#'   Adjust edges by adding an entry with name `"vertex1|vertex2`, and adjust
#'   default edge pairs curvature by adding an entry with name `"pairs"` -
#'   `edge_curves = c("pairs" = .5, "H1|H3" = .25, "H3|H4" = .75)`
#' @param precision An integer scalar specifying how many significant figures
#'   should be displayed for weights
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
#'
#' hypotheses <- c(0.5, 0.5, 0, 0, 0, 0)
#'
#' epsilon <- 1e-5
#' transitions <- rbind(
#'   c(0,       0.5,     0.25,        0,    0.25, 0),
#'   c(0.5,     0,       0,           0.25, 0,    0.25),
#'   c(0,       0,       0,           0,    1,    0),
#'   c(epsilon, 0,       0,           0,    0,    1 - epsilon),
#'   c(0,       epsilon, 1 - epsilon, 0,    0,    0),
#'   c(0,       0,       0,           1,    0,    0)
#' )
#'
#' g <- graph_create(hypotheses, transitions)
#'
#' plot_layout <- rbind(
#'   c(.15, .5),
#'   c(.65, .5),
#'   c(  0,  0),
#'   c( .5,  0),
#'   c( .3,  0),
#'   c( .8,  0)
#' )
#'
#' plot(g, layout = plot_layout, eps = epsilon, edge_curves = c(pairs = .5))
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

  # Make labels ----------------------------------------------------------------
  v_labels <- paste(v_attr$name, round(v_attr$weight, precision), sep = "\n")

  # Very small edges should display as epsilon
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

  # Set curves -----------------------------------------------------------------
  curve <- rep(0, length(igraph::E(graph_igraph)))
  names(curve) <- attr(igraph::E(graph_igraph), "vnames")

  # Vertex pairs connected in both directions should get a small default so
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

  # Set layout -----------------------------------------------------------------
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

        # [] removes extras when grid is not filled all the way
        layout <- cbind(
          rep(seq_len(ncol), nrow)[graph_seq],
          vapply(rev(seq_len(nrow)), rep, integer(ncol), ncol)[graph_seq]
        )
      }
    }
  }

  graphics::par(mar = margins)
  graphics::par(bg = background_color)

  # Draw! ----------------------------------------------------------------------
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
