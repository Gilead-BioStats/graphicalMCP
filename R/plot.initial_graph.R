#' S3 plot method for the class `initial_graph`
#'
#' @param graph
#' @param layout
#' @param groups
#' @param groups_layout
#' @param nrow
#' @param ncol
#' @param edge_curves
#' @param precision
#' @param epsilon
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot.initial_graph <- function(graph,
                               layout = igraph::layout_nicely,
                               groups = list(graph$hypotheses),
                               groups_layout = NULL,
                               nrow = NULL,
                               ncol = NULL,
                               edge_curves = NULL,
                               precision = 4,
                               eps = .001,
                               background_color = "white",
                               ...) {
  graph_size <- length(graph$hypotheses)
  graph_names <- names(graph$hypotheses)
  graph_seq <- seq_along(graph$hypotheses)

  graph_igraph <- as_igraph(graph)

  v_attr <- igraph::vertex_attr(graph_igraph)
  e_attr <- igraph::edge_attr(graph_igraph)

  # make labels ----------------------------------------------------------------
  v_labels <- paste(v_attr$name, round(v_attr$weight, precision), sep = "\n")

  # very small edges should display as epsilon
  edge_labels <- e_attr$weight

  near_0 <- edge_labels <= eps & edge_labels != 0
  near_1 <- edge_labels >= 1 - eps & edge_labels != 1

  edge_labels[!near_0 & !near_1] <-
    round(edge_labels[!near_0 & !near_1], precision)
  edge_labels[near_0] <- expression(epsilon)
  edge_labels[near_1] <- expression(1 - epsilon)

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
    attr(igraph::E(graph_igraph), "vnames") %in% edge_pairs(graph)

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

  graphics::par(bg = background_color)

  # draw! ----------------------------------------------------------------------
  igraph::plot.igraph(
    graph_igraph,
    ...,
    layout = layout,
    vertex.color = "#e8c2ff",
    # vertex.frame.color = "#e8c2ff",
    vertex.label = v_labels,
    vertex.label.color = "black",
    vertex.size = 20,
    edge.label = edge_labels,
    edge.curved = curve,
    edge.arrow.size = .5,
    edge.arrow.width = 1
  )
}
