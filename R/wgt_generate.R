#' Generate weights for the full closure tree of a graph
#'
#' @param g A graph defined as a matrix where entry `i, j` corresponds to the
#'   transition weight between hypotheses `i` and `j`
#' @param w A vector of weights corresponding to the hypotheses in the graph
#'
#' @return Returns a matrix similar to that of `gMCP::generateWeights`: The
#'   first half of columns contains indices for which nodes are left. The second
#'   half of columns contains the weights of the full closure tree of a graph
#'   describing a multiple comparison procedure
#' @export
#'
#' @examples
#'
#' # Example taken from gMCP::generateWeights documentation
#' g <- matrix(
#' 	c(
#' 		0,
#' 		0,
#' 		1,
#' 		0,
#' 		0,
#' 		0,
#' 		0,
#' 		1,
#' 		0,
#' 		1,
#' 		0,
#' 		0,
#' 		1,
#' 		0,
#' 		0,
#' 		0
#' 	),
#' 	nrow = 4,
#' 	byrow = TRUE
#' )
#'
#' ## Choose weights
#' w <- c(.5, .5, 0, 0)
#'
#' generate_weights(g, w)
#'
wgt_generate <- function(g, w) {
	ps <- wgt_powerset(1:length(w))
	ps_indices <- lapply(ps, function(x, y) as.integer(y %in% x), 1:length(w))

	weights <- lapply(ps_indices, function(h, w, g) wgt_delete_nodes(h, w, g), w, g)

	cbind(
		do.call(rbind, ps_indices),
		do.call(rbind, weights)
	)
}
