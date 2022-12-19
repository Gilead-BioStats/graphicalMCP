#' Set transition weights for a multiple comparison procedure graph
#'
#' @export
set_trns_weights <- function(g, weights) {
	UseMethod("set_trns_weights")
}

#' @export
set_trns_weights.mcp_graph <- function(g, weights) {
	stopifnot(length(g) == nrow(weights)) # TODO: Informative errors
	stopifnot(length(g) == ncol(weights))
	stopifnot(is.numeric(g))

	rownames(weights) <- names(g)
	colnames(weights) <- names(g)

	attr(g, "trns_weights") <- weights
	g
}


#' Get transition weights for a multiple comparison procedure graph
#'
#' @export
get_trns_weights <- function(g) {
	UseMethod("get_trns_weights")
}

#' @export
get_trns_weights.mcp_graph <- function(g) {
	attr(g, "trns_weights")
}
