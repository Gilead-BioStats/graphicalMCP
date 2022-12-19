#' Set hypothesis weights for a multiple comparison procedure graph
#'
#' @export
set_hyp_weights <- function(g, weights) {
	UseMethod("set_hyp_weights")
}

#' @export
set_hyp_weights.mcp_graph <- function(g, weights) {
	stopifnot(length(g) == length(weights)) # TODO: Informative errors

	attr(g, "hyp_weights") <- as.numeric(weights) # TODO: Error for non-numeric
	g
}


#' Get hypothesis weights for a multiple comparison procedure graph
#'
#' @export
get_hyp_weights <- function(g) {
	UseMethod("get_hyp_weights")
}

#' @export
get_hyp_weights.mcp_graph <- function(g) {
	attr(g, "hyp_weights")
}
