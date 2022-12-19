new_mcp_graph <- function(hyp_names = 1:4, n_hyps = 4L) {
	stopifnot(is.integer(n_hyps))
	new_graph <- 1:n_hyps
	attributes(new_graph) <- list(
		names = hyp_names,
		class = "mcp_graph"
	)
	new_graph
}

#' `mcp_graph` vector
#'
#' This creates an integer vector that represents the hypotheses of a multiple
#' comparison procedure graph.
#'
#' @param hyp_names A vector of hypothesis names for nodes in the multiple
#'   comparison procedure graph. Defaults to hypothesis numbers if no names are
#'   provided
#' @param n_hyps (Optional) An integer specifying the number of nodes in the
#'   multiple comparison procedure graph
#' @param g An object to test
#' @return An S3 vector of class `mcp_graph`.
#' @export
#' @examples
#' mcp_graph(c("doseA", "doseB"))
#'
#' mcp_graph(n_hyps = 5)
mcp_graph <- function(hyp_names = NULL, n_hyps = NULL) { # TODO: Move some of this validation to an explicit validator
	if (is.null(hyp_names) && is.null(n_hyps)) {
		stop("Please provide a vector of hypothesis names with `hyp_names` or a count of hypotheses with `n_hyps`")
	} else if (!is.null(hyp_names)) {
		if (!is.null(n_hyps) && n_hyps != length(hyp_names)) {
			warn_size_mismatch <- paste0(
				n_hyps,
				" hypotheses specified, but ",
				length(hyp_names),
				" hypothesis names provided\n",
				"  Setting `n_hyps = ",
				length(hyp_names),
				"`"
			)
			warning(warn_size_mismatch)
		}
		n_hyps <- length(hyp_names)
	} else {
		if (n_hyps < 1) {
			stop("Number of hypotheses must be >= 1")
		}
		hyp_names <- paste0("H", 1:n_hyps)
	}

	new_mcp_graph(hyp_names, as.integer(n_hyps))
}

#' @export
#' @rdname mcp_graph
is_mcp_graph <- function(g) {
	inherits(g, "mcp_graph")
}
