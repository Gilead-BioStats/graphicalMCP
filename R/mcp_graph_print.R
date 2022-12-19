print_title <- function() {
	"An mcp_graph" # TODO: Explore {crayon} for using bold or colored text
}

print_hyps <- function(g) {
	max_h_length <- nchar(max(g))
	h_nums <- formatC(paste0("H", unclass(g)), width = max_h_length + 1)
	h_names <- names(g)

	hyps_text <- paste(
		h_nums,
		": ",
		h_names,
		sep = "",
		collapse = "\n"
	)

	paste0(
		"--- Hypothesis names ---\n",
		hyps_text
	)
}

print_hyp_weights <- function(g) {
	if (is.null(get_hyp_weights(g))) {
		hyp_weights_text <- "Add hypothesis/node weights with `set_hyp_weights()`"
	} else {
		hyp_weights_text <- paste(
			formatC(names(g), width = max(nchar(names(g)))),
			": (",
			formatC(get_hyp_weights(g), digits = 4, format = "f"),
			")",
			sep = "",
			collapse = "\n"
		)
	}

	paste(
		"--- Hypothesis weights ---",
		hyp_weights_text,
		sep = "\n"
	)
}

print_trns_weights <- function(g) {
	if (is.null(get_trns_weights(g))) {
		trns_weights_text <- "Add transition weights with `set_trns_weights()`"
	} else {
		trns_weights_text <- data.frame(
			formatC(
				get_trns_weights(g),
				digits = 4,
				format = "f"
			)
		)
	}

	trns_weights_text
}

print_subgraphs <- function(g) {
	if (is.null(get_subgraphs(g))) {
		subgraphs_text <- "Calculate subgraphs with `calc_subgraphs()`"
	} else {
		subgraphs_text <- data.frame(
			formatC(
				get_subgraphs(g),
				digits = 4,
				format = "f"
			)
		)
	}

	subgraphs_text
}

#' @export
print.mcp_graph <- function(g) {
	print_graph <- paste(
		print_title(),
		print_hyps(g),
		print_hyp_weights(g),
		sep = "\n\n"
	)

	cat(print_graph, "", sep = "\n")

  cat("--- Transition weights ---\n")
	if (class(print_trns_weights(g)) == "data.frame") {
		print(print_trns_weights(g))
	} else {
  	cat(print_trns_weights((g)))
	}

  cat("\n--- Subgraph weights ---\n") # TODO: Put in NAs for missing nodes in subgraph matrix
  if (class(print_subgraphs(g)) == "data.frame") {
  	print(print_subgraphs(g))
  } else {
  	cat(print_subgraphs((g)))
  }
}
