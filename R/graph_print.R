print_title <- function() {
  "An MCP graph" # TODO: Explore {crayon} for using bold or colored text
}

print_hypotheses <- function(graph) {
  hypotheses_text <- paste(
    formatC(
      names(graph$hypotheses),
      width = max(nchar(names(graph$hypotheses)))
    ),
    ": (",
    formatC(graph$hypotheses, digits = 4, format = "f"),
    ")",
    sep = "",
    collapse = "\n"
  )

  paste(
    "--- Hypothesis weights ---",
    hypotheses_text,
    sep = "\n"
  )
}

print_transitions <- function(graph) {
  transitions <- formatC(
    graph$transitions,
    digits = 4,
    format = "f"
  )
  diag(transitions) <- rep("  --  ", ncol(transitions))

  transitions_text <- data.frame(transitions, check.names = FALSE)

  transitions_text
}

#' S3 print method for class 'initial_graph'
#'
#' A printed `initial_graph` displays a header stating what the object is, the
#' hypothesis weights, and the transition weights.
#'
#' @param x An object of class `initial_graph` to print
#'
#' @param ... Other values passed on to other methods (currently unused)
#'
#' @export
print.initial_graph <- function(x, ...) {
  if (length(x$hypotheses) == 0) {
    cat("An empty graph\n")
  } else {
    print_graph <- paste(
      print_title(),
      print_hypotheses(x),
      sep = "\n\n"
    )

    cat(print_graph, "", sep = "\n")

    cat("--- Transition weights ---\n")
    print(print_transitions(x))
  }
}
