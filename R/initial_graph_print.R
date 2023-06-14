print_hypotheses <- function(graph) {
  hypotheses_text <- paste(
    formatC(
      names(graph$hypotheses),
      width = max(nchar(names(graph$hypotheses)))
    ),
    ": ",
    formatC(graph$hypotheses, digits = 4, format = "f"),
    sep = "",
    collapse = "\n"
  )

  hypotheses_text
}

print_transitions <- function(graph) {
  transitions <- formatC(
    graph$transitions,
    digits = 4,
    format = "f"
  )

  transitions_text <- data.frame(transitions, check.names = FALSE)

  print(transitions_text)
}

#' S3 print method for the class `initial_graph`
#'
#' A printed `initial_graph` displays a header stating what the object is, the
#' hypothesis weights, and the transition weights.
#'
#' @param x An object of class `initial_graph` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @export
print.initial_graph <- function(x, ...) {
  cat("Initial graph\n\n")

  cat("--- Hypothesis weights ---\n")

  cat(print_hypotheses(x), "", sep = "\n")

  cat("--- Transition weights ---\n")

  print_transitions(x)
}
