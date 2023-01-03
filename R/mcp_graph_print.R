print_title <- function() {
  "An mcp_graph" # TODO: Explore {crayon} for using bold or colored text
}

print_hyps <- function(graph) {
  max_h_length <- nchar(length(graph$hypotheses))
  h_nums <- formatC(
    paste0("H", seq_along(graph$hypotheses)),
    width = max_h_length + 1
  )
  h_names <- names(graph$hypotheses)

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

#' @export
print.mcp_graph <- function(graph) {
  print_graph <- paste(
    print_title(),
    print_hyps(graph),
    print_hypotheses(graph),
    sep = "\n\n"
  )

  cat(print_graph, "", sep = "\n")

  cat("--- Transition weights ---\n")
  print(print_transitions(graph))
}
