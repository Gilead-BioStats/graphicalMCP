print_title <- function() {
  "An mcp_graph" # TODO: Explore {crayon} for using bold or colored text
}

print_hyps <- function(graph) {
  max_h_length <- nchar(length(graph$hyp_wgts))
  h_nums <- formatC(
    paste0("H", seq_along(graph$hyp_wgts)),
    width = max_h_length + 1
  )
  h_names <- names(graph$hyp_wgts)

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

print_hyp_weights <- function(graph) {
  hyp_weights_text <- paste(
    formatC(
      names(graph$hyp_wgts),
      width = max(nchar(names(graph$hyp_wgts)))
    ),
    ": (",
    formatC(graph$hyp_wgts, digits = 4, format = "f"),
    ")",
    sep = "",
    collapse = "\n"
  )

  paste(
    "--- Hypothesis weights ---",
    hyp_weights_text,
    sep = "\n"
  )
}

print_trns_weights <- function(graph) {
  trns_weights <- formatC(
    graph$trns_wgts,
    digits = 4,
    format = "f"
  )
  diag(trns_weights) <- rep("  --  ", ncol(trns_weights))

  trns_weights_text <- data.frame(trns_weights, check.names = FALSE)

  trns_weights_text
}

#' @export
print.mcp_graph <- function(graph) {
  print_graph <- paste(
    print_title(),
    print_hyps(graph),
    print_hyp_weights(graph),
    sep = "\n\n"
  )

  cat(print_graph, "", sep = "\n")

  cat("--- Transition weights ---\n")
  print(print_trns_weights(graph))
}
