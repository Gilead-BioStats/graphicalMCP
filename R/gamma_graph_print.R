#' @export
print.gamma_graph <- function(x, ...) {
  cat("A graph with variable edges\n")
  base_graph <- attr(x, "base_graph")
  graph_names <- names(base_graph$hypotheses)
  set_edges <- paste0(
    "Set variable edges: `<your_gamma_graph_name>(c(<",
    paste0("gamma_", graph_names, collapse = ">, <"),
    ">))`"
  )

  cat(set_edges, "\n\n")

  cat("Base graph:\n")
  print(base_graph)
  cat("\n")

  cat("Gamma rowwise proportions:\n")
  print(attr(x, "gamma_props"))
}
