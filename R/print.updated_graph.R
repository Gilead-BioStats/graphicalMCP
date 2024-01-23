#' S3 print method for the class `updated_graph`
#'
#' A printed `updated_graph` displays the initial graph, the updated graph, and
#' the sequence of graphs between the two (if it's available).
#'
#' @param x An object of class `updated_graph` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the number of significant
#'   figures to include in numeric values
#' @param indent An integer scalar indicating how many spaces to indent results
#'
#' @export
print.updated_graph <- function(x, ..., precision = 6, indent = 2) {
  # Initial graph and updated graph
  section_break("Initial and final graphs")
  cat("\n")

  print(x$initial_graph, ...)

  cat("\n")

  if (length(x$deleted) == 0) {
    title <- "Updated graph after deleting no hypotheses"
  } else if (length(x$deleted) == 1) {
    title <- paste("Updated graph after deleting hypothesis", x$deleted)
  } else {
    title <- paste(
      "Updated graph after deleting hypotheses",
      paste(x$deleted, collapse = ", ")
    )
  }

  attr(x$updated_graph, "title") <- title

  print(x$updated_graph, ...)

  # Graph sequence
  if (!is.null(x$intermediate_graphs)) {
    graph_seq <- x$intermediate_graphs
    del_seq <- x$deleted

    cat("\n")
    section_break("Deletion sequence ($intermediate_graphs)")
    cat("\n")
    for (i in seq_along(graph_seq) - 1) {
      if (i == 0) {
        print(graph_seq[[i + 1]], precision = precision, indent = indent)
      } else {
        attr(graph_seq[[i + 1]], "title") <- paste0(
          "Step ", i, ": Updated graph after removing ",
          if (i == 1) "hypothesis " else "hypotheses ",
          paste0(del_seq[seq_len(i)], collapse = ", ")
        )

        print(
          graph_seq[[i + 1]],
          precision = precision,
          indent = indent * (i + 1)
        )
      }
      cat("\n")
    }

    attr(graph_seq[[length(graph_seq)]], "title") <-
      "Final updated graph after removing deleted hypotheses"

    print(
      graph_seq[[length(graph_seq)]],
      precision = precision,
      indent = indent
    )
    cat("\n")
  }
}
