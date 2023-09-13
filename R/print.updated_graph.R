#' S3 print method for class `updated_graph`
#'
#' A printed `updated_graph` displays the initial graph, the vector of
#' keep/delete hypotheses, and the updated graph
#'
#' @param x An object of class `updated_graph` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the maximum number of decimals
#'   to include in numeric values
#' @param indent An integer scalar indicating how many spaces to indent results
#'
#' @export
print.updated_graph <- function(x, ..., precision = 6, indent = 2) {
  # Initial graph and updated graph
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

  na_output_graph <- x$updated_graph
  na_output_graph$hypotheses[x$deleted] <-
    na_output_graph$transitions[x$deleted, ] <-
    na_output_graph$transitions[, x$deleted] <-
    NA

  print(na_output_graph, title = title, ...)

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
        graph_seq_elt_na <- graph_seq[[i + 1]]
        graph_seq_elt_na$hypotheses[del_seq[seq_len(i)]] <-
          graph_seq_elt_na$transitions[del_seq[seq_len(i)], ] <-
          graph_seq_elt_na$transitions[, del_seq[seq_len(i)]] <-
          NA

        print(
          graph_seq_elt_na,
          precision = precision,
          indent = indent * (i + 1),
          title = paste0(
            "Step ", i, ": Updated graph after removing ",
            if (i == 1) "hypothesis " else "hypotheses ",
            paste0(del_seq[seq_len(i)], collapse = ", ")
          )
        )
      }
      cat("\n")
    }

    final_graph_na <- graph_seq[[length(graph_seq)]]
    final_graph_na$hypotheses[del_seq] <-
      final_graph_na$transitions[del_seq, ] <-
      final_graph_na$transitions[, del_seq] <-
      NA

    print(
      final_graph_na,
      precision = precision,
      indent = indent,
      title = "Final updated graph after removing deleted hypotheses"
    )
    cat("\n")
  }
}
