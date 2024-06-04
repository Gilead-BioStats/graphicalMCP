#' S3 print method for the class `updated_graph`
#'
#' @description
#' A printed `updated_graph` displays the initial graph, the (final) updated
#' graph, and the sequence of intermediate updated graphs after hypotheses are
#' deleted (if available).
#'
#' @param x An object of the class `updated_graph` to print.
#' @param ... Other values passed on to other methods (currently unused).
#' @param precision An integer scalar indicating the number of decimal places
#'   to to display.
#' @param indent An integer scalar indicating how many spaces to indent results.
#'
#' @return NULL, after printing the updated graph.
#'
#' @seealso
#'   [print.initial_graph()] for the print method for the initial graph.
#'
#' @rdname print.updated_graph
#'
#' @export
#'
#' @references
#'   Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
#'   Rohmeyer, K. (2011a). Graphical approaches for multiple comparison
#'   procedures using weighted Bonferroni, Simes, or parametric tests.
#'   \emph{Biometrical Journal}, 53(6), 894-913.
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 1 in Bretz et al. (2011).
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- graph_create(hypotheses, transitions)
#'
#' # Delete the second and third hypotheses in the "unordered mode"
#' graph_update(g, delete = c(FALSE, TRUE, TRUE, FALSE))
#'
#' # Equivalent way in the "ordered mode" to obtain the updated graph after
#' # deleting the second and third hypotheses
#' # Additional intermediate updated graphs are also provided
#' graph_update(g, delete = 2:3)
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
