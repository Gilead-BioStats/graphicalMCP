#' S3 print method for class 'updated_graph'
#'
#' A printed `updated_graph` displays the original graph, the vector of
#' keep/delete hypotheses, and the updated graph
#'
#' @param x An object of class `updated_graph` to print
#'
#' @param ... Other values passed on to other methods (currently unused)
#'
#' @export
print.updated_graph <- function(x, ...) {
  print(x$initial_graph)

  cat("\n--------------------------------------\n\n")

  print(as.data.frame(rbind(x$kept_hypotheses), row.names = ""))

  cat("\n--------------------------------------\n\n")

  print(x$updated_graph)
}
