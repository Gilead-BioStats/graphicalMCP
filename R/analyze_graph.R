#' Check if a graph is optimal
#'
#' @param graph An `mcp_graph` object as created by `graph()`
#'
#' @return A Boolean result specifying whether the graph is optimal or not
#' @export
#'
#' @examples
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0))
#' g <- graph(hypotheses, transitions)
#'
#' is_optimal(g)
#' # TRUE
#'
#' g$hypotheses[[1]] <- 0.4
#' is_optimal(g)
#' # FALSE
is_optimal <- function(graph) {
  if (sum(graph$hypotheses) != 1) return(FALSE)
  if (any(rowSums(graph$transitions) != 1)) return(FALSE)
  return(TRUE)
}

analyze_graph <- function(graph) {
  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  # Check if optimal ---------------------------------------------------------
  if (sum(hypotheses) != 1 || any(rowSums(transitions) != 1)) {
    optimal <- FALSE
  } else {
    optimal <- TRUE
  }
}
