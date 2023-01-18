#' Check if all subgraphs have weights summing to 1
#'
#' @param graph An `initial_graph` as created by `create_graph()`
#'
#' @return TRUE if the graph is optimal, and FALSE otherwise
#' @export
#'
#' @examples
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' names <- c("H1", "H2", "H3", "H4")
#' g <- create_graph(hypotheses, transitions, names)
#'
#' analyze_graph(g)
#' #> graph is optimal
#'
#' g$transitions["H2", "H4"] <- .9
#'
#' analyze_graph(g)
#' #> graph is sub-optimal:
#' #>
#' #>    H1 H2 H3 H4   H1 H2   H3   H4
#' #> 3   0  0  1  1 0.00  0 0.50 0.45
#' #> 4   0  0  0  1 0.00  0 0.00 0.90
#' #> 5   0  0  1  0 0.00  0 0.95 0.00
#' #> 9   1  0  1  1 0.50  0 0.00 0.45
#' #> 10  1  0  0  1 0.50  0 0.00 0.45
#' #> 11  1  0  0  0 0.95  0 0.00 0.00
#' #> 12  1  0  1  0 0.95  0 0.00 0.00
analyze_graph <- function(graph) {
  subgraphs <- generate_weights_recursive(graph)

  sub_weights <- subgraphs[, (ncol(subgraphs) / 2 + 1):ncol(subgraphs)]

  weight_sums <- rowSums(sub_weights)

  if (all(weight_sums == 1)) {
    cat("graph is optimal\n")

    return(TRUE)
  } else {
    cat("graph is sub-optimal:\n\n")

    print(subgraphs[weight_sums != 1, ])

    return(FALSE)
  }
}
