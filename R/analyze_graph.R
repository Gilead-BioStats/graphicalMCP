#' Check if all sub-graphs have weights summing to 1
#'
#' @param graph An `initial_graph` as created by `create_graph()`
#'
#' @return A list with elements `is_optimal` and `offending`. If `is_optimal` is
#'   TRUE, `offending` should be an empty matrix. Otherwise, it's a matrix of
#'   intersection hypotheses whose weights do not sum to 1
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
#' analyze_graph(g)$is_optimal
#' #> [1] TRUE
#'
#' g$transitions["H2", "H4"] <- .9
#'
#' analyze_graph(g)
#' #> $is_optimal
#' #> [1] FALSE
#' #>
#' #> $offending
#' #>    H1 H2 H3 H4   H1 H2   H3   H4
#' #> 3   0  0  1  1 0.00  0 0.50 0.45
#' #> 4   0  0  0  1 0.00  0 0.00 0.90
#' #> 5   0  0  1  0 0.00  0 0.95 0.00
#' #> 9   1  0  1  1 0.50  0 0.00 0.45
#' #> 10  1  0  0  1 0.50  0 0.00 0.45
#' #> 11  1  0  0  0 0.95  0 0.00 0.00
#' #> 12  1  0  1  0 0.95  0 0.00 0.00
analyze_graph <- function(graph) {
  subgraphs <- generate_weights(graph)

  sub_weights <- subgraphs[, (ncol(subgraphs) / 2 + 1):ncol(subgraphs)]

  wgt_sums <- rowSums(sub_weights)

  is_optimal <- isTRUE(
    all.equal(
      wgt_sums,
      rep(1, length(wgt_sums)),
      check.attributes = FALSE
    )
  )

  list(
    is_optimal = is_optimal,
    offending = subgraphs[wgt_sums != 1, ]
  )
}
