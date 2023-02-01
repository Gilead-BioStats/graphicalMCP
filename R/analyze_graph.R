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
  # Check sums of all intersection hypothesis weights --------------------------
  subgraphs <- generate_weights_recursive(graph)
  sub_weights <- subgraphs[, (ncol(subgraphs) / 2 + 1):ncol(subgraphs)]
  wgt_sums <- rowSums(sub_weights)
  sub_wgts_optimal <- all(sapply(wgt_sums, function(x) isTRUE(all.equal(x, 1))))

  # Check connectivity ---------------------------------------------------------
  # Shortest path algorithm adapted from
  #   https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm
  dist <- graph$transitions
  hyp_nums <- seq_len(nrow(dist))
  dist[dist == 0] <- Inf # Initialize shortest path as Inf...
  diag(dist) <- 0 # ...except the diagonal, which is always 0

  for (mid in hyp_nums) {
    for (start in hyp_nums) {
      for (end in hyp_nums) {
        if (dist[start, end] > dist[start, mid] + dist[mid, end]) {
          dist[start, end] <- dist[start, mid] + dist[mid, end]
        }
      }
    }
  }

  connect_optimal <- !any(is.infinite(dist))

  # Combine optimal checks for overall result ----------------------------------
  is_optimal <- sub_wgts_optimal && connect_optimal

  reason <- c(
    NULL,
    if (!sub_wgts_optimal) "subgraphs",
    if (!connect_optimal) "connectivity"
  )

  names <- expand.grid(rows = rownames(dist), cols = colnames(dist))

  offending <- list(
    subgraphs = subgraphs[wgt_sums != 1, ],
    connectivity = names[is.infinite(dist), ]
  )

  list(
    is_optimal = is_optimal,
    reason = reason,
    offending = offending
  )
}
