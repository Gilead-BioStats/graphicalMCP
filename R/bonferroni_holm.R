#' Example graphs from a variety of papers
#'
#' @param n Number of vertices in the Bonferroni-Holm graph
#' @param names Optional names for the hypotheses (Must have length `n`)
#'
#' @return An S3 object of class `initial_graph`, following the structure of
#'   Bonferroni & Holm: A complete graph with equal weight on each hypothesis
#'   and equal weight on each transition
#' @export
#'
#' @examples
#' bonferroni_holm(3, names = paste("dose", letters[1:3]))
bonferroni_holm <- function(n, names = NULL) {
  stopifnot(
    "n must be an integer" = is.numeric(n),
    "names must match size of desired graph" =
      (n == length(names) || is.null(names))
  )

  g <- matrix(rep(1 / (n - 1), n ^ 2), nrow = n)
  diag(g) <- rep(0, n)

  w <- rep(1 / n, n)

  create_graph(w, g, names = names)
}
