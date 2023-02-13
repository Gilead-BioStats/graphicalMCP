#' Example graphs from a variety of papers
#'
#' @param n Number of vertices in the Bonferroni-Holm graph
#' @param names Optional names for the hypotheses (Must have length `n`)
#'
#' @return An S3 object of class `mcp_graph`, following the structure of
#'   Bonferroni & Holm: A complete graph with equal weight on each hypothesis
#'   and equal weight on each transition
#' @export
#'
#' @family example-graphs
#'
#' @examples
#' bonferroni_holm(3, names = paste("dose", letters[1:3]))
bonferroni_holm <- function(n, names = NULL) {
  stopifnot(
    "n must be an integer" = is.numeric(n),
    "names must match size of desired graph" =
      (n == length(names) || is.null(names))
  )

  g <- matrix(rep(1 / (n - 1), n^2), nrow = n)
  diag(g) <- rep(0, n)

  w <- rep(1 / n, n)

  create_graph(w, g, names = names)
}

huque_alosh_bhore_2011 <- function() {
  create_graph(
    c(1, 0, 0, 0),
    matrix(
      c(
        0, .5, .5,  0,
        0,  0,  0,  1,
        0, .5,  0, .5,
        0,  1,  0,  0
      ),
      nrow = 4,
      byrow = TRUE
    )
  )
}

wiens_dmitrienko_2005 <- function(names = NULL) {
  create_graph(
    c(1/3, 1/3, 1/3),
    matrix(
      c(
         0,  1,  0,
         0,  0,  1,
        .5, .5,  0
      ),
      nrow = 3,
      byrow = TRUE
    ),
    names
  )
}

fixed_sequence <- function(n = 3, names = NULL) {
  w <- c(1, rep(0, n - 1))

  g <- matrix(0, nrow = n, ncol = n)

  for (i in seq_len(n - 1)) g[i, i + 1] <- 1

  create_graph(w, g, names)
}

fallback <- function(w = c(1, 0, 0), names = NULL) {
  r <- w[[2]] / (w[[1]] + w[[2]])

  g <- rbind(
    c(0,     1, 0),
    c(0,     0, 1),
    c(1 - r, r, 0)
  )

  create_graph(w, g, names)
}
