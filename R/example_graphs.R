#' Example graphs from a selection of papers
#'
#' @param n Number of vertices in the graph
#' @param hypotheses Hypothesis weights in a fallback procedure
#' @param names Optional names for the hypotheses (Must have length `n` or be
#'   NULL)
#'
#' @return An S3 object as returned by [create_graph()]
#'
#' @export
#'
#' @template references
#'
#' @rdname example-graphs
#'
#' @examples
#' bonferroni_holm(3, names = paste("dose", letters[1:3]))
bonferroni_holm <- function(n, names = NULL) {
  stopifnot(
    "n must be an integer" = is.numeric(n),
    "names must match size of desired graph" =
      (n == length(names) || is.null(names))
  )

  transitions <- matrix(rep(1 / (n - 1), n^2), nrow = n)
  diag(transitions) <- rep(0, n)

  hypotheses <- rep(1 / n, n)

  create_graph(hypotheses, transitions, names = names)
}

#' @export
#' @rdname example-graphs
huque_alosh_bhore_2011 <- function(names = NULL) {
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
    ),
    names = names
  )
}

#' @export
#' @rdname example-graphs
wiens_dmitrienko_2005 <- function(names = NULL) {
  create_graph(
    c(1 / 3, 1 / 3, 1 / 3),
    matrix(
      c(
        0, 1, 0,
        0, 0, 1,
        .5, .5, 0
      ),
      nrow = 3,
      byrow = TRUE
    ),
    names
  )
}

#' @export
#' @rdname example-graphs
fixed_sequence <- function(n = 3, names = NULL) {
  hypotheses <- c(1, rep(0, n - 1))

  transitions <- matrix(0, nrow = n, ncol = n)

  for (i in seq_len(n - 1)) transitions[i, i + 1] <- 1

  create_graph(hypotheses, transitions, names)
}

#' @export
#' @rdname example-graphs
fallback <- function(hypotheses = c(1, 0, 0), names = NULL) {
  r <- hypotheses[[2]] / (hypotheses[[1]] + hypotheses[[2]])

  transitions <- rbind(
    c(0, 1, 0),
    c(0, 0, 1),
    c(1 - r, r, 0)
  )

  create_graph(hypotheses, transitions, names)
}

#' @export
#' @rdname example-graphs
simple_successive_1 <- function(names = NULL) {
  hypotheses <- c(0.5, 0.5, 0, 0)
  transitions <- rbind(
    c(0, 0, 1, 0),
    c(0, 0, 0, 1),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )

  create_graph(hypotheses, transitions, names)
}

#' @export
#' @rdname example-graphs
simple_successive_2 <- function(names = NULL) {
  hypotheses <- c(0.5, 0.5, 0, 0)
  transitions <- rbind(
    c(0, .5, .5, 0),
    c(.5, 0, 0, .5),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )

  create_graph(hypotheses, transitions, names)
}

#' @export
#' @rdname example-graphs
random_graph <- function(n, names = NULL) {
  hypotheses <- sample(seq_len(n), replace = TRUE)
  hypotheses <- hypotheses / sum(hypotheses)

  transitions <- replicate(
    n,
    sample(seq_len(n), replace = TRUE),
    simplify = TRUE
  )
  diag(transitions) <- 0
  transitions <- transitions / rowSums(transitions)

  create_graph(hypotheses, transitions, names)
}

#' @export
#' @rdname example-graphs
complex_example <- function(names = NULL) {
  eps <- .0001

  weights <- c(rep(c(1 / 3, 0, 0), 3))
  transitions <- rbind(
    c(0, .5, .5, 0, 0, 0, 0, 0, 0),                 # 1 --> 2 & 3
    c(0, 0, 1, 0, 0, 0, 0, 0, 0),                   # 2 --> 3
    c(0, 1 - eps, 0, eps / 2, 0, 0, eps / 2, 0, 0), # 3 --> 2, 3 - - > 4 & 7
    c(0, 0, 0, 0, .5, .5, 0, 0, 0),                 # 4 --> 5 & 6
    c(0, 0, 0, 0, 0, 1, 0, 0, 0),                   # 5 --> 6
    c(eps / 2, 0, 0, 0, 1 - eps, 0, eps / 2, 0, 0), # 6 --> 5, 6 - - > 1 & 7
    c(0, 0, 0, 0, 0, 0, 0, .5, .5),                 # 7 --> 8 & 9
    c(0, 0, 0, 0, 0, 0, 0, 0, 1),                   # 8 --> 9
    c(eps / 2, 0, 0, eps / 2, 0, 0, 0, 1 - eps, 0)  # 9 --> 8, 9 - - > 1 & 4
  )

  create_graph(weights, transitions, names = names)
}
