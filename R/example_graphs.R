#' Example graphs from a selection of papers
#'
#' @param num_hyps Number of vertices in the graph
#' @param hypotheses Hypothesis weights in a fallback procedure
#' @param hyp_names Optional names for the hypotheses (Must have length `num_hyps` or be
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
#' bonferroni_holm(3, hyp_names = paste("dose", letters[1:3]))
bonferroni_holm <- function(num_hyps, hyp_names = NULL) {
  stopifnot(
    "num_hyps must be an integer" = is.numeric(num_hyps),
    "names must match size of desired graph" =
      (num_hyps == length(hyp_names) || is.null(hyp_names))
  )

  transitions <- matrix(rep(1 / (num_hyps - 1), num_hyps^2), nrow = num_hyps)
  diag(transitions) <- rep(0, num_hyps)

  hypotheses <- rep(1 / num_hyps, num_hyps)

  create_graph(hypotheses, transitions, hyp_names = hyp_names)
}

#' @export
#' @rdname example-graphs
huque_alosh_bhore_2011 <- function(hyp_names = NULL) {
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
    hyp_names = hyp_names
  )
}

#' @export
#' @rdname example-graphs
wiens_dmitrienko_2005 <- function(hyp_names = NULL) {
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
    hyp_names
  )
}

#' @export
#' @rdname example-graphs
fixed_sequence <- function(num_hyps = 3, hyp_names = NULL) {
  hypotheses <- c(1, rep(0, num_hyps - 1))

  transitions <- matrix(0, nrow = num_hyps, ncol = num_hyps)

  for (i in seq_len(num_hyps - 1)) transitions[i, i + 1] <- 1

  create_graph(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example-graphs
fallback <- function(hypotheses = c(1, 0, 0), hyp_names = NULL) {
  r <- hypotheses[[2]] / (hypotheses[[1]] + hypotheses[[2]])

  transitions <- rbind(
    c(0, 1, 0),
    c(0, 0, 1),
    c(1 - r, r, 0)
  )

  create_graph(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example-graphs
simple_successive_1 <- function(hyp_names = NULL) {
  hypotheses <- c(0.5, 0.5, 0, 0)
  transitions <- rbind(
    c(0, 0, 1, 0),
    c(0, 0, 0, 1),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )

  create_graph(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example-graphs
simple_successive_2 <- function(hyp_names = NULL) {
  hypotheses <- c(0.5, 0.5, 0, 0)
  transitions <- rbind(
    c(0, .5, .5, 0),
    c(.5, 0, 0, .5),
    c(0, 1, 0, 0),
    c(1, 0, 0, 0)
  )

  create_graph(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example-graphs
random_graph <- function(num_hyps, hyp_names = NULL) {
  hypotheses <- sample(seq_len(num_hyps), replace = TRUE)
  hypotheses <- hypotheses / sum(hypotheses)

  transitions <- replicate(
    num_hyps,
    sample(seq_len(num_hyps), replace = TRUE),
    simplify = TRUE
  )
  diag(transitions) <- 0
  transitions <- transitions / rowSums(transitions)

  create_graph(hypotheses, transitions, hyp_names)
}

#' @export
#' @rdname example-graphs
complex_example_1 <- function(hyp_names = NULL) {
  eps <- .0001

  weights <- c(rep(c(1 / 2, 0, 0), 2))
  transitions <- rbind(
    c(0, .5, .5, 0, 0, 0),
    c(0, 0, 1, 0, 0, 0),
    c(0, 1 - eps, 0, eps, 0, 0),
    c(0, 0, 0, 0, .5, .5),
    c(0, 0, 0, 0, 0, 1),
    c(eps, 0, 0, 0, 1 - eps, 0)
  )

  create_graph(weights, transitions, hyp_names = hyp_names)
}

#' @export
#' @rdname example-graphs
complex_example_2 <- function(hyp_names = NULL) {
  eps <- .0001

  weights <- c(rep(c(1 / 3, 0, 0), 3))
  transitions <- rbind(
    c(0, .5, .5, 0, 0, 0, 0, 0, 0), # 1 --> 2 & 3
    c(0, 0, 1, 0, 0, 0, 0, 0, 0), # 2 --> 3
    c(0, 1 - eps, 0, eps / 2, 0, 0, eps / 2, 0, 0), # 3 --> 2, 3 - - > 4 & 7
    c(0, 0, 0, 0, .5, .5, 0, 0, 0), # 4 --> 5 & 6
    c(0, 0, 0, 0, 0, 1, 0, 0, 0), # 5 --> 6
    c(eps / 2, 0, 0, 0, 1 - eps, 0, eps / 2, 0, 0), # 6 --> 5, 6 - - > 1 & 7
    c(0, 0, 0, 0, 0, 0, 0, .5, .5), # 7 --> 8 & 9
    c(0, 0, 0, 0, 0, 0, 0, 0, 1), # 8 --> 9
    c(eps / 2, 0, 0, eps / 2, 0, 0, 0, 1 - eps, 0) # 9 --> 8, 9 - - > 1 & 4
  )

  create_graph(weights, transitions, hyp_names = hyp_names)
}
