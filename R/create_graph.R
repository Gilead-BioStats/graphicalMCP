#' Define a graph representing a multiple comparison procedure
#'
#' A multiple comparison procedure graph can be represented by 1) a vector of
#' initial hypothesis weights, and 2) a matrix of initial transition weights.
#' This function also does validation to make sure that the vector and matrix
#' combine to form a valid MCP graph.
#'
#' @param hypotheses A numeric vector of hypothesis weights in an initial
#'   graphical multiple comparison procedure. Must be a vector of values between
#'   0 & 1 (inclusive). The length should match the row and column lengths of
#'   `transitions`. The sum of hypothesis weights cannot exceed 1
#' @param transitions A numeric matrix of transition weights between hypotheses
#'   in an initial graphical multiple comparison procedure. Must be a square
#'   matrix of values between 0 & 1 (inclusive). The row and column lengths
#'   should match the length of `hypotheses`. Each row (Transition weights
#'   leaving a hypothesis) can sum to no more than 1. The diagonal (Transition
#'   weights from a hypothesis to itself) must be all 0s
#' @param hyp_names (Optional) A character vector of hypothesis names. If not
#'   provided, names from `hypotheses` and `transitions` will be used. If names
#'   are not specified, hypotheses will be named sequentially as H1, H2, ...
#'
#' @return An S3 object of class `initial_graph`. The underlying structure is a
#'   list with elements `hypotheses` and `transitions`
#'
#' @export
#'
#' @template references
#'
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' # See Figure 1 in Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer,
#' # W., & Rohmeyer, K. (2011). Graphical approaches for multiple comparison
#' # procedures using weighted Bonferroni, Simes, or parametric tests.
#' # Biometrical Journal, 53(6), 894-913.
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' hyp_names <- c("H1", "H2", "H3", "H4")
#' g <- create_graph(hypotheses, transitions, hyp_names)
#' g
#'
#' # Explicit names override names in `hypotheses` (with a warning)
#' hypotheses <- c(h1 = 0.5, h2 = 0.5, h3 = 0, h4 = 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#'
#' g <- create_graph(hypotheses, transitions, hyp_names)
#' g
#'
#' # Explicit names override names in `transitions` (with a warning)
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   h1 = c(0, 0, 1, 0),
#'   h2 = c(0, 0, 0, 1),
#'   h3 = c(0, 1, 0, 0),
#'   h4 = c(1, 0, 0, 0)
#' )
#'
#' g <- create_graph(hypotheses, transitions, hyp_names)
#' g
#'
#' # Use names in `hypotheses`
#' hypotheses <- c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- create_graph(hypotheses, transitions)
#' g
#'
#' # Use names in `transitions`
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   H1 = c(0, 0, 1, 0),
#'   H2 = c(0, 0, 0, 1),
#'   H3 = c(0, 1, 0, 0),
#'   H4 = c(1, 0, 0, 0)
#' )
#' g <- create_graph(hypotheses, transitions)
#' g
#'
#' # When names are not specified, hypotheses are numbered sequentially as
#' # H1, H2, ...
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(
#'   c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0)
#' )
#' g <- create_graph(hypotheses, transitions)
#' g
create_graph <- function(hypotheses, transitions, hyp_names = NULL) {
  # Basic input validation -----------------------------------------------------
  stopifnot(
    "Hypothesis weights must be numeric" = is.numeric(hypotheses),
    "Transition weights must be numeric" = is.numeric(transitions)
  )

  if (
    any(
      nrow(transitions) != ncol(transitions),
      nrow(transitions) != length(hypotheses),
      ncol(transitions) != length(hypotheses)
    )
  ) {
    stop("Length of 'hypotheses', rows of 'transitions', and columns of
             'transitions' must all match")
  }

  # Validation of names of hypotheses ----------------------------------------
  explicit_names <- !is.null(hyp_names)

  implicit_names <- any(
    !is.null(names(hypotheses)),
    !is.null(colnames(transitions)),
    !is.null(rownames(transitions))
  )

  names_diff <- any(
    names(hypotheses) != colnames(transitions),
    names(hypotheses) != rownames(transitions),
    colnames(transitions) != rownames(transitions)
  )

  if (implicit_names && explicit_names) {
    warning("Hypothesis names specified - overriding names in
                    `hypotheses` and `transitions`")
  } else if (implicit_names && names_diff) {
    stop("Names provided in `hypotheses` and `transitions` should match")
  } else if (implicit_names) {
    hyp_names <- unique(
      c(names(hypotheses), colnames(transitions), rownames(transitions))
    )
  } else if (!explicit_names) {
    hyp_names <- paste0("H", seq_along(hypotheses))
  }

  names(hypotheses) <-
    colnames(transitions) <- rownames(transitions) <- hyp_names

  # Validation of numerical conditions for a valid graphical multiple
  # comparison procedure -----------------------------------------------------
  if (any(hypotheses < 0 | hypotheses > 1)) {
    offending <- hypotheses[hypotheses < 0 | hypotheses > 1]
    not_zero_float <- sapply(offending, function(x) !isTRUE(all.equal(0, x)))
    not_one_float <- sapply(offending, function(x) !isTRUE(all.equal(1, x)))

    if (any(not_zero_float & not_one_float)) {
      stop("Hypothesis weights must be between 0 and 1")
    }
  }

  if (sum(hypotheses) > 1 && !isTRUE(all.equal(sum(hypotheses), 1))) {
    stop("Hypothesis weights must sum to no more than 1")
  }

  if (any(transitions < 0 | transitions > 1)) {
    offending <- transitions[transitions < 0 | transitions > 1]
    not_zero_float <- sapply(offending, function(x) !isTRUE(all.equal(0, x)))
    not_one_float <- sapply(offending, function(x) !isTRUE(all.equal(1, x)))

    if (any(not_zero_float & not_one_float)) {
      stop("Transition weights must be between 0 and 1")
    }
  }

  not_zero_float <- sapply(
    diag(transitions),
    function(x) !isTRUE(all.equal(0, x))
  )

  if (any(not_zero_float)) {
    stop("Diagonal of transition weights must be all 0s")
  }

  if (any(rowSums(transitions) > 1)) {
    not_one_float <- sapply(
      rowSums(transitions[rowSums(transitions) > 1, , drop = FALSE]),
      function(x) !isTRUE(all.equal(1, x))
    )

    if (any(not_one_float)) {
      stop("Transition weights from each row must sum to no more than 1")
    }
  }

  # Create an initial graph object ---------------------------------------------
  new_graph <- structure(
    list(hypotheses = hypotheses, transitions = transitions),
    class = "initial_graph"
  )
  new_graph
}
