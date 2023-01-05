#' `mcp_graph` object
#'
#' Creates a list that represents a graphical multiple comparison procedure
#'
#' @param hypotheses A numeric vector of hypothesis weights in the graphical
#'   multiple comparison procedure. Must be a vector of values between 0 & 1
#'   (inclusive). The length should match the row and column lengths of
#'   `transitions`. The sum of hypothesis weights cannot exceed 1
#' @param transitions A numeric matrix of transition weights between hypotheses
#'   in the graphical multiple comparison procedure. Must be a square matrix of
#'   values between 0 & 1 (inclusive). The row and column lengths should match
#'   the length of `hypotheses`. Each row (Transition weights leaving a
#'   hypothesis) can sum to no more than 1. The diagonal (Transition weights
#'   from a hypothesis to itself) must be all 0s
#' @param names (Optional) A character vector of hypothesis names. If not
#'   provided, names from `hypotheses` and `transitions` will be used. If names
#'   are not specified, hypotheses will be named sequentially as H1, H2, ...
#' @return An S3 object of class `mcp_graph`
#' @export
#' @examples
#' # A graphical multiple comparison procedure with two primary hypotheses (H1
#' # and H2) and two secondary hypotheses (H3 and H4)
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0))
#' names <- c("H1", "H2", "H3", "H4")
#' g <- graph(hypotheses, transitions, names)
#' g
#'
#' # Explicit names override names in `hypotheses` (with a warning)
#' hypotheses <- c(h1 = 0.5, h2 = 0.5, h3 = 0, h4 = 0)
#' transitions <- rbind(c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0))
#' names <- c("H1", "H2", "H3", "H4")
#' g <- graph(hypotheses, transitions, names)
#' g
#'
#' # Explicit names override names in `transitions` (with a warning)
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(h1 = c(0, 0, 1, 0),
#'   h2 = c(0, 0, 0, 1),
#'   h3 = c(0, 1, 0, 0),
#'   h4 = c(1, 0, 0, 0))
#' names <- c("H1", "H2", "H3", "H4")
#' g <- graph(hypotheses, transitions, names)
#' g
#'
#' # Use names in `hypotheses`
#' hypotheses <- c(H1 = 0.5, H2 = 0.5, H3 = 0, H4 = 0)
#' transitions <- rbind(c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0))
#' g <- graph(hypotheses, transitions)
#' g
#'
#' # Use names in `transitions`
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(H1 = c(0, 0, 1, 0),
#'   H2 = c(0, 0, 0, 1),
#'   H3 = c(0, 1, 0, 0),
#'   H4 = c(1, 0, 0, 0))
#' g <- graph(hypotheses, transitions)
#' g
#'
#' # When names are not specified, hypotheses are numbered sequentially as
#' # H1, H2, ...
#' hypotheses <- c(0.5, 0.5, 0, 0)
#' transitions <- rbind(c(0, 0, 1, 0),
#'   c(0, 0, 0, 1),
#'   c(0, 1, 0, 0),
#'   c(1, 0, 0, 0))
#' g <- graph(hypotheses, transitions)
#' g
graph <- function(hypotheses, transitions, names = NULL) { # TODO: Make it an option to provide just a vector and coerce to the appropriate square matrix?

  stopifnot(
    "hypothesis weights must be numeric" = is.numeric(hypotheses),
    "transition weights must be numeric" = is.numeric(transitions)
  )

  # Name validation ----------------------------------------------------------
  names_provided <- !is.null(names)
  hyps_named <- !is.null(names(hypotheses))
  trns_col_named <- !is.null(colnames(transitions))
  trns_row_named <- !is.null(rownames(transitions))

  if (names_provided) {
    if (hyps_named || trns_col_named || trns_row_named) {
      warning("hypothesis names specified - overriding names in
                    'hypotheses' and 'transitions'")
    }
  } else {
    if (hyps_named && !trns_col_named && !trns_row_named) {
      names <- names(hypotheses)
    } else if (!hyps_named && trns_col_named && !trns_row_named) {
      names <- colnames(transitions)
    } else if (!hyps_named && !trns_col_named && trns_row_named) {
      names <- rownames(transitions)
    } else {
      if (
        any(
          names(hypotheses) != colnames(transitions),
          names(hypotheses) != rownames(transitions),
          colnames(transitions) != rownames(transitions)
        )
      ) {
        stop("names provided in 'hypotheses' and 'transitions' should
                     match")
      }
      names <- paste0("H", 1:length(hypotheses))
    }
  }

  # Values validation --------------------------------------------------------
  if (
    any(
      nrow(transitions) != ncol(transitions),
      nrow(transitions) != length(hypotheses),
      ncol(transitions) != length(hypotheses)
    )
  ) {
    stop("length of 'hypotheses', rows of 'transitions', and columns of
             'transitions' must all match")
  }

  colnames(transitions) <- names
  rownames(transitions) <- names
  names(hypotheses) <- names

  if (any(hypotheses < 0 | hypotheses > 1)) {
    stop("hypothesis weights must be between 0 and 1")
  }

  if (sum(hypotheses) > 1) {
    stop("hypothesis weights must sum to no more than 1")
  }

  if (any(transitions < 0 | transitions > 1)) {
    stop("transition weights must be between 0 and 1")
  }

  if (any(diag(transitions) != 0)) {
    stop("diagonal of transition weights must be all 0s")
  }

  if (any(rowSums(transitions) > 1)) {
    stop("transition weights from each row must sum to no more than 1")
  }

  # Create graph object
  new_mcp_graph <- list(hypotheses = hypotheses, transitions = transitions)
  class(new_mcp_graph) <- "mcp_graph"

  new_mcp_graph
}
