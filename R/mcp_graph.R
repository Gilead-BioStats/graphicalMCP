#' `mcp_graph` object
#'
#' Creates a list that represents a multiple comparison procedure graph
#'
#' @param hypotheses A numeric vector of hypothesis weights in the multiple
#'   comparison procedure graph. Must be a vector of values between 0 & 1
#'   (inclusive) with length matching the row and column lengths of
#'   `transitions`. The sum of hypothesis weights cannot exceed 1
#' @param transitions A numeric matrix of transition weights between hypotheses
#'   in the multiple comparison procedure graph. Must be a square matrix of
#'   values between 0 & 1 (inclusive) with row and column lengths matching the
#'   length of `hypotheses`. Each row (Transition weights leaving a hypothesis)
#'   can sum to no more than 1, and the diagonal (Transition weights from a
#'   hypothesis to itself) must be all 0s
#' @param names (Optional) A character vector of hypothesis names. If not
#'   provided, names from the weights arguments will be used. If the weights
#'   arguments are unnamed, hypotheses will be named sequentially
#' @param graph An object to test
#' @return An S3 object of class `mcp_graph`
#' @export
#' @examples
#' graph(c(.5, .5), rbind(c(0, 1), c(1, 0)), c("doseA", "doseB"))
#'
#' # Explicit names override names in the weights arguments (with a warning)
#' graph(c(d1 = .5, d2 = .5), rbind(c(0, 1), c(1, 0)), c("doseA", "doseB"))
#'
#' # When names are not specified, hypotheses are numbered
#' graph(
#'   c(.333333, .333333, .333333),
#'   matrix(c(0, .5, .5, .5, 0, .5, .5, .5, 0), nrow = 3, byrow = TRUE),
#' )
graph <- function(hypotheses, transitions, names = NULL) { # TODO: Make it an option to provide just a vector and coerce to the appropriate square matrix?
	stopifnot(
		"transition weights must be numeric" = is.numeric(transitions),
		"hypothesis weights must be numeric" = is.numeric(hypotheses)
	)

	# Name validation ------------------------------------------------------------
	names_provided <- !is.null(names)
  hyps_named <- !is.null(names(hypotheses))
  trns_col_named <- !is.null(colnames(transitions))
  trns_row_named <- !is.null(rownames(transitions))

  if (names_provided) {
    if (hyps_named || trns_col_named || trns_row_named) {
      warning("hypothesis names specified - overriding hypothesis and transition weight names")
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
        stop("names provided in w & g must match")
      }
      names <- paste0("H", 1:length(hypotheses))
    }
  }

  colnames(transitions) <- names
  rownames(transitions) <- names
  names(hypotheses) <- names

  # Values validation ----------------------------------------------------------
  if (
  	any(
  		nrow(transitions) != ncol(transitions),
  		nrow(transitions) != length(hypotheses),
  		ncol(transitions) != length(hypotheses)
  	)
  ) {
  	stop("length of hypothesis weights, rows of transition weights, and columns of transition weights must all match")
  }

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
  	stop("transition weights from each hypothesis must sum to no more than 1")
  }

  # Create graph object
  new_mcp_graph <- list(transitions = transitions, hypotheses = hypotheses)
  class(new_mcp_graph) <- "mcp_graph"

  new_mcp_graph
}
