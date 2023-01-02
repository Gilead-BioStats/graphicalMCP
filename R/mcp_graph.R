new_mcp_graph <- function(trns_wgts, hyp_wgts) {
  stopifnot(
    "transition weights must be numeric" = is.numeric(trns_wgts),
    "hypothesis weights must be numeric" = is.numeric(hyp_wgts)
  )

  new_mcp_graph <- list(trns_wgts = trns_wgts, hyp_wgts = hyp_wgts)
  class(new_mcp_graph) <- "mcp_graph"

  new_mcp_graph
}

validate_mcp_graph <- function(graph) {
  trns_wgts_row <- nrow(graph$trns_wgts)
  trns_wgts_col <- ncol(graph$trns_wgts)
  hyp_wgts_len <- length(graph$hyp_wgts)

  if (
    any(
      trns_wgts_row != trns_wgts_col,
      trns_wgts_row != hyp_wgts_len,
      trns_wgts_col != hyp_wgts_len
    )
  ) {
    stop("length of hypothesis weights, rows of transition weights, and columns of transition weights must all match")
  }

  if (any(graph$hyp_wgts < 0 | graph$hyp_wgts > 1)) {
    stop("hypothesis weights must be between 0 and 1")
  }

  if (sum(graph$hyp_wgts) > 1) {
    stop("hypothesis weights must sum to no more than 1")
  }

  if (any(graph$trns_wgts < 0 | graph$trns_wgts > 1)) {
    stop("transition weights must be between 0 and 1")
  }

  if (any(diag(graph$trns_wgts) != 0)) {
    stop("diagonal of transition weights must be all 0s")
  }

  if (any(rowSums(graph$trns_wgts) > 1)) {
    stop("transition weights from each hypothesis must sum to no more than 1")
  }

  graph
}

#' `mcp_graph` object
#'
#' Creates a list that represents a multiple comparison procedure graph
#'
#' @param trns_wgts A numeric matrix of transition weights between hypotheses in
#'   the multiple comparison procedure graph. Must be a square matrix of values
#'   between 0 & 1 (inclusive) with row and column lengths matching the length
#'   of `hyp_wgts`. Each row (Transition weights leaving a hypothesis) can sum
#'   to no more than 1, and the diagonal (Transition weights from a hypothesis
#'   to itself) must be all 0s
#' @param hyp_wgts A numeric vector of hypothesis weights in the multiple
#'   comparison procedure graph. Must be a vector of values between 0 & 1
#'   (inclusive) with length matching the row and column lengths of `trns_wgts`.
#'   The sum of hypothesis weights cannot exceed 1
#' @param names (Optional) A character vector of hypothesis names. If not
#'   provided, names from the weights arguments will be used. If the weights
#'   arguments are unnamed, hypotheses will be named sequentially
#' @param graph An object to test
#' @return An S3 object of class `mcp_graph`
#' @export
#' @examples
#' mcp_graph(rbind(c(0, 1), c(1, 0)), c(.5, .5), c("doseA", "doseB"))
#'
#' # Explicit names override names in the weights arguments (with a warning)
#' mcp_graph(rbind(c(0, 1), c(1, 0)), c(d1 = .5, d2 = .5), c("doseA", "doseB"))
#'
#' # When names are not specified, hypotheses are numbered
#' mcp_graph(
#'   matrix(c(0, .5, .5, .5, 0, .5, .5, .5, 0), nrow = 3, byrow = TRUE),
#'   c(.333333, .333333, .333333)
#' )
mcp_graph <- function(trns_wgts, hyp_wgts, names = NULL) { # TODO: Make it an option to provide just a vector and coerce to the appropriate square matrix?
  names_provided <- !is.null(names)
  hyps_named <- !is.null(names(hyp_wgts))
  trns_col_named <- !is.null(colnames(trns_wgts))
  trns_row_named <- !is.null(rownames(trns_wgts))

  if (names_provided) {
    if (hyps_named || trns_col_named || trns_row_named) {
      warning("hypothesis names specified - overriding hypothesis and transition weight names")
    }
  } else {
    if (hyps_named && !trns_col_named && !trns_row_named) {
      names <- names(hyp_wgts)
    } else if (!hyps_named && trns_col_named && !trns_row_named) {
      names <- colnames(trns_wgts)
    } else if (!hyps_named && !trns_col_named && trns_row_named) {
      names <- rownames(trns_wgts)
    } else {
      if (
        any(
          names(hyp_wgts) != colnames(trns_wgts),
          names(hyp_wgts) != rownames(trns_wgts),
          colnames(trns_wgts) != rownames(trns_wgts)
        )
      ) {
        stop("names provided in w & g must match")
      }
      names <- paste0("H", 1:length(hyp_wgts))
    }
  }

  colnames(trns_wgts) <- names
  rownames(trns_wgts) <- names
  names(hyp_wgts) <- names

  validate_mcp_graph(new_mcp_graph(trns_wgts, hyp_wgts))
}

#' @export
#' @rdname mcp_graph
is_mcp_graph <- function(graph) {
  inherits(graph, "mcp_graph")
}
