#' S3 print method for the class `initial_graph`
#'
#' A printed `initial_graph` displays a header stating what the object is, the
#' hypothesis weights, and the transition weights.
#'
#' @param x An object of class `initial_graph` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the number of significant figures
#'   to include in numeric values
#' @param indent An integer scalar indicating how many spaces to indent results
#' @param title First line of printing, used internally to distinguish initial
#'   graphs from updated graphs
#' @export
print.initial_graph <- function(x,
                                ...,
                                precision = 4,
                                indent = 0,
                                title = "Initial graph") {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")

  cat(paste0(pad, title, "\n\n"))

  cat(paste0(pad, "--- Hypothesis weights ---\n"))

  hypotheses_text <- paste(
    pad,
    formatC(
      names(x$hypotheses),
      width = max(nchar(names(x$hypotheses)))
    ),
    ": ",
    format(x$hypotheses, digits = precision),
    sep = "",
    collapse = "\n"
  )

  cat(hypotheses_text, "", sep = "\n")

  cat(paste0(pad, "--- Transition weights ---\n"))

  transitions <- format(
    x$transitions,
    digits = precision,
    scientific = FALSE
  )

  colname_pad <- format("", width = max(nchar(rownames(transitions))))
  label <- paste0(pad_less_1, colname_pad)
  df_trn <- data.frame(
    paste0(pad_less_1, rownames(transitions)),
    transitions,
    check.names = FALSE
  )
  names(df_trn)[[1]] <- label

  transitions_text <- data.frame(df_trn, check.names = FALSE)

  print(transitions_text, row.names = FALSE)
}
