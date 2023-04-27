#' @rdname testing
test_input_val <- function(graph,
                           p,
                           alpha,
                           groups = list(seq_along(graph$hypotheses)),
                           test_types = c("bonferroni"),
                           corr,
                           verbose,
                           critical) {
  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )

  stopifnot(
    "Please test an `initial_graph` object" = class(graph) == "initial_graph",
    "P-values must be numeric" = is.numeric(p),
    "P-values must be between 0 & 1" = all(p >= 0 & p <= 1),
    "Alpha must be numeric" = is.numeric(alpha),
    "Please choose a single alpha level for testing" = length(alpha) == 1,
    "Alpha must be between 0 & 1" = alpha >= 0 && alpha <= 1,
    "Only Bonferroni, parametric, and Simes tests are currently supported" =
      all(test_types %in% test_opts),
    "Please include each hypothesis in exactly one group" =
      setequal(seq_along(graph$hypotheses), unlist(groups)) &&
        length(graph$hypotheses) == length(unlist(groups)),
    "Please choose one test, or one test per group" =
      length(test_types) == length(groups),
    "Length of p-values & groups must match size of graph" =
      unique(length(p), length(unlist(groups))) == length(graph$hypotheses),
    "Verbose flag must be a length one logical" =
      is.logical(verbose) && length(verbose) == 1,
    "Critical flag must be a length one logical" =
      is.logical(critical) && length(critical) == 1
  )

  valid_corr <- !any(
    vapply(
      seq_along(test_types),
      function(i) {
        if (test_types[[i]] == "parametric") {
          return(corr_has_missing(corr, groups[[i]]))
        } else {
          return(FALSE)
        }
      },
      logical(1)
    )
  )

  stopifnot(
    "Correlation sub-matrix for each parametric test group must be complete" =
      valid_corr,
    "Correlation matrix must be symmetric" =
      isSymmetric.matrix(corr) || is.null(corr),
    "Dimensions of correlation matrix must match size of graph" =
      unique(nrow(corr), ncol(corr)) == length(graph$hypotheses) ||
        is.null(corr)
  )

  invisible(graph)
}
