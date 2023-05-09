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
    "Groups specification must be a list" = is.list(groups),
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
          group <- groups[[i]]
          missing <- any(is.na(corr[group, group])) || is.null(corr)
          return(missing)
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

power_input_val <- function(graph, n, theta, corr, success) {
  graph_size <- length(graph$hypotheses)

  stopifnot(
    "Number of simulations must be a length one integer" =
      is.numeric(n) && as.integer(n) == n && length(n) == 1,
    "Mean and covariance parameters must be numeric" =
      is.numeric(theta) && is.numeric(corr),
    "Lengths of `sim_theta` and `sim_corr` must match graph size" =
      unique(length(theta), nrow(corr), ncol(corr)) == graph_size,
    "Covariance matrix for simulating p-values cannot have missing values" =
      !any(is.na(corr)),
    "Covariance matrix for simulating p-values must be symmetric" =
      isSymmetric.matrix(corr),
    "'Success' hypotheses must be positive integers" =
      all(as.integer(success) == success) && all(success > 0),
    "'Success' hypotheses must be less than graph size, and be unique" =
      length(unique(success)) == length(success) && max(success) <= graph_size
  )

  invisible(graph)
}
