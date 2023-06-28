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
    "Number of test types should match the number of test groups" =
      length(test_types) == length(groups),
    "Length of p-values & groups must match the number of hypotheses" =
      unique(length(p), length(unlist(groups))) == length(graph$hypotheses),
    "Verbose flag must be a length one logical" =
      is.logical(verbose) && length(verbose) == 1,
    "Critical flag must be a length one logical" =
      is.logical(critical) && length(critical) == 1
  )

  missing_corr <- any(
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

  positive_definite_corr <- ifelse(
    !missing_corr,
    all(
      vapply(
        seq_along(test_types),
        function(i) {
          if (test_types[[i]] == "parametric") {
            group <- groups[[i]]
            pos_def <- all(eigen(corr[group, group])$values >= 0)
            return(pos_def)
          } else {
            return(TRUE)
          }
        },
        logical(1)
      )
    ),
    TRUE
  )

  stopifnot(
    "Correlation sub-matrix for each parametric test group must be fully specified" =
      !missing_corr,
    "Correlation matrix must be symmetric" =
      isSymmetric.matrix(corr) || is.null(corr),
    "Dimensions of correlation matrix must match the number of hypotheses" =
      unique(nrow(corr), ncol(corr)) == length(graph$hypotheses) ||
        is.null(corr),
    "Correlation values must be between 0 & 1" =
      all((corr >= 0 & corr <= 1) | is.na(corr)) || is.null(corr),
    "Correlation matrix must be positive definite for each parametric test group" =
      positive_definite_corr
  )

  invisible(graph)
}

power_input_val <- function(graph, sim_n, marginal_power, corr, success) {
  num_hyps <- length(graph$hypotheses)

  stopifnot(
    "Number of simulations must be a length one integer" =
      is.numeric(sim_n) && as.integer(sim_n) == sim_n && length(sim_n) == 1,
    "Marginal power and correlation parameters must be numeric" =
      is.numeric(marginal_power) && is.numeric(corr),
    "Lengths of marginal power and Correlation matrix for simulating p-values must match number of hypotheses" =
      unique(length(marginal_power), nrow(corr), ncol(corr)) == num_hyps,
    "Correlation matrix for simulating p-values cannot have missing values" =
      !any(is.na(corr)),
    "Correlation matrix for simulating p-values must be symmetric" =
      isSymmetric.matrix(corr),
    "Correlation matrix for simulating p-values must have diagonal all 1" =
      all(diag(corr) == 1),
    "Correlation matrix for simulating p-values must be positive definite" =
      all(eigen(corr)$values >= 0),
    "'sim_success' must be a list of functions" =
      all(vapply(success, is.function, logical(1)))
  )

  invisible(graph)
}
