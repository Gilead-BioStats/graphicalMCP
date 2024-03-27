test_input_val <- function(graph,
                           p,
                           alpha,
                           test_groups = list(seq_along(graph$hypotheses)),
                           test_types = c("bonferroni"),
                           test_corr,
                           verbose,
                           test_values) {
  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )

  corr_is_matrix_list <- is.list(test_corr) &&
    all(
      vapply(test_corr, function(elt) is.matrix(elt) || is.na(elt), logical(1))
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
    "Groups specification must be a list" = is.list(test_groups),
    "Please include each hypothesis in exactly one group" =
      setequal(seq_along(graph$hypotheses), unlist(test_groups)) &&
        length(graph$hypotheses) == length(unlist(test_groups)),
    "Correlation matrix should be a list of matrices or missing values" =
      corr_is_matrix_list,
    "Number of test types, groups, and correlation matrices should match" =
      unique(length(test_types), length(test_groups)) == length(test_corr),
    "Length of p-values & groups must match the number of hypotheses" =
      unique(length(p), length(unlist(test_groups))) ==
        length(graph$hypotheses),
    "Verbose flag must be a length one logical" =
      is.logical(verbose) && length(verbose) == 1,
    "Test values flag must be a length one logical" =
      is.logical(test_values) && length(test_values) == 1
  )

  # Additional correlation matrix checks ---------------------------------------
  corr_parametric <- test_corr[names(test_types)[test_types == "parametric"]]

  missing_corr <- any(
    vapply(corr_parametric, function(cr) any(is.na(cr)), logical(1))
  )

  symmetric_corr <- all(vapply(corr_parametric, isSymmetric.matrix, logical(1)))

  bounded_corr <- all(
    vapply(corr_parametric, function(cr) all(cr >= 0 & cr <= 1), logical(1))
  )

  # Positive definite-ness is irrelevant if there are missing values, and
  # testing for it will throw an error
  positive_definite_corr <- ifelse(
    !missing_corr,
    all(
      vapply(
        corr_parametric,
        function(cr) all(round(eigen(cr)$values, 10) >= 0),
        logical(1)
      )
    ),
    TRUE
  )

  stopifnot(
    "Correlation matrix for parametric test groups must be fully specified" =
      !missing_corr,
    "Correlation matrix must be symmetric" = symmetric_corr,
    "Dimensions of correlation matrices must match the parametric test groups" =
      all(
        lengths(test_corr[names(test_types)[test_types == "parametric"]]) ==
          lengths(test_groups[names(test_types)[test_types == "parametric"]])^2
      ),
    "Correlation values must be between 0 & 1" = bounded_corr,
    "Correlation matrix must be positive definite for parametric test groups" =
      positive_definite_corr
  )

  invisible(graph)
}

power_input_val <- function(graph, sim_n, power_marginal, test_corr, success) {
  num_hyps <- length(graph$hypotheses)

  stopifnot(
    "Number of simulations must be a length one integer" =
      is.numeric(sim_n) && as.integer(sim_n) == sim_n && length(sim_n) == 1,
    "Marginal power must be between 0 and 1" =
      all(power_marginal >= 0 & power_marginal <= 1),
    "Marginal power and correlation parameters must be numeric" =
      is.numeric(power_marginal) && is.numeric(test_corr),
    "Lengths of marginal power must match number of hypotheses" =
      length(power_marginal) == num_hyps,
    "Correlation matrix for simulating p-values must match no. of hypotheses" =
      unique(nrow(test_corr), ncol(test_corr)) == num_hyps,
    "Correlation matrix for simulating p-values cannot have missing values" =
      !any(is.na(test_corr)),
    "Correlation matrix for simulating p-values must be symmetric" =
      isSymmetric.matrix(test_corr),
    "Correlation matrix for simulating p-values must have diagonal all 1" =
      all(diag(test_corr) == 1),
    "Correlation matrix for simulating p-values must be positive definite" =
      all(round(eigen(test_corr)$values, 10) >= 0),
    "'sim_success' must be a list of functions" =
      all(vapply(success, is.function, logical(1)))
  )

  invisible(graph)
}
