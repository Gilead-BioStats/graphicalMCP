# library(gMCP)
#
# graph <- simpleSuccessiveII()
#
# corMat <- rbind(c(1, 0.5, 0.5, 0.5/2),
#                 c(0.5,1,0.5/2,0.5),
#                 c(0.5,0.5/2,1,0.5),
#                 c(0.5/2,0.5,0.5,1))
#
# theta <- c(4, 0, 0, 0)
#
# calcPower(graph=graph, alpha=0.025, mean=theta, corr.sim=corMat, n.sim=100000)

# basic function, not doing any "outside work" like generating weights
#' @export
run_power <- function(graph,
                      success = 1:2,
                      alpha = .05,
                      groups = list(seq_along(graph$hypotheses)),
                      test_types = c("bonferroni"),
                      test_corr = NULL,
                      sim_corr = diag(length(graph$hypotheses)),
                      n_sim = 100,
                      gamma = NULL,
                      theta = rep(0, length(graph$hypotheses))) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  p_sim <- pnorm(
    mvtnorm::rmvnorm(n_sim, theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = n_sim,
    ncol = length(theta),
    dimnames = list(seq_len(n_sim), names(graph$hypotheses))
  )

  for (row in seq_len(n_sim)) {
    if (all(test_types == "bonferroni" | test_types == "b")) {
      test_res_mat[row, ] <- bonferroni_sequential(
        graph,
        p_sim[row, ],
        alpha
      )$outputs$rejected
    } else {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        alpha,
        groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

#' @export
# generates weights only once
run_power2 <- function(graph,
                      success = 1:2,
                      alpha = .05,
                      groups = list(seq_along(graph$hypotheses)),
                      test_types = c("bonferroni"),
                      test_corr = NULL,
                      sim_corr = diag(length(graph$hypotheses)),
                      n_sim = 100,
                      gamma = NULL,
                      theta = rep(0, length(graph$hypotheses))) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  test_input_val(
    graph,
    rep(0, length(graph$hypotheses)),
    alpha,
    groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  p_sim <- pnorm(
    mvtnorm::rmvnorm(n_sim, theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  # some values for testing
  gw <- generate_weights(graph)
  graph_size <- length(graph$hypotheses)
  gw_size <- 2 ^ graph_size - 1
  num_groups <- length(groups)

  test_res_mat <- matrix(
    NA,
    nrow = n_sim,
    ncol = length(theta),
    dimnames = list(seq_len(n_sim), names(graph$hypotheses))
  )

  for (row in seq_len(n_sim)) {
    if (all(test_types == "bonferroni" | test_types == "b")) {
      test_res_mat[row, ] <- bonferroni_sequential2(
        graph,
        p_sim[row, ],
        alpha,
        check_input = FALSE
      )$outputs$rejected
    } else {
      test_res_mat[row, ] <- test_graph_fast(
        graph,
        p_sim[row, ],
        alpha,
        groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

#' @export
run_power3 <- function(graph,
                      success = 1:2,
                      alpha = .05,
                      groups = list(seq_along(graph$hypotheses)),
                      test_types = c("bonferroni"),
                      test_corr = NULL,
                      sim_corr = diag(length(graph$hypotheses)),
                      n_sim = 100,
                      gamma = NULL,
                      theta = rep(0, length(graph$hypotheses))) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  test_input_val(
    graph,
    rep(0, length(graph$hypotheses)),
    alpha,
    groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  p_sim <- pnorm(
    mvtnorm::rmvnorm(n_sim, theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = n_sim,
    ncol = length(theta),
    dimnames = list(seq_len(n_sim), names(graph$hypotheses))
  )

  for (row in seq_len(n_sim)) {
    if (all(test_types == "bonferroni" | test_types == "b")) {
      test_res_mat[row, ] <- bonferroni_sequential3(
        graph,
        p_sim[row, ],
        alpha,
        check_input = FALSE
      )$outputs$rejected
    } else {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        alpha,
        groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

#' @export
run_power4 <- function(graph,
                       success = 1:2,
                       alpha = .05,
                       groups = list(seq_along(graph$hypotheses)),
                       test_types = c("bonferroni"),
                       test_corr = NULL,
                       sim_corr = diag(length(graph$hypotheses)),
                       n_sim = 100,
                       gamma = NULL,
                       theta = rep(0, length(graph$hypotheses))) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  test_input_val(
    graph,
    rep(0, length(graph$hypotheses)),
    alpha,
    groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  p_sim <- pnorm(
    mvtnorm::rmvnorm(n_sim, theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = n_sim,
    ncol = length(theta),
    dimnames = list(seq_len(n_sim), names(graph$hypotheses))
  )

  if (all(test_types == "bonferroni" | test_types == "b")) {
    for (row in seq_len(n_sim)) {
      test_res_mat[row, ] <- bonferroni_sequential_cpp(
        graph$hypotheses,
        graph$transitions,
        p_sim[row, ],
        alpha
      )
    }
  } else if (all(test_types == "simes" | test_types == "s")) {
    # some values for testing
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(groups)

    for (row in seq_len(n_sim)) {
      test_res_mat[row, ] <- test_graph_fast_simes(
        graph,
        p_sim[row, ],
        alpha,
        groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else {
    for (row in seq_len(n_sim)) {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        alpha,
        groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

#' @export
run_power5 <- function(graph,
                       success = 1:2,
                       alpha = .05,
                       groups = list(seq_along(graph$hypotheses)),
                       test_types = c("bonferroni"),
                       test_corr = NULL,
                       sim_corr = diag(length(graph$hypotheses)),
                       n_sim = 100,
                       gamma = NULL,
                       theta = rep(0, length(graph$hypotheses))) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  p_sim <- pnorm(
    mvtnorm::rmvnorm(n_sim, theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = n_sim,
    ncol = length(theta),
    dimnames = list(seq_len(n_sim), names(graph$hypotheses))
  )

  if (all(test_types == "bonferroni" | test_types == "b")) {
    test_res_mat <- bonferroni_sequential_power(
      graph$hypotheses,
      graph$transitions,
      p_sim,
      alpha
    )
  } else {
    for (row in seq_len(n_sim)) {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        alpha,
        groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}
