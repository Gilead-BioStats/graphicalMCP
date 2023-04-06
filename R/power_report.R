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
calculate_power_slow <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_corr = diag(length(graph$hypotheses)),
                            gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  test_types <- test_opts[tolower(test_types)]
  if (length(test_types) == 1) {
    test_types <- rep(test_types, length(test_groups))
  }

  fake_p <- rep(0, length(graph$hypotheses))
  test_input_val(
    graph,
    fake_p,
    test_alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  for (row in seq_len(sim_n)) {
    if (all(test_types == "bonferroni" | test_types == "b")) {
      test_res_mat[row, ] <- bonferroni_sequential(
        graph,
        p_sim[row, ],
        test_alpha
      )$outputs$rejected
    } else {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

#' @export
calculate_power <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_corr = diag(length(graph$hypotheses)),
                            gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  test_types <- test_opts[tolower(test_types)]
  if (length(test_types) == 1) {
    test_types <- rep(test_types, length(test_groups))
  }

  fake_p <- rep(0, length(graph$hypotheses))
  test_input_val(
    graph,
    fake_p,
    test_alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  if (all(test_types == "bonferroni" | test_types == "b")) {
    test_res_mat <- bonferroni_sequential_power_cpp(
      graph$hypotheses,
      graph$transitions,
      p_sim,
      test_alpha
    )
  } else if (all(test_types == "simes" | test_types == "s")) {
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_simes_ordered_r(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else if (all(test_types == "parametric" | test_types == "p")) {
    gwc <- add_critical(graph, test_corr, test_alpha, test_groups)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_parametric(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        gwc,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else {
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
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
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

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
                       test_alpha = .05,
                       test_groups = list(seq_along(graph$hypotheses)),
                       test_types = c("bonferroni"),
                       test_corr = NULL,
                       sim_n = 100,
                       sim_theta = rep(0, length(graph$hypotheses)),
                       sim_success = 1:2,
                       sim_corr = diag(length(graph$hypotheses)),
                       gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  test_input_val(
    graph,
    rep(0, length(graph$hypotheses)),
    test_alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  if (all(test_types == "bonferroni" | test_types == "b")) {
    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- bonferroni_sequential_cpp(
        graph$hypotheses,
        graph$transitions,
        p_sim[row, ],
        test_alpha
      )
    }
  } else if (all(test_types == "simes" | test_types == "s")) {
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_simes(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else {
    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}
