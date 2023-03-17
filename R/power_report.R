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

  power_all <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  reject_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_all = power_all,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    reject_all = reject_all,
    power_success = power_success
  )
}

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
      test_res_mat[row, ] <- bonferroni_sequential2(
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

  power_all <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  reject_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_all = power_all,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    reject_all = reject_all,
    power_success = power_success
  )
}

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

  power_all <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  reject_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_all = power_all,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    reject_all = reject_all,
    power_success = power_success
  )
}


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
      cat(graph$hypotheses, "\n")
      print(graph$transitions)
      cat(p_sim[row, ], "\n")
      test_res_mat[row, ] <- bs_fast(
        graph$hypotheses,
        graph$transitions,
        p_sim[row, ],
        alpha,
        length(graph$hypotheses)
      )
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

  power_all <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / n_sim
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  reject_all <- mean(rowSums(test_res_mat) == length(theta))
  power_success <- mean(rowSums(test_res_mat[, success]) > 0)

  list(
    power_all = power_all,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    reject_all = reject_all,
    power_success = power_success
  )
}
