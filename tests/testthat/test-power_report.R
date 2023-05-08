test_that("improper inputs throw errors", {
  rando <- random_graph(3)

  expect_no_error(calculate_power_vms(rando))

  expect_error(calculate_power_vms(rando, sim_n = 100.5))

  expect_error(calculate_power_vms(rando, sim_theta = c("1", 1, 1)))
  expect_error(calculate_power_vms(rando, sim_corr = matrix("1", 3, 3)))

  expect_error(calculate_power_vms(rando, sim_theta = c(1, 1)))
  expect_error(calculate_power_vms(rando, sim_corr = matrix(1, 2, 2)))

  expect_error(calculate_power_vms(rando, sim_corr = matrix(NA, 3, 3)))
  corr_inval <- matrix(c(1, 0, .5, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
  expect_error(calculate_power_vms(rando, sim_corr = corr_inval))

  expect_error(calculate_power_vms(rando, sim_success = 0))
  expect_error(calculate_power_vms(rando, sim_success = .5))
  expect_error(calculate_power_vms(rando, sim_success = c(1, 1)))
  expect_error(calculate_power_vms(rando, sim_success = 4))
})

test_that("power results are identical under a given seed", {
  rando <- random_graph(2)

  expect_equal(
    calculate_power_vms(rando, sim_n = 1e5, seed = 42823),
    calculate_power_vms(rando, sim_n = 1e5, seed = 42823)
  )

  expect_equal(
    calculate_power_vms(
      rando,
      test_types = "s",
      sim_n = 1e5,
      seed = 42823
    ),
    calculate_power_vms(
      rando,
      test_types = "s",
      sim_n = 1e5,
      seed = 42823
    )
  )

  expect_equal(
    calculate_power_vms(
      rando,
      test_types = "p",
      test_corr = diag(2),
      sim_n = 1e5,
      seed = 42823
    ),
    calculate_power_vms(
      rando,
      test_types = "p",
      test_corr = diag(2),
      sim_n = 1e5,
      seed = 42823
    )
  )
})

test_that("size one groups are turned into Bonferroni", {
  rando <- random_graph(2)

  expect_equal(
    calculate_power_vms(
      rando,
      .05,
      list(1, 2),
      c("s", "p"),
      sim_n = 1e5,
      seed = 42823
    )$inputs$test_types,
    c("bonferroni", "bonferroni"),
    ignore_attr = TRUE
  )
})

test_that("parallel gatekeeping with 1-corr parametric runs without error", {
  expect_no_error(
    calculate_power_vms(
      simple_successive_1(),
      test_types = "p",
      test_corr = matrix(1, 4, 4)
    )
  )
})
