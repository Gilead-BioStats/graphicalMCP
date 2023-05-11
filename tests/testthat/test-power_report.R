test_that("improper inputs throw errors", {
  rando <- random_graph(3)

  expect_no_error(calculate_power(rando))

  expect_error(calculate_power(rando, sim_n = 100.5))

  expect_error(calculate_power(rando, sim_theta = c("1", 1, 1)))
  expect_error(calculate_power(rando, sim_corr = matrix("1", 3, 3)))

  expect_error(calculate_power(rando, sim_theta = c(1, 1)))
  expect_error(calculate_power(rando, sim_corr = matrix(1, 2, 2)))

  expect_error(calculate_power(rando, sim_corr = matrix(NA, 3, 3)))
  corr_inval <- matrix(c(1, 0, .5, 0, 1, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
  expect_error(calculate_power(rando, sim_corr = corr_inval))

  expect_error(calculate_power(rando, sim_success = 0))
  expect_error(calculate_power(rando, sim_success = .5))
  expect_error(calculate_power(rando, sim_success = c(1, 1)))
  expect_error(calculate_power(rando, sim_success = 4))
})

test_that("power results are identical under a given seed", {
  rando <- random_graph(2)

  expect_equal(
    calculate_power(rando, sim_n = 1e5, sim_seed = 42823),
    calculate_power(rando, sim_n = 1e5, sim_seed = 42823)
  )

  expect_equal(
    calculate_power(
      rando,
      test_types = "s",
      sim_n = 1e5,
      sim_seed = 42823
    ),
    calculate_power(
      rando,
      test_types = "s",
      sim_n = 1e5,
      sim_seed = 42823
    )
  )

  expect_equal(
    calculate_power(
      rando,
      test_types = "p",
      test_corr = diag(2),
      sim_n = 1e5,
      sim_seed = 42823
    ),
    calculate_power(
      rando,
      test_types = "p",
      test_corr = diag(2),
      sim_n = 1e5,
      sim_seed = 42823
    )
  )
})

test_that("results are identical whether using closure test or shortcut", {
  g <- wiens_dmitrienko_2005()

  expect_equal(
    calculate_power(g, sim_seed = 51123),
    calculate_power(g, sim_seed = 51123, force_closure = TRUE)
  )
})

test_that("size one groups are turned into Bonferroni", {
  g <- fallback()

  expect_equal(
    calculate_power(
      g,
      .05,
      list(1, 2, 3),
      c("s", "p", "p"),
      sim_n = 1e5,
      sim_seed = 42823
    )$inputs$test_types,
    c("bonferroni", "bonferroni", "bonferroni"),
    ignore_attr = TRUE
  )
})

test_that("parallel gatekeeping with 1-corr parametric runs without error", {
  expect_no_error(
    calculate_power(
      simple_successive_1(),
      test_types = "p",
      test_corr = matrix(1, 4, 4)
    )
  )
})

test_that("multi-group/multi-test type runs without error", {
  expect_no_error(
    calculate_power(
      random_graph(4),
      test_groups = list(c(4, 1), 2:3),
      test_types = "s"
    )
  )

  expect_no_error(
    calculate_power(
      random_graph(4),
      test_groups = list(c(3, 1), c(2, 4)),
      test_types = "p",
      test_corr = diag(4)
    )
  )
})
