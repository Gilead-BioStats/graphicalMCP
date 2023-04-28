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
    calculate_power_vms(rando, sim_n = 1e5, seed = 42823),
    calculate_power_vms(
      rando,
      .05,
      list(1, 2),
      c("s", "p"),
      sim_n = 1e5,
      seed = 42823
    )
  )
})
