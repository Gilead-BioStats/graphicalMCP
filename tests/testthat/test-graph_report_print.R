test_that("printing bare minimum", {
  par_gate <- simple_successive_1()

  expect_snapshot(test_graph(par_gate, rep(.01, 4)))

  expect_snapshot(test_graph(par_gate, rep(.01, 4), verbose = TRUE))

  expect_snapshot(test_graph(par_gate, rep(.01, 4), critical = TRUE))

  expect_snapshot(
    print(
      test_graph(par_gate, rep(.01, 4), verbose = TRUE, critical = TRUE),
      precison = 4,
      indent = 4
    )
  )
})
