test_that("printing Bonferroni/Simes closure test", {
  par_gate <- simple_successive_1()

  expect_snapshot(graph_test_closure(par_gate, rep(.01, 4), test_types = "s"))

  expect_snapshot(graph_test_closure(par_gate, rep(.01, 4), verbose = TRUE))

  expect_snapshot(graph_test_closure(par_gate, rep(.01, 4), test_values = TRUE))
})

test_that("printing parametric closure test", {
  par_gate <- simple_successive_1()

  expect_snapshot(
    graph_test_closure(
      par_gate,
      rep(.01, 4),
      test_types = "p",
      corr = list(diag(4))
    )
  )

  expect_snapshot(
    graph_test_closure(
      par_gate,
      rep(.01, 4),
      groups = list(1:2, 3:4),
      test_types = c("p", "s"),
      corr = list(diag(2), NA),
      test_values = TRUE,
      verbose = TRUE
    )
  )

  expect_snapshot(
    graph_test_closure(
      par_gate,
      rep(.01, 4),
      groups = list(1:2, 3:4),
      test_types = c("p", "p"),
      corr = list(diag(2), diag(2)),
      test_values = TRUE,
      verbose = TRUE
    )
  )
})

test_that("printing Bonferroni sequential results", {
  expect_snapshot(graph_test_shortcut(simple_successive_1(), rep(.01, 4)))
})

test_that("additional printing options for graph report", {
  par_gate <- simple_successive_1()

  expect_snapshot(
    print(
      graph_test_closure(
        par_gate,
        rep(.01, 4),
        verbose = TRUE,
        test_values = TRUE
      ),
      precison = 4,
      indent = 4
    )
  )

  expect_snapshot(
    print(
      graph_test_shortcut(
        simple_successive_1(),
        rep(.01, 4),
        verbose = TRUE,
        test_values = TRUE
      ),
      precision = 7,
      indent = 9
    )
  )

  expect_snapshot(
    print(
      graph_test_shortcut(
        complex_example_1(),
        5:0 / 200,
        verbose = TRUE,
        test_values = TRUE
      )
    )
  )
})
