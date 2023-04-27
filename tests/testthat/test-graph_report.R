meta_test_graph <- function(...) {
  args <- list(...)

  valid_inputs <- list(
    graph = par_gate,
    p = p_val,
    alpha = alpha_val,
    groups = groups_val,
    test_types = tests_val,
    corr = corr_val,
    verbose = verbose_val,
    critical = critical_val
  )

  if (length(args) > 0) {
    spliced_inputs <- c(
      valid_inputs[!names(valid_inputs) %in% names(args)],
      args
    )
  } else {
    spliced_inputs <- valid_inputs
  }

  do.call(test_graph, spliced_inputs)
}

test_that("invalid test inputs throw errors", {
  par_gate <- simple_successive_1()
  par_gate_inval <- unclass(par_gate)

  p_val <- c(.001, .02, .002, .03)
  p_inval1 <- as.character(p_val)
  p_inval2 <- c(-.001, .01, .002, 1.03)
  p_inval3 <- p_val[1:3]

  alpha_val <- .025
  alpha_inval1 <- ".025"
  alpha_inval2 <- c(.025, .05)
  alpha_inval3 <- 1.1

  tests_val <- c("bonferroni", "simes", "parametric")
  tests_inval1 <- c("bonferoni", "simes", "parametric")
  tests_inval2 <- c("bonferroni", "simes")

  groups_val <- list(1, 2, 3:4)
  groups_inval1 <- list(1, 1, 3:4)
  groups_inval2 <- list(1, 3:4)

  verbose_val <- TRUE
  verbose_inval <- c(TRUE, FALSE)

  critical_val <- TRUE
  critical_inval <- 1

  corr_val <- rbind(
    c(NA, NA, NA, NA),
    c(NA, NA, NA, NA),
    c(NA, NA, 1,  0),
    c(NA, NA, 0,  1)
  )
  corr_inval1 <- corr_val
  corr_inval1[3, 4] <- .01
  corr_inval2 <- diag(2)

  expect_s3_class(meta_test_graph(), "graph_report")

  expect_error(meta_test_graph(graph = par_gate_inval))
  expect_error(meta_test_graph(p = p_inval1))
  expect_error(meta_test_graph(p = p_inval2))
  expect_error(meta_test_graph(p = p_inval3))
  expect_error(meta_test_graph(alpha = alpha_inval1))
  expect_error(meta_test_graph(alpha = alpha_inval2))
  expect_error(meta_test_graph(alpha = alpha_inval3))
  expect_error(meta_test_graph(test_types = tests_inval1))
  expect_error(meta_test_graph(groups = groups_inval1))
  expect_error(
    meta_test_graph(
      test_types = tests_inval2,
      groups = groups_inval2
    )
  )
  expect_error(meta_test_graph(verbose = verbose_inval))
  expect_error(meta_test_graph(critical = critical_inval))
  expect_error(meta_test_graph(corr = corr_inval1))
  expect_error(meta_test_graph(corr = corr_inval2))
})


