devtools::check(quiet = FALSE, vignettes = FALSE)
# devtools::load_all("..")
#
# test_m <- function(m, test_groups = list(seq_len(m)), test_types = "b", verbose = FALSE, critical = FALSE) {
#   w <- sample(1:m, replace = TRUE)
#   w <- w / sum(w)
#   g <- replicate(m, sample(1:m, replace = TRUE), simplify = TRUE)
#   diag(g) <- 0
#   g <- g / rowSums(g)
#   graph2 <- graph_create(w, g)
#
#   p <- runif(m, .0001, .05)
#   sim_corr <- diag(m)
#
#   bench::mark(
#     graph_test_closure(
#       graph2,
#       p = p,
#       alpha = .05,
#       test_corr = sim_corr,
#       test_groups = groups,
#       test_types = test_types,
#       verbose = verbose,
#       critical = critical
#     ),
#     min_iterations = 5
#   )
# }
#
# my_press <- bench::press(
#   test_types = c("b", "s", "p"),
#   m = 4:10,
#   verbose = c(TRUE, FALSE),
#   critical = c(TRUE, FALSE),
#   {test_m(m, test_types = test_types, verbose = verbose, critical = critical)}
# )
#
# vroom::vroom_write(x = my_press, file = "./data/my_press.csv", delim = ",")
