## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(graphicalMCP)
library(ggplot2)
library(dplyr)
library(here)
library(gMCP)
library(gt)

## ----initial-graph------------------------------------------------------------
# hyp_names <- c("lo_a1c", "hi_a1c", "lo_tir", "hi_tir", "lo_ret", "hi_ret")
hyp_names <- paste0("H", 1:6)

motivating_example <- function(hyp_1, gamma, hyp_names) {
  eps <- .0001

  hypotheses <- c(hyp_1, 1 - hyp_1, 0, 0, 0, 0)

  transitions <- rbind(
    c(0, gamma, (1 - gamma) / 2, 0, (1 - gamma) / 2, 0),
    c(gamma, 0, 0, (1 - gamma) / 2, 0, (1 - gamma) / 2),
    c(0, 0, 0, 0, 1, 0),
    c(eps, 0, 0, 0, 0, 1 - eps),
    c(0, eps, 1 - eps, 0, 0, 0),
    c(0, 0, 0, 1, 0, 0)
  )

  graph_create(hypotheses, transitions, hyp_names)
}

g_complex <- motivating_example(.75, .99, hyp_names)

print(g_complex)

## ----test-corr----------------------------------------------------------------
rho_com_end <- .5

t_corr <- rep(list(rbind(c(1, rho_com_end), c(rho_com_end, 1))), 3)

## ----power, eval=FALSE--------------------------------------------------------
#  graph_calculate_power(
#    # graph = <variable graph>,
#    alpha = .025,
#    test_groups = list(1:2, c(3, 5), c(4, 6)),
#    test_types = c("p", "s", "s"),
#    test_corr = t_corr,
#    sim_n = 1e5,
#    sim_seed = 6923,
#    sim_success = function(.) .[1] && .[4],
#    # marginal_power = <variable power>,
#    # sim_corr = <variable correlation>
#  )

## ----hyp-weights--------------------------------------------------------------
v_hyp_1 <- c(0.5, 0.75, 1.0)

## ----trn-weights--------------------------------------------------------------
v_gamma <- c(0, .5, .99)

## ----sim-corr-----------------------------------------------------------------
rho_com_end <- 0.5
v_rho_com_dose <- .8 * c(.95, 1, 1.05)

s_corr_list <- lapply(
  v_rho_com_dose,
  function(rho_com_dose) {
    rho_mat <- matrix(rho_com_end * rho_com_dose, 6, 6)

    rho_mat[1:2, 1:2] <- rho_mat[3:4, 3:4] <- rho_mat[5:6, 5:6] <- rho_com_end

    rho_mat[c(1, 3, 5), c(1, 3, 5)] <-
      rho_mat[c(2, 4, 6), c(2, 4, 6)] <-
      rho_com_dose

    diag(rho_mat) <- 1

    rho_mat
  }
)

## ----marginal-power-----------------------------------------------------------
marginal_power_list <- list(
  rep(0, 6),
  rep(.5, 6),
  rep(.9, 6),
  c(.1, 0, 0, .1, 0, 0),
  c(.1, .3, .3, .1, .3, .3),
  c(.3, 0, 0, .3, 0, 0),
  c(0, .1, .1, .4, .2, .2),
  c(.2, 0, .1, .4, .3, 0),
  c(.4, .2, .1, 0, .3, .3)
)

## ----power-results------------------------------------------------------------
power_scenario <- function(hyp_1, gamma, s_corr, marginal_power) {
  graph_calculate_power(
    graph = motivating_example(hyp_1, gamma),
    alpha = .025,
    test_groups = list(1:2, c(3, 5), c(4, 6)),
    test_types = c("p", "s", "s"),
    test_corr = t_corr,
    sim_n = 1e5,
    sim_seed = 6923,
    sim_success = function(.) .[1] && .[2],
    marginal_power = marginal_power,
    sim_corr = s_corr
  )$power
}

# Do some simple caching so this re-knits faster
if (file.exists(here("vignettes/data/power_results.rds"))) {
  res_list <- readRDS(here("vignettes/data/power_results.rds"))
} else {
  res_list <- vector(
    "list",
    length(v_hyp_1) *
      length(v_gamma) *
      length(marginal_power_list) *
      length(s_corr_list)
  )
  i <- 1

  for (hyp_1 in v_hyp_1) {
    for (gamma in v_gamma) {
      for (s_corr in s_corr_list) {
        for (marginal_power in marginal_power_list) {
          res_list[[i]] <- c(
            list(hyp_1, gamma, s_corr[1, 2], marginal_power),
            power_scenario(hyp_1, gamma, s_corr, marginal_power)
          )

          i <- i + 1
        }
      }
    }
  }

  saveRDS(res_list, here("vignettes/data/power_results.rds"))
}

results <- as.data.frame(do.call(rbind, lapply(res_list, unlist)))
# shorten the names so the table isn't quite so wide
names(results) <- c(
  "hyp_1",
  "gamma",
  "rho",
  paste0("H", seq_along(hyp_names)),
  paste0("p_H", seq_along(hyp_names)),
  "p_expect",
  "p_geq_1",
  "p_all",
  "p_success"
)

## ----results-full-------------------------------------------------------------
results %>%
  gt() %>%
  tab_header(
    title = "Power results (all)",
    subtitle = paste0("H", seq_along(hyp_names), ": ", hyp_names, " | ")
  ) %>%
  tab_options(table.font.size = 10)

## ----param-arranged-success---------------------------------------------------
sub_results <- results %>%
  filter(!(H1 == 4 & H4 == 0) & !(H1 == 0 & H4 == 4) & !rho == .99)

sub_results %>%
  arrange(desc(p_success)) %>%
  head(10) %>%
  gt() %>%
  tab_header(
    title = "Power results, constrained (max success)",
    subtitle = paste0("H", seq_along(hyp_names), ": ", hyp_names, " | ")
  ) %>%
  tab_options(table.font.size = 10)

## ----param-arranged-expect----------------------------------------------------
sub_results %>%
  arrange(desc(p_expect)) %>%
  head(10) %>%
  gt() %>%
  tab_header(
    title = "Power results, constrained (max expected)",
    subtitle = paste0("H", seq_along(hyp_names), ": ", hyp_names, " | ")
  ) %>%
  tab_options(table.font.size = 10)

## ----study-graph--------------------------------------------------------------
example_.5_.5 <- motivating_example(.5, .5, hyp_names)

example_.5_.5

## ----study-results------------------------------------------------------------
p_mean_diff_from_placebo <- c(.0129, .0132, .026, .007, .012, .012)

names(p_mean_diff_from_placebo) <- hyp_names

## ----test-bonferroni----------------------------------------------------------
graph_test_shortcut(example_.5_.5, p_mean_diff_from_placebo, alpha = .025)

## ----test-para-bonf-----------------------------------------------------------
graph_test_closure(
  example_.5_.5,
  p_mean_diff_from_placebo,
  alpha = .025,
  groups = list(1:2, 3:6),
  test_types = c("p", "b"),
  corr = list(t_corr[[1]], NA)
)

## ----test-para-simes----------------------------------------------------------
graph_test_closure(
  example_.5_.5,
  p_mean_diff_from_placebo,
  alpha = .025,
  groups = list(1:2, c(3, 5), c(4, 6)),
  test_types = c("p", "s", "s"),
  corr = t_corr,
  verbose = TRUE,
  test_values = TRUE
)

## ----pres-examples-1----------------------------------------------------------
names(example_.5_.5$hypotheses) <-
  rownames(example_.5_.5$transitions) <-
  colnames(example_.5_.5$transitions) <- paste0("H", 1:6)

graph_test_shortcut(
  graph = example_.5_.5,
  p = c(.0123, .0132, .026, .007, .012, .012),
  alpha = .025,
  verbose = TRUE,
  test_values = TRUE
)

## ----pres-examples-2----------------------------------------------------------
t_corr <- rep(list(rbind(c(1, rho_com_end), c(rho_com_end, 1))), 3)

graph_test_closure(
  graph = example_.5_.5,
  p = c(
    .0129,
    .0132,
    .026,
    .007,
    .012,
    .012
  ),
  alpha = .025,
  groups =
    list(1:2, c(3, 5), c(4, 6)),
  test_types =
    c("parametric", "simes", "simes"),
  corr = t_corr,
  verbose = TRUE,
  test_values = TRUE
)

## ----pres-examples-3----------------------------------------------------------
# Assume 0.8 correlation between endpoints with a common dose
s_corr <- rbind(
  c(1, 0.5, 0.8, 0.4, 0.8, 0.4),
  c(0.5, 1, 0.4, 0.8, 0.4, 0.8),
  c(0.8, 0.4, 1, 0.5, 0.8, 0.4),
  c(0.4, 0.8, 0.5, 1, 0.4, 0.8),
  c(0.8, 0.4, 0.8, 0.4, 1, 0.5),
  c(0.4, 0.8, 0.4, 0.8, 0.5, 1)
)

marginal_power <- c(.9, .9, .8, .8, .7, .7)

graph_calculate_power(
  graph = example_.5_.5,
  alpha = .025,
  test_groups = list(1:2, c(3, 5), c(4, 6)),
  test_types = c("parametric", "simes", "simes"),
  test_corr = t_corr,
  sim_n = 1e5,
  marginal_power = marginal_power,
  sim_corr = s_corr,
  sim_success = list(
    `H1 & H2` =
      function(x) x[1] & x[2],
    `(H1 & H3 & H5) | (H2 & H4 & H6)` =
      function(x) {
        (x[1] & x[3] & x[5]) | (x[2] & x[4] & x[6])
      }
  ),
  verbose = TRUE
) |> print(precision = 3)

## ----performance-comp---------------------------------------------------------
# gMCP relies on corr.test for group specification
# t_corr_gmcp <- t_corr
# t_corr_gmcp[3:6, 3:6] <- NA
# diag(t_corr_gmcp) <- 1
#
# if (file.exists(here("vignettes/data/power_timing.rds"))) {
#   power_timing <- readRDS(here("vignettes/data/power_timing.rds"))
# } else {
#   power_timing <- bench::mark(
#     `gMCP shortcut (C)` = calcPower(
#       graph = as_graphMCP(example_.5_.5),
#       alpha = .025,
#       mean = marginal_power, corr.sim = s_corr,
#       n.sim = 2^13
#     ),
#     `graphicalMCP shortcut (R, closure trick)` = graph_calculate_power_r3(
#       example_.5_.5,
#       sim_n = 2^13,
#       marginal_power = marginal_power,
#       sim_corr = s_corr
#     ),
#     `graphicalMCP shortcut (R)` = graph_calculate_power_r(
#       example_.5_.5,
#       sim_n = 2^13,
#       marginal_power = marginal_power,
#       sim_corr = s_corr
#     ),
#     `graphicalMCP closure (R)` = graph_calculate_power_r3(
#       example_.5_.5,
#       test_groups = list(1:2, 3:6),
#       test_types = c("p", "b"),
#       test_corr = t_corr,
#       sim_n = 2^13,
#       marginal_power = marginal_power,
#       sim_corr = s_corr
#     ),
#     # `gMCP closure (R)` = calcPower(
#     #   graph = as_graphMCP(example_.5_.5),
#     #   alpha = .025,
#     #   mean = marginal_power, corr.sim = s_corr,
#     #   n.sim = 2^13,
#     #   corr.test = t_corr_gmcp
#     # ),
#     check = FALSE,
#     min_iterations = 1,
#     time_unit = "s"
#   )
#
#   # We'd like to show time difference for 100,000 simulations, but that just
#   # takes too long for gMCP. So the timing is done for 2^13 simulations, and
#   # results are scaled up.
#   power_timing$median_scale <- power_timing$median * 1e5 / 2^13
#   power_timing$char_expr <- as.character(power_timing$expression)
#
#   saveRDS(power_timing, here("vignettes/data/power_timing.rds"))
# }
#
# power_timing <- power_timing[, keep_cols]
# power_timing_old <- readRDS("vignettes/data/power_timing.rds.bak")[, keep_cols]
# power_timing <- rbind(power_timing, power_timing_old[4, , drop = FALSE])
#
# power_timing[c("char_expr", "median_scale")] %>%
#   mutate(
#     test_types = c("Bonferroni", "Bonferroni", "Bonferroni", "Parametric, Bonferroni", "Parametric, Bonferroni"),
#     .before = median_scale
#   ) %>%
#   gt() %>%
#   tab_header("Power calculations", "100,000 simulations") %>%
#   cols_label(
#     char_expr = "Package/algorithm",
#     test_types = "Test Types",
#     median_scale = "Median runtime (s)"
#   ) %>%
#   tab_footnote(
#     "Shortcut procedures use all Bonferroni testing. Closure procedures use parametric for hypotheses 1 & 2, and Bonferroni otherwise",
#     cells_column_labels("char_expr")
#   ) %>%
#   fmt_number(median_scale) %>%
#   tab_options(table.font.size = 24)

