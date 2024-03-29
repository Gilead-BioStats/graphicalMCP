## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(graphicalMCP)
library(htmltools)
library(gt)

## ----udfs---------------------------------------------------------------------
# run a single simulation given a few options
sim_func <- function(sim_n,
                     hypotheses,
                     transitions,
                     gamma_props,
                     gamma,
                     rho,
                     theta,
                     ...) {
  new_transitions <- transitions + gamma * gamma_props

  graph <- graph_create(hypotheses, new_transitions)

  sim_corr <- rbind(
    c(1, 0.5, rho, rho / 2),
    c(0.5, 1, rho / 2, rho),
    c(rho, rho / 2, 1, 0.5),
    c(rho / 2, rho, 0.5, 1)
  )

  graph_calculate_power(
    graph = graph,
    alpha = 0.025,
    power_marginal = theta,
    sim_corr = sim_corr,
    sim_n = sim_n,
    sim_success = function(.) .[1],
    ...
  )
}

# create a similar display style to the original table
gt_display_table_1 <- function(df) {
  df %>%
    gt() %>%
    cols_label(
      case    = html("Case<br>#"),
      gamma_1 = em(html("&gamma;<sub>1</sub>")),
      gamma_2 = em(html("&gamma;<sub>2</sub>")),
      rho     = em(html("&rho;")),
      theta_1 = em(html("&theta;<sub>1</sub>")),
      theta_2 = em(html("&theta;<sub>2</sub>")),
      theta_3 = em(html("&theta;<sub>3</sub>")),
      theta_4 = em(html("&theta;<sub>4</sub>")),
      pi      = em(html("&pi;")),
      pi_1    = em(html("&pi;<sub>1</sub>")),
      pi_2    = em(html("&pi;<sub>2</sub>")),
      pi_3    = em(html("&pi;<sub>3</sub>")),
      pi_4    = em(html("&pi;<sub>4</sub>")),
    ) %>%
    tab_header(html(title)) %>%
    tab_spanner("Design parameters", H1:gamma_2) %>%
    tab_spanner("Scenarios", rho:theta_4) %>%
    tab_spanner("Outcome measures", pi:pi_4) %>%
    tab_style(
      list(cell_fill("#d1d3d4"), cell_text(align = "left")),
      cells_title()
    ) %>%
    tab_style(
      cell_text(align = "center"),
      list(
        cells_body(!case),
        cells_column_labels(!case),
        cells_column_spanners()
      )
    ) %>%
    tab_style(
      cell_text(align = "left"),
      list(cells_body(case), cells_column_labels(case))
    ) %>%
    tab_style(cell_borders("all", weight = px(0)), cells_body()) %>%
    tab_options(
      column_labels.border.bottom.color = "black",
      table.border.top.color = "black",
      table.border.top.style = "solid",
      table.border.left.color = "black",
      table.border.left.style = "solid",
      table.border.right.color = "black",
      table.border.right.style = "solid",
      table.border.bottom.color = "black",
      table.border.bottom.style = "solid",
      table_body.border.bottom.style = "none",
      heading.border.bottom.style = "hidden",
      data_row.padding = "2px",
      heading.padding = "2px",
      column_labels.padding = "2px",
      column_labels.border.top.style = "none"
    ) %>%
    opt_css("table th {border-style: none;}")
}

## ----target-------------------------------------------------------------------
table_1_target <- read.table(
  text = "I   0.0125  0.0125  0.5   0.5  0.5   0   0   0   0  0.025  0.015  0.014  0.002  0.001
  2   0.0125  0.0125  0.5   0.5  0.5   3   0   0   0  0.773  0.773  0.018  0.006  0.003
  3   0.0125  0.0125  0.5   0.5  0.5   3   0   3   0  0.774  0.774  0.022  0.596  0.003
  4   0.0125  0.0125  0.5   0.5  0.5   3   0   3   3  0.78   0.78   0.026  0.606  0.025
  5   0.0125  0.0125  0.5   0.5  0.5   2   0   3   3  0.404  0.403  0.023  0.351  0.022
  6   0.0125  0.0125  0.5   0.5  0.5   1   0   3   3  0.111  0.108  0.018  0.102  0.017
  7   0.0125  0.0125  0.5   0.5  0.5   3   3   0   0  0.897  0.806  0.806  0.014  0.015
  8   0.0125  0.0125  0.5   0.5  0.5   3   3   2   2  0.896  0.808  0.809  0.409  0.402
  9   0.0125  0.0125  0.5   0.5  0    3   3   2   2  0.899  0.812  0.81   0.359  0.353
  10  0.0125  0.0125  0.5   0.5  0.99  3   3   2   2  0.897  0.812  0.812  0.448  0.44
  11  0.0125  0.0125  .999  .999  0.5   3   0   3   0  0.774  0.774  0.024  0.131  0.004
  12  0.0125  0.0125   0    0   0.5   3   0   3   0  0.779  0.779  0.026  0.663  0.005
  13  0.025  0     0   0   0.5 3   0  3  0  0.85 0.85 0.023  0.759  0.004
  14  0.025  0     0   0  0.5  0  3   3  0  0.025  0.025  0.024  0.024  0.002
"
)

table_1_target <- data.frame(
  alpha_1 = c(0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.025, 0.025),
  alpha_2 = c(0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 0, 0),
  gamma_1 = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.999, 0, 0, 0),
  gamma_2 = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.999, 0, 0, 0),
  rho = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0, 0.99, 0.5, 0.5, 0.5, 0.5),
  theta_1 = c(0L, 3L, 3L, 3L, 2L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 0L),
  theta_2 = c(0L, 0L, 0L, 0L, 0L, 0L, 3L, 3L, 3L, 3L, 0L, 0L, 0L, 3L),
  theta_3 = c(0L, 0L, 3L, 3L, 3L, 3L, 0L, 2L, 2L, 2L, 3L, 3L, 3L, 3L),
  theta_4 = c(0L, 0L, 0L, 3L, 3L, 3L, 0L, 2L, 2L, 2L, 0L, 0L, 0L, 0L),
  pi = c(0.025, 0.773, 0.774, 0.78, 0.404, 0.111, 0.897, 0.896, 0.899, 0.897, 0.774, 0.779, 0.85, 0.025),
  pi_1 = c(0.015, 0.773, 0.774, 0.78, 0.403, 0.108, 0.806, 0.808, 0.812, 0.812, 0.774, 0.779, 0.85, 0.025),
  pi_2 = c(0.014, 0.018, 0.022, 0.026, 0.023, 0.018, 0.806, 0.809, 0.81, 0.812, 0.024, 0.026, 0.023, 0.024),
  pi_3 = c(0.002, 0.006, 0.596, 0.606, 0.351, 0.102, 0.014, 0.409, 0.359, 0.448, 0.131, 0.663, 0.759, 0.024),
  pi_4 = c(0.001, 0.003, 0.003, 0.025, 0.022, 0.017, 0.015, 0.402, 0.353, 0.44, 0.004, 0.005, 0.004, 0.002)
)

## ----base-graph-specs---------------------------------------------------------
hyp_names <- paste0("H", 1:4)

hyp1 <- c(.5, .5, 0, 0)
hyp2 <- c(1, 0, 0, 0)

hyps <- do.call(
  rbind,
  c(
    rep(list(hyp1), 12),
    rep(list(hyp2), 2)
  )
)
colnames(hyps) <- hyp_names

transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)

gt(as.data.frame(hyps))

## ----gamma-edges--------------------------------------------------------------
gamma_props <- rbind(
  c(0, 1, -1, 0),
  c(1, 0, 0, -1),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0)
)

gamma1 <- c(.5, .5, 0, 0)
gamma2 <- c(.999, .999, 0, 0)
gamma3 <- rep(0, 4)
gammas <- do.call(
  rbind,
  c(
    rep(list(gamma1), 10),
    list(gamma2),
    rep(list(gamma3), 3)
  )
)

## ----mvn-params---------------------------------------------------------------
rhos <- c(rep(.5, 8), 0, .99, rep(.5, 4))

theta1 <- c(0, 0, 0, 0)
theta2 <- c(3, 0, 0, 0)
theta3 <- c(3, 0, 3, 0)
theta4 <- c(3, 0, 3, 3)
theta5 <- c(2, 0, 3, 3)
theta6 <- c(1, 0, 3, 3)
theta7 <- c(3, 3, 0, 0)
theta8 <- c(3, 3, 2, 2)
theta9 <- c(0, 3, 3, 0)

thetas <- rbind(
  theta1,
  theta2,
  theta3,
  theta4,
  theta5,
  theta6,
  theta7,
  theta8,
  theta8,
  theta8,
  theta3,
  theta3,
  theta3,
  theta9
)

## ----scenarios-bonf-----------------------------------------------------------
out_list <- list()

for (i in 1:14) {
  out_list[[i]] <- sim_func(
    sim_n = 100000,
    hypotheses = hyps[i, ], # Hyp scenario
    transitions = transitions, # Base edges
    gamma_props = gamma_props, # Gamma edge locations
    gamma = gammas[i, ], # Gamma edge values
    rho = rhos[[i]], # Correlation base value
    theta = thetas[i, ], # Hypothesis means
  )
}

at_least_1 <- as.numeric(lapply(out_list, function(x) x$power$power_at_least_1))

loc_pow <- do.call(rbind, lapply(out_list, function(x) x$power$power_local))

res_pi <- round(cbind(at_least_1, loc_pow), 3)

table_1 <- cbind(
  1:14,
  hyps,
  gammas,
  rhos,
  thetas,
  res_pi
)

colnames(table_1) <- c(
  "case",
  paste0("H", 1:4),
  paste0("gamma_", 1:4),
  "rho",
  paste0("theta_", 1:4),
  "pi",
  paste0("pi_", 1:4)
)

## ----summarize-bonf-----------------------------------------------------------
title <- "<strong>Table I.</strong> Probability <em>&pi;</em> for a successful trial and individual power <em>&pi;<sub>i</sub></em> for different design options and scenarios (<em>&alpha;</em>=0.025). "

gt_display_table_1(as.data.frame(table_1[, c(1:3, 6:7, 10:19)]))

## ----compare------------------------------------------------------------------
gt(table_1_target[10:14] - table_1[, 15:19])

## ----scenarios-simes, eval=FALSE----------------------------------------------
#  out_list_simes <- list()
#  
#  for (i in 1:14) {
#    out_list_simes[[i]] <- sim_func(
#      sim_n = 100000,
#      hypotheses = hyps[i, ], # Hyp scenario
#      transitions = transitions, # Base edges
#      gamma_props = gamma_props, # Gamma edge locations
#      gamma = gammas[i, ], # Gamma edge values
#      rho = rhos[[i]], # Correlation base value
#      theta = thetas[i, ], # Hypothesis means
#      test_types = "simes"
#    )
#  }
#  
#  at_least_1_simes <- as.numeric(
#    lapply(out_list_simes, function(x) x$power$power_at_least_1)
#  )
#  
#  loc_pow_simes <- do.call(
#    rbind,
#    lapply(out_list_simes, function(x) x$power$power_local)
#  )
#  
#  res_pi_simes <- round(cbind(at_least_1_simes, loc_pow_simes), 3)
#  
#  table_1_simes <- cbind(
#    1:14,
#    hyps,
#    gammas,
#    rhos,
#    thetas,
#    res_pi_simes
#  )
#  
#  colnames(table_1_simes) <- c(
#    "case",
#    paste0("H", 1:4),
#    paste0("gamma_", 1:4),
#    "rho",
#    paste0("theta_", 1:4),
#    "pi",
#    paste0("pi_", 1:4)
#  )
#  
#  title <- "<strong>Table I.</strong> Probability <em>&pi;</em> for a successful trial and individual power <em>&pi;<sub>i</sub></em> for different design options and scenarios (<em>&alpha;</em>=0.025, Simes testing). "
#  
#  gt_display_table_1(as.data.frame(table_1_simes[, c(1:3, 6:7, 10:19)]))

## ----scenarios-mixed, eval=FALSE----------------------------------------------
#  out_list_mix <- list()
#  
#  for (i in 1:14) {
#    out_list_mix[[i]] <- sim_func(
#      sim_n = 100000,
#      hypotheses = hyps[i, ], # Hyp scenario
#      transitions = transitions, # Base edges
#      gamma_props = gamma_props, # Gamma edge locations
#      gamma = gammas[i, ], # Gamma edge values
#      rho = rhos[[i]], # Correlation base value
#      theta = thetas[i, ], # Hypothesis means
#      test_groups = list(1:2, 3:4),
#      test_types = c("p", "s"),
#      test_corr = diag(4)
#    )
#  }
#  
#  at_least_1_mix <- as.numeric(
#    lapply(out_list_mix, function(x) x$power$power_at_least_1)
#  )
#  
#  loc_pow_mix <- do.call(
#    rbind,
#    lapply(out_list_mix, function(x) x$power$power_local)
#  )
#  
#  res_pi_mix <- round(cbind(at_least_1_mix, loc_pow_mix), 3)
#  
#  table_1_mix <- cbind(
#    1:14,
#    hyps,
#    gammas,
#    rhos,
#    thetas,
#    res_pi_mix
#  )
#  
#  colnames(table_1_mix) <- c(
#    "case",
#    paste0("H", 1:4),
#    paste0("gamma_", 1:4),
#    "rho",
#    paste0("theta_", 1:4),
#    "pi",
#    paste0("pi_", 1:4)
#  )
#  
#  title <- "<strong>Table I.</strong> Probability <em>&pi;</em> for a successful trial and individual power <em>&pi;<sub>i</sub></em> for different design options and scenarios (<em>&alpha;</em>=0.025, mixed testing). "
#  
#  gt_display_table_1(as.data.frame(table_1_mix[, c(1:3, 6:7, 10:19)]))

