## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##>"
)

ggplot2::theme_set(ggplot2::theme_bw())

## ----setup--------------------------------------------------------------------
library(graphicalMCP)
library(lrstat)
library(gMCP)

library(gt)
library(here)
library(bench)
library(dplyr)
library(tidyr)
library(tictoc)
library(ggplot2)
library(forcats)

## ----udfs---------------------------------------------------------------------
plot_benchmarks <- function(df_benchmarks,
                            subtitle,
                            x_max = 14,
                            y_breaks = 10^(0:4),
                            scale = FALSE) {
  ggplot(
    df_benchmarks,
    aes(
      size,
      if (scale) log(median * 1e5 / sims) else log(median),
      colour = fct_reorder2(char_expression, size, median),
      group = char_expression
    )
  ) +
    geom_line() +
    geom_point() +
    geom_text(
      data = . %>%
        group_by(char_expression) %>%
        drop_na(median) %>%
        filter(size == max(size)),
      aes(label = char_expression),
      nudge_x = .1,
      nudge_y = .2,
      color = "black",
      size = 3.5,
      hjust = 0,
      check_overlap = TRUE
    ) +
    scale_x_continuous(breaks = 2:10, limits = c(2, x_max)) +
    # scale_y_log10(
    #   breaks = y_breaks,
    #   labels =
    #     function(breaks) format(breaks, scientific = FALSE, big.mark = ",")
    # ) +
    labs(
      title = "Median run time (sec, log scale)",
      subtitle = subtitle,
      x = "Number of hypotheses",
      y = NULL
    ) +
    theme(legend.position = "none", panel.grid.minor = element_blank())
}

table_benchmarks <- function(df, title, unit) {
  df %>%
    select(char_expression, size, median) %>%
    pivot_wider(names_from = size, values_from = median) %>%
    gt() %>%
    tab_header(title) %>%
    fmt_number(
      pattern = paste0("{x}", unit),
      seq_along(unique(df$size)) + 1
    ) %>%
    tab_spanner("Number of hypotheses", seq_along(unique(df$size)) + 1) %>%
    cols_label(char_expression = "Algorithm") %>%
    tab_options(table.font.size = 12)
}

## ----df-benchmarks-gw, fig.width=7, fig.height=5, warning=FALSE---------------
set.seed(7523)

sizes <- seq(2, 12, by = 2)

keep_cols <-
  c(
    "min",
    "median",
    "itr/sec",
    "mem_alloc",
    "gc/sec",
    "n_itr",
    "n_gc",
    "total_time",
    "size",
    "char_expression"
  )

if (file.exists(here("vignettes/data/df_benchmarks_gw.rds"))) {
  df_benchmarks_gw <-
    readRDS(here("vignettes/data/df_benchmarks_gw.rds"))
} else {
  list_benchmarks_gw <- lapply(
    sizes,
    function(size) {
      rando <- random_graph(size)
      trn <- rando$transitions
      hyp <- rando$hypotheses

      # lrstat::fwgtmat() errors out due to weights not summing to 1, sometimes
      # even when they seem to sum to 1 exactly. This loop tests whether
      # fwgtmat() will run successfully, and updates weights if not
      i <- 0
      res_is_error <- TRUE
      while (res_is_error) {
        if (i > 0) {
          hyp[seq_len(size - i)] <- round(hyp[seq_len(size - i)], 6)
          hyp[(size - i + 1):size] <- (1 - sum(hyp[seq_len(size - i)])) / i
        }

        res <- try(fwgtmat(hyp, trn), silent = TRUE)
        res_is_error <- inherits(res, "try-error")

        i <- i + 1
      }

      speed_comp <- mark(
        `gMCP (R)` = generateWeights(trn, hyp),
        `graphicalMCP (R)` = graph_generate_weights(rando),
        `lrstat (C++)` = fwgtmat(hyp, trn),
        check = FALSE,
        min_iterations = 1,
        time_unit = "ms"
      )

      speed_comp$size <- size
      speed_comp$mem_alloc <- as.integer(speed_comp$mem_alloc)
      speed_comp$char_expression <- as.character(speed_comp$expression)

      speed_comp
    }
  )

  df_benchmarks_gw <- do.call(rbind, list_benchmarks_gw)[keep_cols]

  saveRDS(
    df_benchmarks_gw,
    here("vignettes/data/df_benchmarks_gw.rds")
  )
}

df_benchmarks_gw %>%
  filter(char_expression != "lrstat (C++)") %>%
  plot_benchmarks("Generate weights of the closure", 12, 10^(-2:4))

df_benchmarks_gw %>%
  filter(char_expression != "lrstat (C++)") %>%
  select(char_expression, size, median) %>%
  pivot_wider(names_from = size, values_from = median) %>%
  gt() %>%
  tab_header("Generate weights of the closure") %>%
  fmt_number(pattern = "{x}ms", 2:7) %>%
  tab_spanner("Number of hypotheses", 2:7) %>%
  cols_label(char_expression = "Package") %>%
  tab_options(table.font.size = 24)

## ----df-benchmarks-seqtest, fig.width=7, fig.height=5, warning=FALSE----------
set.seed(7523)

sizes <- seq(2, 12, by = 2)

if (file.exists(here("vignettes/data/df_benchmarks_seqtest.rds"))) {
  df_benchmarks_seqtest <-
    readRDS(here("vignettes/data/df_benchmarks_seqtest.rds"))[keep_cols]
} else {
  list_benchmarks_seqtest <- lapply(
    sizes,
    function(size) {
      rando <- random_graph(size)
      trn <- rando$transitions
      hyp <- rando$hypotheses

      p <- as.numeric(
        pnorm(mvtnorm::rmvnorm(1, runif(size, 1.5, 3)), lower.tail = FALSE)
      )

      # lrstat::fwgtmat() errors out due to weights not summing to 1, sometimes
      # even when they seem to sum to 1 exactly. This loop tests whether
      # fwgtmat() will run successfully, and updates weights if not
      if (size != 2) {
        i <- 0
        res_is_error <- TRUE
        while (res_is_error) {
          if (i > 0) {
            hyp[seq_len(size - i)] <- round(hyp[seq_len(size - i)], 6)
            hyp[(size - i + 1):size] <- (1 - sum(hyp[seq_len(size - i)])) / i
          }

          res <- try(fwgtmat(hyp, trn), silent = TRUE)
          res_is_error <- inherits(res, "try-error")

          i <- i + 1
        }
      }

      speed_comp <- mark(
        `gMCP shortcut (C)` =
          gMCP(as_graphMCP(rando), p, alpha = .025)@rejected,
        `graphicalMCP shortcut (R)` = graph_test_shortcut(rando, p),
        `lrstat Bonferroni (C++)` = fadjpbon(hyp, trn, p) <= .025,
        `gMCP closure parametric (R)` =
          gMCP(as_graphMCP(rando), p, alpha = .025, corr = diag(size)),
        `graphicalMCP closure parametric (R)` =
          graph_test_closure(rando, p, corr = diag(size)),
        `lrstat Simes (C++)` =
          fadjpsim(fwgtmat(hyp, trn), p, matrix(1, ncol = size)) <= .025,
        `graphicalMCP closure Simes (R)` =
          graph_test_closure(rando, p, test_types = "s"),
        check = FALSE,
        min_iterations = 5,
        time_unit = "ms"
      )

      speed_comp$size <- size
      speed_comp$mem_alloc <- as.integer(speed_comp$mem_alloc)
      speed_comp$char_expression <- as.character(speed_comp$expression)

      speed_comp
    }
  )

  df_benchmarks_seqtest <- do.call(rbind, list_benchmarks_seqtest)[keep_cols]

  saveRDS(
    df_benchmarks_seqtest,
    here("vignettes/data/df_benchmarks_seqtest.rds")
  )
}

df_benchmarks_seqtest %>%
  filter(
    !char_expression %in% c(
      "lrstat Bonferroni (C++)",
      "lrstat Simes (C++)",
      "graphicalMCP closure Simes (R)"
    )
  ) %>%
  plot_benchmarks("Test a graph")

## ----df-benchmarks-power-shortcut, fig.width=7, fig.height=5, warning=FALSE----
set.seed(7523)

sizes <- seq(4, 20, by = 4)
sims <- 2^17

if (file.exists(here("vignettes/data/df_benchmarks_power_shortcut.rds"))) {
  df_benchmarks_power_shortcut <- readRDS(
    here("vignettes/data/df_benchmarks_power_shortcut.rds")
  )[keep_cols] %>%
    filter(!(char_expression == "gMCP closure parametric (R)" & size > 5))
} else {
  cat("", file = here::here("vignettes/log/pwr_short.log"), append = FALSE)

  list_benchmarks_power_shortcut <- lapply(
    sizes,
    function(size) {
      cat(
        size, "|", format(Sys.time()), "\n",
        file = here::here("vignettes/log/pwr_short.log"),
        append = TRUE
      )

      rando <- random_graph(size)
      trn <- rando$transitions
      hyp <- rando$hypotheses

      marg_pow <- rep(.95, size)
      ncp <- qnorm(1 - .025) - qnorm(1 - marg_pow)
      t_corr <- s_corr <- diag(size)

      speed_comp <- mark(
        `gMCP shortcut (C)` =
          calcPower(hyp, .025, trn, ncp, s_corr, n.sim = sims),
        `graphicalMCP shortcut, high power (R)` =
          graph_calculate_power(rando,
            sim_n = sims, power_marginal = marg_pow,
            sim_corr = s_corr
          ),
        `graphicalMCP shortcut, mid power (R)` =
          graph_calculate_power(rando,
            sim_n = sims, power_marginal = marg_pow / 2,
            sim_corr = s_corr
          ),
        `graphicalMCP shortcut, low power (R)` =
          graph_calculate_power(rando,
            sim_n = sims, power_marginal = marg_pow / 20,
            sim_corr = s_corr
          ),
        check = FALSE,
        min_iterations = 3,
        time_unit = "s",
        memory = FALSE
      )

      speed_comp$size <- size
      speed_comp$mem_alloc <- as.integer(speed_comp$mem_alloc)
      speed_comp$char_expression <- as.character(speed_comp$expression)

      speed_comp
    }
  )

  cat(
    format(Sys.time()), "\n",
    file = here::here("vignettes/log/pwr_short.log"),
    append = TRUE
  )

  df_benchmarks_power_shortcut <-
    do.call(rbind, list_benchmarks_power_shortcut)[keep_cols]

  saveRDS(
    df_benchmarks_power_shortcut,
    here("vignettes/data/df_benchmarks_power_shortcut.rds")
  )
}

df_benchmarks_power_shortcut %>%
  # filter(char_expression != "graphicalMCP closure Simes (R)") %>%
  plot_benchmarks("Power: 131,072 simulations", scale = TRUE)

df_benchmarks_power_shortcut %>%
  table_benchmarks("Power: 131,072 simulations", "s")

## ----df-benchmarks-power-closure, fig.width=7, fig.height=5, warning=FALSE----
set.seed(7523)

sizes <- seq(2, 14, by = 2)
sims <- 2^16

if (file.exists(here("vignettes/data/df_benchmarks_power_closure.rds"))) {
  df_benchmarks_power_closure <- readRDS(
    here("vignettes/data/df_benchmarks_power_closure.rds")
  )[keep_cols] %>%
    filter(!(char_expression == "gMCP closure parametric (R)" & size > 5))
} else {
  cat("", file = here::here("vignettes/log/pwr_clos.log"), append = FALSE)

  list_benchmarks_power_closure <- lapply(
    sizes,
    function(size) {
      cat(
        size, "|", format(Sys.time()), "\n",
        file = here::here("vignettes/log/pwr_clos.log"),
        append = TRUE
      )

      rando <- random_graph(size)
      trn <- rando$transitions
      hyp <- rando$hypotheses

      marg_pow <- rep(.8, size)
      ncp <- qnorm(1 - .025) - qnorm(1 - marg_pow)
      t_corr <- s_corr <- diag(size)

      speed_comp <- mark(
        `gMCP closure parametric (R)` =
          if (size < 6) {
            calcPower(
              hyp, .025, trn,
              marg_pow, s_corr, t_corr, sims
            )
          },
        `graphicalMCP closure Bonferroni (R)` =
          graph_calculate_power(rando,
            sim_n = sims, power_marginal = marg_pow,
            sim_corr = s_corr,
            force_closure = TRUE
          ),
        `graphicalMCP closure parametric (R)` =
          graph_calculate_power(rando,
            sim_n = sims, power_marginal = marg_pow,
            sim_corr = s_corr, test_types = "parametric",
            test_corr = list(t_corr)
          ),
        `graphicalMCP closure Simes (R)` =
          graph_calculate_power(rando,
            sim_n = sims, power_marginal = marg_pow,
            sim_corr = s_corr, test_types = "simes"
          ),
        check = FALSE,
        min_iterations = 1,
        time_unit = "s"
      )

      speed_comp$size <- size
      speed_comp$mem_alloc <- as.integer(speed_comp$mem_alloc)
      speed_comp$char_expression <- as.character(speed_comp$expression)

      speed_comp
    }
  )

  cat(
    format(Sys.time()), "\n",
    file = here::here("vignettes/log/pwr_clos.log"),
    append = TRUE
  )

  df_benchmarks_power_closure <-
    do.call(rbind, list_benchmarks_power_closure)[keep_cols] %>%
    filter(!(char_expression == "gMCP closure parametric (R)" & size > 5))

  saveRDS(
    df_benchmarks_power_closure,
    here("vignettes/data/df_benchmarks_power_closure.rds")
  )
}

df_benchmarks_power_closure %>%
  # filter(char_expression != "graphicalMCP closure Simes (R)") %>%
  plot_benchmarks("Power: 65,536 simulations", scale = TRUE)

df_benchmarks_power_closure %>%
  table_benchmarks("Power: 65,536 simulations", "s")

## ----gg-benchmarks-runtime, fig.width = 7, fig.height=5-----------------------
gg_benchmarks <- ggplot(df_benchmarks_gw) +
  scale_y_log10() +
  theme_minimal()

gg_benchmarks +
  geom_point(
    aes(
      as.factor(size),
      median,
      colour = fct_reorder2(char_expression, size, median)
    )
  ) +
  geom_text(
    data = . %>%
      group_by(char_expression) %>%
      drop_na() %>%
      filter(size == max(size)),
    aes(as.factor(size),
      median,
      label = char_expression
    ),
    nudge_x = .1,
    nudge_y = .12,
    color = "black",
    size = 7
  ) +
  labs(
    colour = "Expression",
    title = "Log of median runtime in milliseconds",
    x = "Graph size",
    y = NULL
  )

## ----gg-benchmarks-memory, fig.width = 7--------------------------------------
gg_benchmarks +
  geom_point(
    aes(
      as.factor(size),
      mem_alloc,
      colour = fct_reorder2(char_expression, size, median)
    )
  ) +
  labs(
    colour = "Expression",
    title = "Log of bytes used",
    x = "Graph size",
    y = NULL
  )

## ----example------------------------------------------------------------------
check_methods <- xfun::cache_rds(
  {
    set.seed(1515)

    m <- 15

    w <- sample(1:m, replace = TRUE)
    w <- w / sum(w)

    g <- replicate(m, sample(1:m, replace = TRUE), simplify = TRUE)
    diag(g) <- 0
    g <- g / rowSums(g)

    graph <- new("graphMCP", m = g, weights = w)
    graph2 <- graph_create(w, g)

    tic("gMCP::generateWeights()")
    gmcp_weights <- generateWeights(graph)
    tictoc_gmcp <- toc(quiet = TRUE)$callback_msg

    tic("graphicalMCP::graph_generate_weights()")
    graphicalmcp_weights <- graph_generate_weights(graph2)
    tictoc_graphicalmcp <- toc(quiet = TRUE)$callback_msg

    # Order of results is reversed between packages, and column names differ,
    # but values are the same
    res_equal <- all.equal(
      unname(gmcp_weights[rev(seq_len(nrow(gmcp_weights))), ]),
      unname(graphicalmcp_weights)
    )

    list(tictoc_gmcp, tictoc_graphicalmcp, res_equal)
  },
  clean = FALSE
)

## ----ex-results---------------------------------------------------------------
cat(check_methods[[1]], "\n")
cat(check_methods[[2]], "\n")
cat(
  "Results from the two packages are equal (up to ordering & names):",
  check_methods[[3]],
  "\n"
)

