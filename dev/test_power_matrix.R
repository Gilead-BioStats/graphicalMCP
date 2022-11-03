library(tidyverse)
library(microbenchmark)

dir <- 'dev/power_matrix'

purrr::walk(
    list.files(dir, "R$", full.names = TRUE),
    source
)

n <- 20
times <- 25
measurements <- vector('list', n)

for (i in 1:n) {
    message(glue::glue('Evaluating n = {i} {times} times'))
    measurement <- microbenchmark(
        power_matrix1(i),
        power_matrix2(i),
        power_matrix3(i),
        times = times
    )
    measurement$n <- i
    measurements[[i]] <- measurement
}

measurement_summary <- measurements %>%
    bind_rows() %>%
    mutate(
        method = case_when(
            expr == 'power_matrix1(i)' ~ 'outer()',
            expr == 'power_matrix2(i)' ~ 'expand.grid()',
            expr == 'power_matrix3(i)' ~ 'for loops'
        )
    )

measurement_summary %>%
    ggplot(aes(
        x = time/10^9,
        fill = method
    )) +
    geom_density(
        alpha = .3
    ) +
    facet_wrap(
        vars(n),
        scales = 'free'
    ) +
    labs(
        fill = 'Method',
        title = glue::glue('Power Matrix Performance ({times} evaluations per set size)'),
        x = 'Time (s)',
        y = 'Density',
    )

ggsave(
    glue::glue('{dir}/benchmark_n={n}_times={times}.png'),
    width = 12,
    height = 8,
    units = 'in'
)
