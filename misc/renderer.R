rmarkdown::render(
  here::here("vignettes/testing-basics.Rmd"),
  output_file = here::here(
    paste0(
      "vignettes/knitted/testing-basics-m",
      3,
      "-nsim",
      100000,
      "-",
      stringr::str_replace_all(Sys.Date(), "-", "")
    )
  ),
  params = list(m = 3)
)

rmarkdown::render(
  here::here("vignettes/testing-basics.Rmd"),
  output_file = here::here(
    paste0(
      "vignettes/knitted/testing-basics-m",
      4,
      "-nsim",
      100000,
      "-",
      stringr::str_replace_all(Sys.Date(), "-", "")
    )
  ),
  params = list(m = 4)
)

rmarkdown::render(
  here::here("vignettes/testing-basics.Rmd"),
  output_file = here::here(
    paste0(
      "vignettes/knitted/testing-basics-m",
      5,
      "-nsim",
      100000,
      "-",
      stringr::str_replace_all(Sys.Date(), "-", "")
    )
  ),
  params = list(m = 5)
)


rmarkdown::render(
  here::here("vignettes/testing-basics.Rmd"),
  output_file = here::here(
    paste0(
      "vignettes/knitted/testing-basics-m",
      6,
      "-nsim",
      100000,
      "-",
      stringr::str_replace_all(Sys.Date(), "-", "")
    )
  ),
  params = list(m = 6)
)

rmarkdown::render(
  here::here("vignettes/testing-basics.Rmd"),
  output_file = here::here(
    paste0(
      "vignettes/knitted/testing-basics-m",
      7,
      "-nsim",
      100000,
      "-",
      stringr::str_replace_all(Sys.Date(), "-", "")
    )
  ),
  params = list(m = 7)
)

rmarkdown::render(
  here::here("vignettes/testing-basics.Rmd"),
  output_file = here::here(
    paste0(
      "vignettes/knitted/testing-basics-m",
      8,
      "-nsim",
      100000,
      "-",
      stringr::str_replace_all(Sys.Date(), "-", "")
    )
  ),
  params = list(m = 8)
)
