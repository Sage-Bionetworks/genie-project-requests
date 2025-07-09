library(tidyverse)

dat <- tibble(
  name = character(0),
  n = numeric(0),
  lab = character(0),
  color = character(0)
)

dat <- dat |>
  add_row(
    name = "GENIE",
    n = 200 * 10^6,
    lab = "Main GENIE",
    color = "#a08498"
  ) |>
  add_row(
    name = "BPC pubilc",
    n = 9 * 10^6,
    lab = "BPC Public",
    color = "#bb9bac"
  ) |>
  add_row(
    name = "BPC private",
    n = 11 * 10^6,
    lab = "BPC Private",
    color = "#cabcc9"
  ) |>
  add_row(
    name = "Sponsored projects",
    n = 3 * 10^6,
    lab = "Sponsored Projects",
    color = "#d4ded3"
  ) |>
  add_row(
    name = "Special projects",
    n = 1.5 * 10^6,
    lab = "Special Projects",
    color = "#dcf3da"
  )
