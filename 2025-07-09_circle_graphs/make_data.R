library(tidyverse)

dat <- tibble(
  name = character(0),
  n = numeric(0),
  lab = character(0),
  color = character(0)
)

# For now I'm just going to input the numbers.  You could obviously add
#   a section to download the data and get them directly if you want.
dat <- dat |>
  add_row(
    name = "GENIE",
    n = 230 * 10^3,
    lab = "Main GENIE",
    color = "#ffecd1"
  ) |>
  add_row(
    name = "BPC consortium",
    n = 3819 + 2299 + 1129 + 1109 + 1116 + 714,
    lab = "BPC consortium",
    color = "#001524"
  ) |>
  add_row(
    name = "BPC public",
    n = 3819 + 2299, # CRC and lung
    lab = "BPC public",
    color = "#15616d"
  ) |>
  add_row(
    name = "Sponsored projects",
    n = 1000,
    lab = "Sponsored Projects",
    color = "#ff7d00"
  ) |>
  add_row(
    name = "Special projects",
    n = 500,
    lab = "Special Projects",
    color = "#78290f"
  )

readr::write_rds(dat, 'cohort_data.rds')
