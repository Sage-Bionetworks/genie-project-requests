library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_med_onc <- readr::read_csv(
  here('data-raw', 'med_onc_note_level_dataset.csv')
)

dft_med_onc %>%
  augment_med_onc_imputed_ecog(.) %>%
  filter(md_ecog_imputed != "Not documented in note") %>%
  group_by(record_id)