
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)


dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_path <- readr::read_csv(
  here('data-raw', 'pathology_report_level_dataset.csv')
)

dft_flow <- flow_record_helper(dft_ca_ind, "Raw data")



dft_cohort <- dft_ca_ind %>% 
  filter(stage_dx %in% c("Stage I", "Stage II", "Stage III", "Stage I-III NOS"))

dft_flow %<>% flow_record_helper(dft_cohort, "Stage I-III at diagnosis", .)





dft_cohort %<>% filter_stage_messy(.) # separate function

dft_flow %<>% flow_record_helper(dft_cohort, "Remove <1B, >3A (several vars)", .)





dft_cohort 




