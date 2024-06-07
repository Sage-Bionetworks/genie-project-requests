# Based on Jen Hoppe email from May 13.


library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)


dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_reg <- readr::read_csv(
  here('data-raw', 'regimen_cancer_level_dataset.csv')
)

dft_flow <- flow_record_helper(dft_ca_ind, "People with NSCLC")


dft_cohort <- dft_ca_ind

dft_mut <- readr::read_rds(here('data', 'samples_with_mut.rds'))
dft_cohort %<>% filter(record_id %in% dft_mut$record_id)

dft_flow %<>% flow_record_helper(dft_cohort, "EGFR L858R or exon 19 inframe del (ever)", .)


dft_cohort %<>%
  filter(stage_dx %in% c("Stage I", "Stage II", "Stage III", "Stage I-III NOS"))
dft_flow %<>% flow_record_helper(dft_cohort, "Stage I-III at diagnosis", .)




dft_osi <- dft_reg %>% 
  filter(str_detect(regimen_drugs, "Osimertinib")) %>%
  select(
    record_id, ca_seq, regimen_number,
    regimen_drugs, 
    dob_reg_start_days = drugs_startdt_int_1,
    dx_reg_start_int
  )

dft_cohort <- dft_osi %>% 
  group_by(record_id, ca_seq) %>%
  summarize(osi_ever = T, .groups = "drop") %>%
  left_join(dft_cohort, ., by = c("record_id", "ca_seq")) %>%
  filter(osi_ever)

dft_flow %<>% flow_record_helper(dft_cohort, "Cases with Osimertinib ever", .)




# Addon after reading Lev's email

# Last cpt for each  person
dft_last_cpt <- dft_cpt %>%
  group_by(record_id, ca_seq) %>%
  arrange(desc(cpt_number)) %>%
  slice(1) %>%
  ungroup(.) %>%
  select(record_id, ca_seq, dob_cpt_report_days)

dft_ngs_after_osi <- dft_osi %>%
  group_by(record_id, ca_seq) %>%
  arrange(regimen_number) %>%
  slice(1) %>%
  ungroup(.) %>%
  left_join(
    select(dft_cohort, record_id, ca_seq),
    .,
    by = c('record_id', 'ca_seq')
  ) %>%
  left_join(
    .,
    dft_last_cpt,
    by = c('record_id', 'ca_seq')
  ) %>%
  mutate(
    any_cpt_after_osi = dob_cpt_report_days > dob_reg_start_days
  ) 

dft_cohort %<>% 
  left_join(
    ., select(dft_ngs_after_osi, record_id, ca_seq, any_cpt_after_osi),
    by = c("record_id", "ca_seq")
  ) %>%
  filter(any_cpt_after_osi)

dft_flow %<>% flow_record_helper(dft_cohort, "Cases with an NGS test after Osi use", .)







readr::write_rds(
  dft_flow,
  here('data', 'table_method_broad.rds')
)

readr::write_rds(
  dft_cohort,
  here('data', 'final_dat_broad_method.rds')
)







  
    







