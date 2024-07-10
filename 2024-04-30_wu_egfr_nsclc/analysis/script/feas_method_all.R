# Based on J&J worksheet.

library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)

dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_reg <- readr::read_csv(
  here('data-raw', 'regimen_cancer_level_dataset.csv')
)
dft_ecog <- readr::read_rds(
  here('data', 'ecog_imputed_not_missing.rds')
)

# Just going to do this up front for now.  It's too hard to figure out if
#   a second cancer is the metastasized one and such.
dft_cohort <- dft_ca_ind %>%
  group_by(record_id) %>%
  arrange(ca_seq) %>%
  slice(1) %>%
  ungroup(.)

dft_flow <- flow_record_helper(dft_cohort, "People with NSCLC")

dft_mut <- readr::read_rds(here('data', 'samples_with_mut.rds'))
dft_cohort %<>% filter(record_id %in% dft_mut$record_id)

dft_flow %<>% flow_record_helper(dft_cohort, "EGFR L858R or exon 19 inframe del (ever)", .)




dft_dmet <- get_dmet_time(dft_ca_ind, annotate_type = F) %>%
  mutate(dx_dmet_days = dx_dmet_yrs * 365.25) %>%
  select(-dx_dmet_yrs)
# Note:  At the time of this writing there are people who were stage IV with no 
#  mets in the data.  That's an error.  See them with annotate_type = T above.

dft_cohort <- dft_cohort %>%
  left_join(
    .,
    dft_dmet,
    by = c('record_id', 'ca_seq')
  )

dft_cohort %<>%
  mutate(
    dx_adv_or_met_days = case_when(
      stage_dx %in% c("Stage III", "Stage IV") ~ 0,
      !is.na(dx_dmet_days) ~ dx_dmet_days,
      T ~ NA_real_
    )
  )

dft_cohort %<>%
  filter(!is.na(dx_adv_or_met_days))

dft_flow %<>% flow_record_helper(dft_cohort, "Stage 3/4 dx, or met anytime", .)

# A small QC output for chelsea nichols and team (these cases are all errors): 
# dft_ca_ind %>% 
#   filter(stage_dx %in% "Stage IV", !str_detect(ca_dmets_yn, "Yes") | is.na(ca_dmets_yn)) %>% 
#   select(phase, cohort, record_id, ca_seq, stage_dx, ca_dmets_yn) %>% 
#   arrange(ca_dmets_yn) %>%
#   readr::write_csv(., file = here('data', 'nsclc_s4_no_mets.csv'))


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


dft_cohort %>% 
  select(record_id, ca_seq)






readr::write_rds(
  dft_flow,
  here('data', 'table_method_jj_all.rds')
)



# Add the timing of the FIRST osi regimen in.
dft_cohort <- dft_osi %>%
  select(
    record_id, ca_seq, regimen_number,
    dob_reg_start_days
  ) %>%
  arrange(regimen_number) %>%
  group_by(record_id, ca_seq) %>%
  slice(1) %>%
  ungroup(.) %>%
  left_join(
    dft_cohort,
    .,
    by = c('record_id', 'ca_seq'),
    relationship = "one-to-one"
  )

readr::write_rds(
  dft_cohort,
  here('data', 'final_dat_jj_all.rds')
)







  
    







