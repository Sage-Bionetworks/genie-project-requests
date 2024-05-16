
library(purrr); library(fs); library(here)
purrr::walk(.x = fs::dir_ls('R'), .f = source)


dft_ca_ind <- readr::read_csv(
  here('data-raw', 'cancer_level_dataset_index.csv')
)
dft_path <- readr::read_csv(
  here('data-raw', 'pathology_report_level_dataset.csv')
)
dft_reg <- readr::read_csv(
  here('data-raw', 'regimen_cancer_level_dataset.csv')
)

dft_flow <- flow_record_helper(dft_ca_ind, "Raw data")



dft_cohort <- dft_ca_ind %>% 
  filter(stage_dx %in% c("Stage I", "Stage II", "Stage III", "Stage I-III NOS"))

dft_flow %<>% flow_record_helper(dft_cohort, "Stage I-III at diagnosis", .)





dft_cohort %<>% filter_stage_light(.) # separate function

dft_flow %<>% flow_record_helper(dft_cohort, "Exclude those confirmed to be <1B or >3A at dx", .)













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







# This step does no filtering now - just to get var names included.
dft_osi_mod <- dft_osi %>%
  group_by(record_id, ca_seq) %>%
  summarize(
    first_regimen_number = first(regimen_number),
    .groups = "drop"
  )

dft_cohort %<>%
  left_join(., dft_osi_mod, by = c("record_id", "ca_seq")) 








# Join the appropriate regimen data in from dft_osi to get progression data
dft_cohort <- left_join(
  dft_cohort,
  dft_osi,
  by = c("record_id", "ca_seq",
         first_regimen_number = "regimen_number"),
  relationship = "one-to-one"
)


# Pull in the set of progression events:
dft_prog <- readr::read_rds(
  here('data', 'prog_events.rds')
)

dft_prog_times <- dft_cohort %>% 
  select(record_id, ca_seq, first_regimen_number, dob_reg_start_days) %>%
  left_join(., dft_prog, by = "record_id", relationship = 'many-to-many') %>%
  # only consider progressions AFTER (not on) the regimen in question.
  filter((dob_prog_days - dob_reg_start_days) > 0.5)  

dft_prog_times %<>%
  mutate(
    cumsum_i = sum(prog_i),
    cumsum_m = sum(prog_m)
  ) %>%
  group_by(record_id, ca_seq, first_regimen_number) %>%
  summarize(
    prog_i_or_m_status = sum(prog_i) > 0 | sum(prog_m) > 0,
    prog_i_and_m_status = sum(prog_i) > 0 & sum(prog_m) > 0,
    # This part is not needed for the data request but it's right here...
    dob_prog_i_or_m_days = first(dob_prog_days[cumsum_i > 0 | cumsum_m > 0]),
    dob_prog_i_and_m_days = first(dob_prog_days[cumsum_i > 0 & cumsum_m > 0]),
    .groups = "drop"
  )

dft_cohort <- left_join(
  dft_cohort,
  dft_prog_times,
  by = c('record_id', 'ca_seq', 'first_regimen_number')
)
    
dft_cohort %<>% 
  replace_na(
    ., 
    replace = list(
      prog_i_or_m_status = F,
      prog_i_and_m_status = F
    )
  )

dft_cohort %<>% filter(prog_i_or_m_status)
dft_flow %<>% flow_record_helper(dft_cohort, "Progressed (I or M) after adjuvant osi", .) 





dft_cohort <- dft_cohort %>% 
  select(record_id, ca_seq, dob_prog_i_or_m_days) %>%
  left_join(
    .,
    dft_reg,
    by = c("record_id", "ca_seq")
  ) %>%
  group_by(record_id, ca_seq) %>%
  summarize(
    any_after_prog = any(drugs_startdt_int_1 >= dob_prog_i_or_m_days),
    drugs_used_after = first(regimen_drugs[drugs_startdt_int_1 >= dob_prog_i_or_m_days]),
    .groups = "drop"
  )

dft_cohort %<>% filter(any_after_prog)
dft_flow %<>% flow_record_helper(dft_cohort, "One or more regimens after progression", .) 

readr::write_rds(
  dft_flow,
  here('data', 'table_method_2.rds')
)







  
    







