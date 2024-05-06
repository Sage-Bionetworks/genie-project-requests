
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





dft_cohort %<>% filter_stage_heavy(.) # separate function

dft_flow %<>% flow_record_helper(dft_cohort, "Confirmed 1B-3A at dx", .)


dft_osi <- dft_reg %>% 
  filter(str_detect(regimen_drugs, "Osimertinib")) %>%
  select(
    record_id, ca_seq, regimen_number,
    regimen_drugs, 
    dx_reg_start_int,
    dx_reg_end_any_int,
    pfs_i_or_m_g_status,
    tt_pfs_i_or_m_g_days,
    pfs_i_and_m_g_status,
    tt_pfs_i_and_m_g_days
  )

dft_cohort <- dft_osi %>% 
  group_by(record_id, ca_seq) %>%
  summarize(osi_ever = T, .groups = "drop") %>%
  left_join(dft_cohort, ., by = c("record_id", "ca_seq")) %>%
  filter(osi_ever)

dft_flow %<>% flow_record_helper(dft_cohort, "Cases with Osimertinib ever", .)



# This is record/ca_seq keyed even though it derives from the path dataset.
dft_exc <- readr::read_rds(here('data', 'first_excision_timing.rds'))

dft_osi_adj <- dft_exc %>%
  select(record_id, ca_seq, dx_fexc_days) %>%
  left_join(., dft_osi, by = c("record_id", "ca_seq")) %>%
  filter(!is.na(regimen_drugs))

dft_osi_adj %<>%
  mutate(drug_adjuvant = dx_reg_start_int - dx_fexc_days > -0.5) %>%
  group_by(record_id, ca_seq) %>% # summarizing over drug uses now.
  summarize(
    any_drug_adjuvant = any(drug_adjuvant, na.rm = T),
    first_adj_regimen_number = first(regimen_number[drug_adjuvant]),
    .groups = "drop"
  )

# Now merge that into the cohort data.
dft_cohort %<>%
  left_join(., dft_osi_adj, by = c("record_id", "ca_seq")) %>%
  filter(any_drug_adjuvant)
dft_flow %<>% flow_record_helper(dft_cohort, "Osi in adjuvant (confirmed with excision record)", .)



# Join the appropriate regimen data in from dft_osi to get progression data
dft_cohort <- left_join(
  dft_cohort,
  dft_osi,
  by = c("record_id", "ca_seq",
         first_adj_regimen_number = "regimen_number"),
  relationship = "one-to-one"
)

dft_cohort %>% count(pfs_i_or_m_g_status) # oh right. this is useless.  


  
    







