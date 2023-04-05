 # Description:  Request from Jennifer Hoppe, on behalf of Jeremy Warner and 
#    Shawn Sweeney.  Create a list of all drugs used in the BPC projects using 
#    the regimen datasets.  For each drug used, state the BPC cohort it was observed 
#    in and whether it was observed as a regimen for a non-index or index cancer.   
#    Use the latest release available, whether public or consortium, in each case.  
#    The desired output format is excel.
# Author: Alex Paynter

library(synapser)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(glue)

synLogin()

# slightly altered to get tibbles. 
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA) {
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read_csv(entity$path,
                   show_col_types = F,
                   progress = F)
  return(data)
}

# The synapse ID here should be the folder containing csv data, such as
#   cancer_level_dataset_index.csv
merge_one_folder <- function(synid_fold) {
  df_clin_children <- synGetChildren(synid_fold) %>%
    as.list %>%
    purrr::map_dfr(.x = .,
                   .f = as_tibble)
  
  
  syn_ca_index <- df_clin_children %>%
    filter(name %in% "cancer_level_dataset_index.csv") %>%
    pull(id)
  syn_ca_non_index <- df_clin_children %>%
    filter(name %in% "cancer_level_dataset_non_index.csv") %>% 
    pull(id)
  syn_reg <- df_clin_children %>%
    filter(name %in% "regimen_cancer_level_dataset.csv") %>% 
    pull(id)
  
  
  dft_ca_index <- get_synapse_entity_data_in_csv(syn_ca_index)
  dft_ca_non_index <- get_synapse_entity_data_in_csv(syn_ca_non_index)
  dft_regimen <- get_synapse_entity_data_in_csv(syn_reg)
  
  # A few messages just to get the rough idea:
  message(glue("Cohort: {first(dft_ca_index$cohort)}"))
  message(glue("Index dataset read in with {nrow(dft_ca_index)} rows."))
  message(glue("Non-index dataset read in with {nrow(dft_ca_non_index)} rows."))
  message(glue("Regimen dataset read in with {nrow(dft_regimen)} rows."))
  
  
  ca_comb <- dplyr::bind_rows(
    (dft_ca_index %>% 
       select(record_id, ca_seq) %>%
       mutate(index_ca = T)),
    (dft_ca_non_index %>%
       select(record_id, ca_seq) %>%
       mutate(index_ca = F))
  )
  
  dft_reg_aug <- dft_regimen %>%
    left_join(., ca_comb, by = c("record_id", "ca_seq"))
  
  # for now we just want a list of the drugs used:
  dft_reg_aug %<>% 
    select(cohort, record_id, ca_seq, index_ca,
           contains("drugs_drug")) %>%
    pivot_longer(cols = -c(cohort, record_id, ca_seq, index_ca),
                 names_to = "drug_num",
                 values_to = "drug_name") %>%
    mutate(
      drug_num = stringr::str_replace(drug_num,
                                      "drugs_drug_",
                                      ""),
      drug_num = readr::parse_number(drug_num)
    ) 
  
  dft_reg_aug %<>%
    filter(!is.na(drug_name)) %>%
    # first get a 0/1 for each person:
    group_by(drug_name, cohort, index_ca, record_id) %>%
    summarize(obs = 1, .groups = "drop") %>%
    # now summarize over all participants.  cohort and index_ca are constant, so 
    #   "grouping by" is just a way to propagate them.
    group_by(drug_name, cohort, index_ca) %>%
    summarize(n = n(), .groups = "drop") %>%
  
  return(dft_reg_aug)
}

# a interation dataframe where each row specifies a cohort and synid for the folder.
dft_folders <- tibble::tribble(
  ~cohort, ~synid,
  "BLADDER", "syn28495599", # 1.1 consortium
  "BrCa", "syn39802381", # 1.2 consortium
  "CRC", "syn39802279", # 2.0 public
  "NSCLC", "syn27245047", # 2.0 public 
  "PANC", "syn50612197", # 1.2 consortium
  "Prostate", "syn50612196", # 1.2 consortium
  # cohorts that had no available releases at the time:
  #   CRC2, ESOPHAGO, MELANOMA, NSCLC2, OVARIAN, RENAL
)
# If you want to try one just do something like:
# merge_one_folder(dft_folders[["synid"]][6])

# We'll do them all at once:
dft_cohort_comb <- dft_folders %>%
  mutate(
    dat = purrr::pmap(
      .f = merge_one_folder,
      .l = list(synid_fold = synid)
    )
  ) %>%
  pull(dat) %>%
  dplyr::bind_rows(.)

# At this point we have counts of subjects, need yes/no observed to meet goals.
dft_cohort_comb %<>%
  mutate(index_ca = if_else(index_ca, "index_n", "nonindex_n")) %>%
  pivot_wider(
    names_from = "index_ca",
    values_from = "n") %>%
  mutate(
    obs_index = if_else(index_n >= 1, 1, 0, 0),
    obs_nonindex = if_else(nonindex_n >= 1, 1, 0, 0)
  ) 

# order it in the way that seems most helpful:
dft_cohort_comb %<>%
  select(drug_name, cohort, obs_index, obs_nonindex) %>%
  arrange(drug_name, cohort)


# Todo:  
# create synapse folder for output
# output as an excel file
# check with what Jen produced if possible.


  
# tests:
# merge_one_folder("syn50612196"),
# test <- get_synapse_entity_data_in_csv('syn50908661')
  
  