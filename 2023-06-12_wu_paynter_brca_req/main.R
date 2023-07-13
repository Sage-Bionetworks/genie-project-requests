# Description:  See https://sagebionetworks.jira.com/browse/GEN-768
#   Note:  This file is very inefficient computationally.  Lots of data frame
#   lists, lots of room for optimization.
# Author: Alex Paynter


synid_releases <- "syn7492881"
# output_location_synid <- "syn51317177" # in 'GENIE BioPharma Collaborative Internal' > requests

library(synapser)
library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(glue)
library(fs)
library(mskcc.oncotree)
library(hash)

purrr::walk(fs::dir_ls('R'), .f = source)

synLogin()

# Set up a hash table for the oncotree codes:
dft_oncotree <- mskcc.oncotree::get_tumor_types() %>%
  select(oncotree_code, tissue)

dft_releases <- get_syn_children_df(synid_releases)

# Recurse to subfolders:
dft_releases %<>%
  rename(major = name) %>%
  mutate(
    child_df = purrr::map(.x = id, .f = get_syn_children_df)
  ) %>%
  select(-c(id, createdOn)) %>%
  unnest(child_df) %>%
  rename(
    minor = name,
    minor_id = id,
    minor_createdOn = createdOn
  )

dft_releases <- dft_releases %>%
  mutate(
    df_file_ids = purrr::map(.x = minor_id, .f = add_select_file_ids)
  ) %>%
  unnest(df_file_ids)






syn_link_obj <- dft_releases %>% 
  slice(n()) %>% 
  glimpse
  pull(data_clinical_patient.txt) %>% 
  get_synapse_entity_txt

  
syn_link_obj_newest <- dft_releases %>%
  slice(n()) 

# Skip levels:
#   0 for clinical
#   4 for clinical_patient or clinical_sample
#   0 for mutation

test_clin_pt <- syn_link_obj_newest %>%
  pull(data_clinical_patient.txt) %>%
  get_synapse_entity_txt(., skip = 4)
test_clin_sample <- syn_link_obj_newest %>%
  pull(data_clinical_sample.txt) %>%
  get_synapse_entity_txt(., skip = 4)
test_clin_comb <- syn_link_obj_newest %>%
  pull(data_clinical.txt) %>%
  get_synapse_entity_txt(., skip = 0)
test_mut <- syn_link_obj_newest %>%
  pull(data_mutations_extended.txt) %>%
  get_synapse_entity_txt(., skip = 0)

test_clin_pt %>% glimpse
test_clin_sample %>% glimpse
test_clin_comb %>% glimpse
test_mut %>% head %>% glimpse

nam_pt <- test_clin_pt %>% names
nam_samp <- test_clin_sample %>% names
nam_comb <- test_clin_comb %>% names

# We can create the patient file from the combined one:

vec_clin_pt_vars <- c(
  "patient_id", "sex", 'primary_race', 'secondary_race', 'tertiary_race', 
  'ethnicity', 'birth_year', 'center', 'int_contact', 
  'int_dod', 'year_contact', 'dead', 'year_death'
)
created_clin_pt <- test_clin_comb %>%
  select(any_of(vec_clin_pt_vars)) %>%
  distinct(.)
waldo::compare(created_clin_pt, test_clin_pt)

vec_clin_samp_vars <- c(    
  'patient_id', 'sample_id', 'age_at_seq_report', 'oncotree_code',
  'sample_type', 'seq_assay_id', 'cancer_type', 'cancer_type_detailed',
  'age_at_seq_report_days', 'sample_type_detailed',
  'seq_year', 'sample_class'
)
# Likewise for the sample file:
created_clin_sample <- test_clin_comb %>%
  select(any_of(vec_clin_samp_vars)) %>%
  distinct(.)
waldo::compare(created_clin_sample, test_clin_sample)

# We can go the other way too:
created_clin_comb <- left_join(
  test_clin_sample,
  test_clin_pt,
  by = "patient_id"
)
waldo::compare(created_clin_comb, test_clin_comb)
setdiff(
  names(test_clin_comb),
  names(created_clin_comb)
)

summarize_release_wu_brca <- function(
  id_mut, 
  id_clin_pt,
  id_clin_sample, 
  id_clin_comb,
  dat_onco = dft_oncotree) {
  
  dat_mut <- get_synapse_entity_txt(id_mut, skip = 0)
  
  if (is.na(id_clin_comb)) {
    dat_pt <- get_synapse_entity_txt(id_clin_pt, skip = 4)
    dat_sample <- get_synapse_entity_txt(id_clin_sample, skip = 4)
    dat_clin <- left_join(
      
    )
  } else {
    
  }

  
}



synGet(syn_link_obj_newest)

# get returns a function that gets the value associated with a key.
synGet(syn_link_obj$properties$linksTo$get("targetId"))

syn_link_obj$properties %>% names

# example from prostate group:
syn_nolink_obj <- synGet('syn50908661')
syn_nolink_obj$properties %>% names





if (any(stringr::str_detect(df_clin_children$name, ".csv^"))) {
  warning("Non-CSV files unexpectedly contained in {synid_clin_data}.")
}

syn_store_in_dataraw <- function(sid) {
  synGet(entity = sid, downloadLocation = here("data-raw"))
}

purrr::walk(.x = df_clin_children$id, 
            .f = syn_store_in_dataraw)



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
