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
library(mskcc.oncotree) # the built in oncotree_primary_node was better,
# but we will use this if it's not populated.

purrr::walk(fs::dir_ls('R'), .f = source)

synLogin()

# Set up a table for the oncotree codes:
dft_oncotree <- mskcc.oncotree::get_tumor_types() %>%
  select(oncotree_code, tissue) %>%
  # do this to temporarily match the styling of oncotree_primary_node in some
  # of the data.
  mutate(
    tissue = toupper(tissue),
    tissue = str_replace_all(tissue, pattern = " ", replacement = "_")
  )

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


dft_releases_test <- dft_releases %>%
  # 15 is an important test case because it has no clinical file.
  slice(c(1, 15, 50, n()))

summarize_release_wu_brca <- function(
  id_mut, 
  id_clin_comb,
  id_clin_pt,
  id_clin_sample, 
  dat_onco = dft_oncotree) {
  
  dat_mut <- get_synapse_entity_txt(id_mut, skip = 0, cols_to_lower = T)
  
  if (is.na(id_clin_comb)) {
    dat_pt <- get_synapse_entity_txt(id_clin_pt, skip = 4)
    dat_sample <- get_synapse_entity_txt(id_clin_sample, skip = 4)
    dat_clin <- left_join(
      dat_sample,
      dat_pt,
      by = "patient_id"
    )
  } else {
    dat_clin <- get_synapse_entity_txt(id_clin_comb, skip = 0)
  }
  
  dat_mut %<>% 
    select(
      all_of(
        c('tumor_sample_barcode', 
          'hugo_symbol', 
          'mutation_status')
      )
    ) %>%
    mutate(mutation_status = tolower(mutation_status))
  
  dat_clin %<>%
    select(
      all_of(c('sample_id', 'patient_id', 'center', 'oncotree_code')),
      any_of(c('oncotree_primary_node'))
    ) 
  
  # Needed for some older releases:
  if (!("oncotree_primary_node" %in% names(dat_clin))) {
    dat_clin %<>%
      left_join(., dft_oncotree, by = "oncotree_code") %>%
      mutate(tissue = if_else(is.na(tissue), "UNKNOWN", tissue)) %>%
      rename(oncotree_primary_node = tissue)
  }
  
  dat_mut %<>%
    filter(hugo_symbol %in% c("BRCA1", "BRCA2")) %>%
    filter(mutation_status %in% "somatic") %>%
    mutate(variant = 1) %>%
    select(-c(hugo_symbol, mutation_status))
  
  dat_rtn <- left_join(
    dat_clin,
    dat_mut,
    by = c(sample_id = "tumor_sample_barcode")
  )
  
  dat_rtn %<>%
    mutate(variant = if_else(is.na(variant), 0, variant)) %>%
    group_by(center, oncotree_primary_node, patient_id) %>%
    summarize(variant = max(variant, na.rm = T), .groups = "drop") # %>%
    # group_by(center, oncotree_primary_node) %>%
    # summarize(
    #   n_total = n(),
    #   n_variant = sum(variant),
    #   .groups = "drop"
    # )
  
  
  
  # Check that oncotree_primary_node does not exist when oncotree code is missing.
  return(dat_rtn)
  
}

# Example of doing one:
summarize_release_wu_brca(
  id_mut = pull(slice(dft_releases_test, 1), data_mutations_extended.txt),
  id_clin_comb = pull(slice(dft_releases_test, 1), data_clinical.txt),
  id_clin_sample = NA,
  id_clin_pt = NA
)

# Doing them all:
dft_rel_sums <- dft_releases_test %>%
  mutate(
    release_sum = purrr::pmap(
      .l = list(
        id_mut = data_mutations_extended.txt,
        id_clin_comb = data_clinical.txt,
        id_clin_sample = data_clinical_sample.txt,
        id_clin_pt = data_clinical_patient.txt
      ),
      .f = summarize_release_wu_brca
    )
  )

dft_rel_sums %<>% unnest(release_sum)

# At this point you can save it as an RDA and move into another script.
