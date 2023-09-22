# Description:  Pull the clinical data and save it.  Simple.
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
library(here)

purrr::walk(fs::dir_ls(here('R')), .f = source)

synLogin()

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

# For this we only need a recent release - any recent one will probably do.
incl_releases <- c("14.1-consortium")
dft_releases %<>%
  filter(minor %in% incl_releases)

test_clin <- get_synapse_entity_txt(
  pull(slice(dft_releases, 1), data_clinical.txt), 
  skip = 0
)


# Example of doing one:
# summarize_release_wu_brca(
#   id_mut = pull(slice(dft_releases_test, 1), data_mutations_extended.txt),
#   id_clin_comb = pull(slice(dft_releases_test, 1), data_clinical.txt),
#   id_clin_sample = NA,
#   id_clin_pt = NA
# )

# Doing them all:
dft_rel_sums <- dft_releases %>%
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

# Just checking to see if this is more efficient (probably)
readr::write_rds(
  x = dft_rel_sums,
  file = here('data', 'releases_by_pt_nested.rds')
)

toc()

# This would be a reasonable next step, but it's way less efficient to store this way:
# dft_rel_sums %<>% unnest(release_sum)




