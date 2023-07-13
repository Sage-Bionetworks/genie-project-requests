# Description:  See https://sagebionetworks.jira.com/browse/GEN-768
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

synLogin()

get_syn_children_df <- function(sid) {
  synGetChildren(sid) %>%
    as.list %>%
    purrr::map_dfr(
      .x = .,
      .f = as_tibble
    ) %>%
    select(name, id, createdOn)
}

dft_releases <- get_syn_children_df(synid_releases)

# Recurse to expand for the children:
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
    

dft_releases %>% slice(1) %>% pull(id) %>% get_syn_children_df

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
