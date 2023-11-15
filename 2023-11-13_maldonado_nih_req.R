# Description:  Starter code for Alberto Maldonado's project, which seeks to test the frequency of particiants with multiple gene alterations.
# Author:  Alex Paynter

# We will use the synapse R client 'synapser' to download the data.
# Useful resources on synapser (install, use, etc) include:
# - https://github.com/Sage-Bionetworks/synapser
# - https://r-docs.synapse.org/
# If you don't want to use synapser this project could absolutely be 
# completed by downloading the data and loading it into memory from 
# a file.  The commands below would need changes.

library(synapser)
library(dplyr)
library(magrittr)
library(gtsummary)

# Defining a convenience function for loading synapse txt entities into memory.
get_synapse_entity_txt <- function(
  synapse_id, 
  version = NA,
  skip = 0,
  cols_to_lower = T
) {
  if (is.na(version)) {
    entity <- synGet(synapse_id, followLink = T)
  } else {
    entity <- synGet(synapse_id, followLink = T, version = version)
  }
  data <- read_tsv(
    file = entity$path,
    show_col_types = F,
    progress = F,
    skip = skip,
    comment = "#"
  )
  if (cols_to_lower) {
    data <- rename_all(data, tolower)
  }
  return(data)
}

synLogin()

# The appropriate synapse entity IDs can be found by looking at
#   Synapse in a web browser.  
dft_maf <- get_synapse_entity_txt('syn9734426') # data_mutations_extended.txt

# As a proof of concept we will just do this 
dft_kras_g12d <- dft_maf %>% 
  filter(hugo_symbol %in% "KRAS") %>%
  filter(hgvsp_short %in% "p.G12D")

# I'm just going to keep the columns we need:  sample_id and an indicator.
# You can keep other information.
dft_kras_g12d %<>% 
  mutate(has_kras_g12d = "Yes") %>%
  select(tumor_sample_barcode, has_kras_g12d)

dft_clin <- get_synapse_entity_txt('syn7392892') #data_clinical.txt

dft_clin_prad <- dft_clin %>%
  filter(oncotree_code %in% "PRAD")
# Any other restrictions?  Primary tumor sample?  Metastatic?

dft_clin_prad %<>% 
  select(sample_id, patient_id, sex, primary_race, ethnicity) %>%
  left_join(
    .,
    dft_kras_g12d,
    by = c(sample_id = "tumor_sample_barcode")
  ) %>%
  mutate(
    has_kras_g12d = if_else(is.na(has_kras_g12d), "No", has_kras_g12d),
    has_kras_g12d = factor(has_kras_g12d)
  )

dft_clin_prad %>%
  arrange(desc(has_kras_g12d)) %>%
  group_by(patient_id) %>%
  slice(1) %>%
  
  
gtsummary::tbl_summary(
  select(dft_clin_prad, ethnicity, has_kras_g12d),
  by = "ethnicity"
)

  






