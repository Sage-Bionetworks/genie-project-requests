output_prca_synid <- "syn52060653" 

library(synapser)
library(magrittr)
library(here)

synLogin()
synapser::File(here("scripts", "wu-brca-main-genie-request.html"),
               parent = output_prca_synid) %>%
  synStore()
