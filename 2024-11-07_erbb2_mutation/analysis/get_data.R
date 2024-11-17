library(synapser)
library(here)

synLogin()

get_synapse_entity_data_in_tsv <- function(synapse_id, 
                                           version = NA) {
    if (is.na(version)) {
        entity <- synGet(synapse_id)
    } else {
        entity <- synGet(synapse_id, version = version)
    }
    
    data <- readr::read_tsv(entity$path,
                            show_col_types = F,
                            progress = F)
    return(data)
}

mut <- get_synapse_entity_data_in_tsv(
    synapse_id = 'syn5571527'
)

clin <- get_synapse_entity_data_in_tsv(
    synapse_id = 'syn7392892',
)

readr::write_rds(mut, here('data', 'mut.rds'))
readr::write_rds(clin, here('data', 'clin.rds'))
    
