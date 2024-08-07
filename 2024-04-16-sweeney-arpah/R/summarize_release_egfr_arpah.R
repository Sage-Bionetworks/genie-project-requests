
summarize_release_egfr_arpah <- function(
        id_mut, 
        id_clin_sample, 
        protein_symbols,
        samp_oncotree_codes) {
    
    protein_regex <- paste(paste0("^p.",  protein_symbols, "$"), collapse = "|")
    
    dat_mut <- get_synapse_entity_txt(id_mut, skip = 0, cols_to_lower = T)
    
    if (is.na(id_clin_sample)) {
        
    } else {
        # dat_clin <- get_synapse_entity_txt(id_clin_comb, skip = 0)
        dat_sample <- get_synapse_entity_txt(id_clin_sample, skip = 4)
        
    }
    
    dat_mut %<>%
        select(
            all_of(
                c('tumor_sample_barcode',
                  'consequence',
                  'hugo_symbol',
                  'hgvsp_short',
                  'hgvsc',
                  'mutation_status')
            )
        ) %>%
        mutate(mutation_status = tolower(mutation_status))
    
    dat_mut %<>%
        filter(hugo_symbol %in% "EGFR") %>%
        # select only non-synonymous variants:
        filter(!(consequence %in% "synonymous_variant")) %>%
        filter(str_detect(hgvsp_short, protein_regex))
    
    # This is quite odd, but some people have more than one row for the same
    #  protein change.  We can filter these out:
    dat_mut %<>%
        group_by(tumor_sample_barcode, hugo_symbol, hgvsp_short) %>%
        slice(1) %>%
        ungroup(.)
    
    dat_sample %<>%
        filter(oncotree_code %in% samp_oncotree_codes) %>%
        filter(sample_type %in% "Metastasis") %>%
        select(patient_id, sample_id, oncotree_code, sample_type, cancer_type_detailed)
    
    dat_rtn <- dat_mut %>%
        rename(sample_id = tumor_sample_barcode) %>%
        filter(sample_id %in% dat_sample$sample_id) 
    
    dat_rtn <- left_join(
        dat_rtn,
        dat_sample,
        by = "sample_id"
    )
    
    dat_rtn %<>%
        mutate(
            institution = map_chr(
                .x = str_split(sample_id, pattern = "-"), 
                .f = (\(x) x[2])
            )
        ) 
    
    dat_rtn %<>%
        select(
            # primary keys of return:
            sample_id, hgvsp_short,
            patient_id, hgvsp_short, institution,
            everything()
        )
        
    # Ready for a hideous hack?
    # Every iteration I clear out the synapseCache so I don't run out of memory on
    #   the AWS instance.  This deletes all the cache for other projects, which
    #   I don't have a problem with because I usually save whatever data I need.
    # fs::dir_delete("~/.synapseCache")
    
    return(dat_rtn)

}
