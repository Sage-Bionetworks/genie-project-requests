
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)


# Highly manual process.

dft_age_vars <- tribble(
    ~var, ~unit,
    "age_death_yrs", 'year',
    "age_dx", 'year',
    "age_last_fu_yrs", 'year',
    
    "dob_ca_dx_days", 'day', 
    "dob_ca_dx_mos", 'month',
    "dob_ca_dx_yrs", 'year',
    "dob_cpt_report_days", 'day',
    "dob_cpt_report_mos", 'month',
    "dob_cpt_report_yrs", 'year',
    
    "dob_lastalive_int_mos", 'month',
    "dob_lastalive_int_yrs", 'year',
    
    "dob_next_ca_days", 'day',
    "dob_next_ca_mos", 'month',
    "dob_next_ca_yrs", 'year',
    
    
    "drugs_startdt_int_1", 'day',
    "drugs_startdt_int_2", 'day',
    "drugs_startdt_int_3", 'day',
    "drugs_startdt_int_4", 'day',
    "drugs_startdt_int_5", 'day',
    
    "dx_drug_end_int_1", 'day',
    "dx_drug_end_int_2", 'day',
    "dx_drug_end_int_3", 'day',
    "dx_drug_end_int_4", 'day',
    "dx_drug_end_int_5", 'day',
    
    "dx_drug_end_int_mos_1", 'month',
    "dx_drug_end_int_mos_2", 'month',
    "dx_drug_end_int_mos_3", 'month',
    "dx_drug_end_int_mos_4", 'month',
    "dx_drug_end_int_mos_5", 'month',
    
    
    "drugs_startdt_int_1", 'day',
    "drugs_startdt_int_2", 'day',
    "drugs_startdt_int_3", 'day', 
    "drugs_startdt_int_4", 'day',
    "drugs_startdt_int_5", 'day',
    
    "dx_cpt_rep_days", 'day',
    "dx_cpt_rep_mos", 'month',
    "dx_cpt_rep_yrs", 'year', 
    
    "dx_drug_end_int_1", 'day',
    "dx_drug_end_int_2", 'day',
    "dx_drug_end_int_3", 'day',  
    "dx_drug_end_int_4", 'day', 
    "dx_drug_end_int_5", 'day', 
    
    "dx_drug_end_int_mos_1", 'month',
    "dx_drug_end_int_mos_2", 'month',
    "dx_drug_end_int_mos_3", 'month',
    "dx_drug_end_int_mos_4", 'month',
    "dx_drug_end_int_mos_5", 'month',
    
    "dx_drug_end_or_lastadm_int_1", 'day', 
    "dx_drug_end_or_lastadm_int_2", 'day',
    "dx_drug_end_or_lastadm_int_3", 'day', 
    "dx_drug_end_or_lastadm_int_4", 'day', 
    "dx_drug_end_or_lastadm_int_5", 'day', 
    
    "dx_drug_start_int_1", 'day', 
    "dx_drug_start_int_2", 'day', 
    "dx_drug_start_int_3", 'day', 
    "dx_drug_start_int_4", 'day', 
    "dx_drug_start_int_5", 'day', 
    
    "dx_drug_start_int_mos_1", 'month',
    "dx_drug_start_int_mos_2", 'month',
    "dx_drug_start_int_mos_3", 'month',
    "dx_drug_start_int_mos_4", 'month',
    "dx_drug_start_int_mos_5", 'month',
    
    "dx_md_visit_days", 'day',
    "dx_md_visit_mos", 'month',
    "dx_md_visit_yrs", 'year',
    
    "dx_path_proc_cpt_days", 'day',
    "dx_path_proc_cpt_mos", 'month',
    "dx_path_proc_cpt_yrs", 'year',
    
    "dx_path_proc_days", 'day',
    "dx_path_proc_mos", 'month',
    "dx_path_proc_yrs", 'year',
    
    "dx_ref_scan_days", 'day',
    "dx_ref_scan_mos", 'month',
    "dx_ref_scan_yrs", 'year',
    
    'dx_reg_end_all_int', 'day',
    "dx_reg_end_all_int_mos", 'month', 
    "dx_reg_end_all_int_yrs", 'year',
   
    'dx_reg_end_any_int', 'day', 
    "dx_reg_end_any_int_mos", 'month',
    "dx_reg_end_any_int_yrs", 'year',
    
    'dx_reg_start_int', 'day',
    "dx_reg_start_int_mos", 'month',
    "dx_reg_start_int_yrs", 'year',
    
    "dx_scan_days", 'day',
    "dx_scan_mos", 'month',
    "dx_scan_yrs", 'year',
    
    "dx_tm_days", 'day',
    "dx_tm_mos", 'month',
    "dx_tm_yrs", 'year',
    
    "dx_to_dmets_abdomen_days", 'day',
    "dx_to_dmets_abdomen_mos", 'month',
    "dx_to_dmets_abdomen_yrs", 'year',
    
    "dx_to_dmets_bone_days", 'day',
    "dx_to_dmets_bone_mos", 'month',
    "dx_to_dmets_bone_yrs", 'year',
    
    "dx_to_dmets_brain_days", 'day',
    "dx_to_dmets_brain_mos", 'month',
    "dx_to_dmets_brain_yrs", 'year',
    
    "dx_to_dmets_breast_days", 'day',
    "dx_to_dmets_breast_mos", 'month',
    "dx_to_dmets_breast_yrs", 'year',
    
    "dx_to_dmets_days", 'day',
    "dx_to_dmets_mos", 'month',
    "dx_to_dmets_yrs", 'year',
    
    "dx_to_dmets_extremity_days", 'day',
    "dx_to_dmets_extremity_mos", 'month',
    "dx_to_dmets_extremity_yrs", 'year',
    
    "dx_to_dmets_head_neck_days", 'day',
    "dx_to_dmets_head_neck_mos", 'month',
    "dx_to_dmets_head_neck_yrs", 'year',
    
    "dx_to_dmets_liver_days", 'day',
    "dx_to_dmets_liver_mos", 'month',
    "dx_to_dmets_liver_yrs", 'year',
    
    "dx_to_dmets_pelvis_days", 'day',
    "dx_to_dmets_pelvis_mos", 'month',
    "dx_to_dmets_pelvis_yrs", 'year',
    
    "dx_to_dmets_thorax_days", 'day',
    "dx_to_dmets_thorax_mos", 'month',
    "dx_to_dmets_thorax_yrs", 'year',
    
    "first_index_ca_days", 'day',
    "first_index_ca_mos", 'month',
    "first_index_ca_yrs", 'year',
    
    "path_proc_cpt_rep_days", 'day',
    "path_proc_cpt_rep_mos", 'month',
    "path_proc_cpt_rep_yrs", 'year',
    
    "tt_os_adv_days", 'day',
    "tt_os_adv_mos", 'month',
    "tt_os_adv_yrs", 'year',
    
    "tt_os_d1_days", 'day',
    "tt_os_d1_mos", 'month',
    "tt_os_d1_yrs", 'year',
    
    "tt_os_d2_days", 'day',
    "tt_os_d2_mos", 'month',
    "tt_os_d2_yrs", 'year',
    
    "tt_os_d3_days", 'day',
    "tt_os_d3_mos", 'month',
    "tt_os_d3_yrs", 'year',
    
    "tt_os_d4_days", 'day',
    "tt_os_d4_mos", 'month',
    "tt_os_d4_yrs", 'year',
    
    "tt_os_d5_days", 'day',
    "tt_os_d5_mos", 'month',
    "tt_os_d5_yrs", 'year',
    
    "tt_os_dx_days", 'day',
    "tt_os_dx_mos", 'month',
    "tt_os_dx_yrs", 'year',
    
    "tt_os_g_days", 'day',
    "tt_os_g_mos", 'month',
    "tt_os_g_yrs", 'year',
    "tt_pfs_i_adv_days", 'day',
    "tt_pfs_i_adv_mos", 'month',
    
    "tt_pfs_i_adv_yrs", 'year',
    "tt_pfs_i_and_m_adv_days", 'day',
    "tt_pfs_i_and_m_adv_mos", 'month',
    "tt_pfs_i_and_m_adv_yrs", 'year',
    
    "tt_pfs_i_and_m_g_days", 'day',
    "tt_pfs_i_and_m_g_mos", 'month',
    "tt_pfs_i_and_m_g_yrs", 'year',
    
    "tt_pfs_i_g_days", 'day',
    "tt_pfs_i_g_mos", 'month',
    "tt_pfs_i_g_yrs", 'year',
    
    "tt_pfs_i_or_m_adv_days", 'day',
    "tt_pfs_i_or_m_adv_mos", 'month',
    "tt_pfs_i_or_m_adv_yrs", 'year',
    
    "tt_pfs_i_or_m_g_days", 'day',
    "tt_pfs_i_or_m_g_mos", 'month',
    "tt_pfs_i_or_m_g_yrs", 'year',
    
    "tt_pfs_m_adv_days", 'day',
    "tt_pfs_m_adv_mos", 'month',
    "tt_pfs_m_adv_yrs", 'year',
    
    "tt_pfs_m_g_days", 'day',
    "tt_pfs_m_g_mos", 'month',
    "tt_pfs_m_g_yrs", 'year',
    
    "ttnt_any_ca_days", 'day',
    "ttnt_any_ca_mos", 'month',
    "ttnt_any_ca_yrs", 'year',
    
    "ttnt_ca_seq_days", 'day',
    "ttnt_ca_seq_mos", 'month',
    "ttnt_ca_seq_yrs", 'year',
    
    
    # A few noticed later on:
    'last_alive_int', 'day',
    'last_anyvisit_int', 'day',
    'last_oncvisit_int', 'day',
    'enroll_hospice_int', 'day',
    'hybrid_death_int', 'day',
    
    'dob_lastalive_int', 'day',
    'dob_lastalive_int_mos', 'month',
    'dob_lastalive_int_yrs', 'year',
    
    'image_ref_scan_int', 'day',
    'image_scan_int', 'day',
    
    'md_onc_visit_int', 'day',
    'naaccr_first_contact_int', 'day',
    'path_proc_int', 'day',
    'tm_spec_collect_int', 'day',
    'cpt_order_int', 'day'
)


if (
    (dft_age_vars %>%
    filter(!unit %in% c('day', 'month', 'year')) %>%
    nrow) > 0
) {
    cli::cli_abort("typo in some units - or some missing units")
}

readr::write_rds(
    dft_age_vars,
    here('data', 'age_vars.rds')
)

# To watch:
# cpt_seq_date
