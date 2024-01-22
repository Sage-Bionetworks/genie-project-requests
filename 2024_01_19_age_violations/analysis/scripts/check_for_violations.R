library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_vio_cond <- readr::read_rds(
    here('data', 'vio_cond.rds')
)

dft_all <- readr::read_rds(
    here('data', 'all_dat.rds')
)


test_bpc <- dft_all %>% slice(1) %>% pull(dat) %>% `[[`(.,1)

# function demos:

# find_violations(
#     test_bpc,
#     dft_vio_cond,
#     var_to_check = "dob_ca_dx_days"
# ) 

# find_all_violations(
#     test_bpc,
#     dft_vio_cond,
# ) 

dft_all %<>%
    mutate(
        viol = purrr::map(
            .x = dat,
            .f = \(x) {
                find_all_violations(
                    bpc_dat = x,
                    vio_dat = dft_vio_cond
                )
            }
        )
    )

dft_viol <- dft_all %>%
    select(cohort, dat_name, viol) %>%
    unnest(viol) 

dft_viol %>% View(.)

dft_viol %>% count(cohort, dat_name, violation_type)

dft_viol %>% count(cohort, dat_name, var, violation_type)




# Special case:  Birth date.

dft_all %>%
    filter(dat_name %in% "pt") %>%
    mutate(
        bd_lt_1935 = purrr::map(
            .x = dat,
            .f = \(x) {
                select(x, record_id, birth_year) %>%
                    filter(birth_year < 1935)
            }
        )
    ) %>%
    select(cohort, bd_lt_1935) %>%
    unnest(bd_lt_1935)
    
