library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_vio_cond <- readr::read_rds(
    here('data', 'vio_cond.rds')
)

dft_all <- readr::read_rds(
    here('data', 'all_dat.rds')
)


test_bpc <- dft_all %>% slice(1) %>% pull(dat) %>% `[[`(.,1)

find_violations(
    test_bpc,
    dft_vio_cond,
    var_to_check = "dob_ca_dx_days"
) 

find_all_violations(
    test_bpc,
    dft_vio_cond,
) 

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

dft_all %>%
    select(cohort, dat_name, viol) %>%
    unnest(viol)
