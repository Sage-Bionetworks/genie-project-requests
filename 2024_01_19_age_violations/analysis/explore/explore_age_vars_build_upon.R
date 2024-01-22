
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_all <- readr::read_rds(
    here('data', 'all_dat.rds')
)

get_var_dat <- function(dat) {
    tibble(
        var = names(dat),
        type = purrr::map_chr(dat, typeof)
    )
}

# dft_all %>%
#     filter(dat_name %in% 'ca_ind') %>%
#     filter(cohort %in% "CRC") %>%
#     pull(dat) %>%
#     `[[`(.,1) %>%
#     get_var_dat

dft_all %<>%
    mutate(
        var_dat = map(
            .x = dat,
            .f = get_var_dat
        )
    )

dft_var <- dft_all %>%
    select(cohort, dat_name, var_dat) %>%
    unnest(var_dat)

# Technically this gets unique combinations of variable and type.
dft_unique_var <- dft_var %>%
    group_by(var, type) %>%
    slice(1) %>%
    ungroup(.)

obvious_time_strings <- c(
    '_int', '_age$', '^age', 
    '_days', '_mos', '_yrs'
)

dft_age_vars <- readr::read_rds(here('data', 'age_vars.rds'))

# A helper printout to discover variables that look to be 
# time related but are not yet declared in our file.
dft_unique_var %>%
    mutate(
        looks_like_time_string = str_detect(
            var,
            paste(obvious_time_strings, collapse = "|")
        ),
        not_in_declared_vars = !(var %in% dft_age_vars$var)
    ) %>%
    filter(
        looks_like_time_string & not_in_declared_vars
    ) %>%
    arrange(cohort) %>%
    View(.)
 
# A much greater burden:  look through all the variables to see if any appear
#   to be agey.
dft_unique_var %>%
    filter(!(var %in% dft_age_vars$var)) %>%
    View(.)




# Check that 

