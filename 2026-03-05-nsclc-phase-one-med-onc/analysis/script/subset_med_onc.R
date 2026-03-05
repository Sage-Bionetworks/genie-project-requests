library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

public_med_onc <- readr::read_csv(
  here('data-raw', 'NSCLC', '2.0-public', 'med_onc_note_level_dataset.csv')
)

p2_med_onc <- readr::read_csv(
  here('data-raw', 'NSCLC', '3.1-consortium', 'med_onc_note_level_dataset.csv')
)

public_cases_p2_data <- left_join(
  select(public_med_onc, record_id, md_visit_number),
  p2_med_onc,
  by = c('record_id', 'md_visit_number'),
  relationship = 'one-to-one'
)

# Do these have the same keys?
# waldo::compare(
#   select(public_cases_p2_data, record_id, md_onc_visit_int),
#   select(public_med_onc, record_id, md_onc_visit_int)
# )

anti_join(
  public_med_onc,
  p2_med_onc,
  by = c('record_id', 'md_onc_visit_int')
) %>%
  count(record_id) %>%
  print(n = 500)

anti_join(
  public_med_onc,
  p2_med_onc,
  by = c('record_id', 'md_visit_number')
) %>%
  count(record_id) %>%
  print(n = 500)

# Not great.  The set of md_onc_visit_int values in phase 1 is not released in phase 2, and the md visit numbers aren't there either.
# Let's have a look to see if they're all close to 89 years old:

anti_join(
  public_med_onc,
  p2_med_onc,
  by = c('record_id', 'md_visit_number')
) %>%
  ggplot(aes(x = md_onc_visit_int / 365.25)) +
  geom_histogram()
# Nope.  Yikes.

# k let's take a different approach.  Find the max and min visit released in the public for each patient:
pub_min_max <- public_med_onc %>%
  group_by(record_id) %>%
  summarize(
    pub_min_int = min(md_onc_visit_int, na.rm = T),
    pub_max_int = max(md_onc_visit_int, na.rm = T)
  )

# pull all the phase 2 cases that are in those bounds.
p2_within_public_bounds <- p2_med_onc %>%
  left_join(
    pub_min_max,
    by = 'record_id',
    relationship = 'many-to-one'
  ) %>%
  filter(
    md_onc_visit_int >= pub_min_int,
    md_onc_visit_int <= pub_max_int
  )

if (any(p2_within_public_bounds$phase != "Phase I", na.rm = F)) {
  cli_abort("Some cases are not Phase I - important to resolve")
}


p2_within_public_bounds %<>%
  select(-c(pub_min_int, pub_max_int)) %>%
  augment_med_onc_imputed_ecog(., add_numeric = T)

readr::write_csv(
  p2_within_public_bounds,
  here('output', 'nsclc_ecog_public_dates.csv')
)
