library(dplyr)
library(glue)
library(lubridate)
library(magrittr)
library(here)
library(readr)

unclear_cases <- tibble(
  date_birth = "1929-07-20", 
  date_last_contact = "2015-01-20",
  date_curation = "2020-01-01", 
  date_process = "2023-08-01", 
  date_release = "2023-11-20",
  any_redact_required = F,
  reasoning = glue(
    "There is no interval in the data that is indicative of a person living to 
  age 90 or above.  In other words, it is not relevant that they were curated,
  processed and released after their 90th birthday"
  )
)

unclear_cases %<>%
  add_row(
    date_birth = "2005-08-10", 
    date_last_contact = "2022-07-01",
    date_curation = "2023-09-01", 
    date_process = "2024-11-01", 
    date_release = "2025-06-01",
    any_redact_required = T,
    reasoning = glue(
    "The last contact with the participant was before their 18th birthday
    which we are required to redact by GENIE standards.  That they were 
    processed, curated or released after their 18th birthday has no relevance."
    )
  )

unclear_cases %<>%
  add_row(
    date_birth = "2002-08-10", 
    date_last_contact = "2020-09-01",
    date_curation = "2023-09-15", 
    date_process = "2024-10-10", 
    date_release = "2025-05-01",
    any_redact_required = T,
    reasoning = glue(
      "The last contact with this participant was after their 18th birthday, so all of their data can be released in GENIE (even that occuring before their 18th birthday."
    )
  )

unclear_cases %<>%
  mutate(
    across(
      .cols = matches("^date"),
      .fns = lubridate::ymd
    )
  ) 

augment_public <- function(dat) {
  dat %<>%
    mutate(
      INT_CONTACT = time_length(date_last_contact-date_birth, unit = "day"),
      int_contact_year = time_length(date_last_contact-date_birth, unit = "year"),
      
      INT_DOD = time_length(date_death-date_birth, unit = "day"),
      int_dod_year = time_length(date_death-date_birth, unit = "year"),
      
      YEAR_CONTACT
      
      
}

unclear_cases %>%
  augment_public

write_rds(
  here('data', '
  


