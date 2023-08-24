library("tidyr")
library(synapser)
synLogin()

warning_report <- read.csv(synGet("syn51322625")$path)

data_dictionary <- read.csv(synGet('syn34217014')$path)[,c(1,5,4)]
colnames(data_dictionary) <- c('variable','label','type')

result <- warning_report %>% 
  filter(check_no == 9 | check_no == 35) %>%
  select(-c(1,6,8,10)) %>%
  separate(column_name, c("variable", "checkbox_num"), "___") %>%
  left_join(data_dictionary, by='variable') %>%
#  filter(type != 'checkbox') %>%
  select(c(1,2,5,6,7,11,12,8,9,10))

msk_result <- result %>%
  filter(site=='MSK' | is.na(site)) %>%
  select(c(3,4,5,6,7,8,9,10))

dfci_result <- result %>%
  filter(site=='DFCI' | is.na(site)) %>%
  select(c(3,4,5,6,7,8,9,10))

uhn_result <- result %>%
  filter(site=='UHN' | is.na(site)) %>%
  select(c(3,4,5,6,7,8,9,10))

write.csv(msk_result, file='msk_empty_columns_list.csv')
write.csv(dfci_result, file='dfci_empty_columns_list.csv')
write.csv(uhn_result, file='uhn_empty_columns_list.csv')

# upload to Synapse with provanance setup
