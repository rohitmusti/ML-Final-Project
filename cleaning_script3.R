library(tidyverse)

opioid_data <- read_csv("opioid_overdose_deathrates_raw.csv")

opioid_data_cleaned <- opioid_data %>%
  select(-Notes, -County, -Population) %>%
  mutate(fips=`County Code`) %>%
  select(-`County Code`) %>%
  filter(`MCD - Drug/Alcohol Induced` == "Drug-induced causes") %>%
  select(fips, `Crude Rate`)
  
write_csv(opioid_data_cleaned, "FINAL_DATA/cdc_wonder_opioid_cleaned.csv")