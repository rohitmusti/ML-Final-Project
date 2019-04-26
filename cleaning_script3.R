library(tidyverse)

opioid_data <- read_csv("opioid_overdose_deathrates_raw.csv")

opioid_data_cleaned <- opioid_data %>%
  select(-Notes, -County, -Population) %>%
  mutate(fips=`County Code`) %>%
  select(-`County Code`) %>%
  filter(`MCD - Drug/Alcohol Induced` == "Drug-induced causes") %>%
  select(fips, `Crude Rate`) %>%
  rename(opioid_death_rate = `Crude Rate`)

opioid_data_cleaned$opioid_death_rate[opioid_data_cleaned$opioid_death_rate == "Unreliable"] <- NA
opioid_data_cleaned$opioid_death_rate <- as.numeric(as.character(opioid_data_cleaned$opioid_death_rate))

write_csv(opioid_data_cleaned, "FINAL_DATA/cdc_wonder_opioid_cleaned.csv")