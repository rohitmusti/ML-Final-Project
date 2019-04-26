library(tidyverse)

avg_household_size_df <- read_csv("avg_household_size_df.csv")
health_coverage_df <- read_csv("health_coverage_df.csv")
med_house_income_df <- read_csv("med_house_income.csv")
median_age_df <- read_csv("median_age_df.csv")
perc_foreign_born_df <- read_csv("perc_foreign_born_df.csv")
perc_grad_df <- read_csv("perc_grad_df.csv")
perc_male_df <- read_csv("perc_male_df.csv")
poverty_df <- read_csv("poverty_df.csv")
presc_data <- read_csv("presc_data.csv")
total_population_df <- read_csv("total_population_df.csv")
presc_rates <- read_csv("presc_data.csv")


acs_final1 <- distinct(inner_join(avg_household_size_df, health_coverage_df))
acs_final2 <- inner_join(med_house_income_df, median_age_df, by="fips")
acs_final3 <- inner_join(perc_foreign_born_df, perc_grad_df)
acs_final4 <- inner_join(perc_male_df, poverty_df)
acs_final5 <- inner_join(presc_data, total_population_df)

acs_final1.1 <- inner_join(acs_final1, acs_final2)
acs_final1.2 <- inner_join(acs_final3, acs_final4)

acs_final2.0 <- inner_join(acs_final5, acs_final1.1)

acs_final_done <- inner_join(acs_final2.0, acs_final1.2)

write_csv(acs_final_done, "acs_final.csv")

merge_acs_presc <- inner_join(acs_final_done, presc_rates)

opioid_deaths <- read_csv("cdc_wonder_opioid_cleaned.csv")

merge_acs_presc_death_rate <- inner_join(merge_acs_presc, opioid_deaths)

write_csv(merge_acs_presc_death_rate,"all_data_ready_for_anal.csv")
