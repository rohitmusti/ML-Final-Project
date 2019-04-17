library(tidyverse)
library(acs)

states = c(
  "Alabama",  "Alaska",  "Arizona", "Arkansas",  "California",  "Colorado",  "Connecticut",  
  "Delaware",  
  "Florida",  "Georgia",  "Hawaii",  "Idaho",  "Illinois",  "Indiana",  
  "Iowa",  "Kansas",  "Kentucky",  "Louisiana",  "Maine",  "Maryland",  
  "Massachusetts",  "Michigan",  "Minnesota",  "Mississippi",  "Missouri",  
  "Montana",  "Nebraska",  "Nevada",  "New Hampshire",  "New Jersey",  "New Mexico",  
  "New York", "North Carolina",   "North Dakota",  "Oklahoma",  "Oregon",  
  "Pennsylvania",  "Rhode Island",  "South Carolina",  "South Dakota",  
  "Tennessee",  "Texas",  "Utah",  "Vermont",  "Virginia",  "Washington",  
  "West Virginia",  "Wisconsin",  "Wyoming", "District of Columbia"
)


states_mnm = c(
  "Alabama",  "Alaska",  "Arizona", "Arkansas",  "California",  "Colorado",  "Connecticut",  
  "Delaware",  
  "Florida",  "Georgia",  "Hawaii",  "Idaho",  "Illinois",  "Indiana",  
  "Iowa",  "Kansas",  "Kentucky",  "Louisiana",  "Maine",  "Maryland",  
  "Massachusetts",  "Michigan",  "Minnesota",  "Mississippi",  "Missouri",  
  "Montana",  "Nebraska",  "Nevada",  "New Hampshire",  "New Jersey",  
  "New York", "North Carolina",   "North Dakota",  "Oklahoma",  "Oregon",  
  "Pennsylvania",  "Rhode Island",  "South Carolina",  "South Dakota",  
  "Tennessee",  "Texas",  "Utah",  "Vermont",  "Virginia",  "Washington",  
  "West Virginia",  "Wisconsin",  "Wyoming", "District of Columbia"
)

states_after_vermont <- c("Washington", "West Virginia",  "Wisconsin",  
                          "Wyoming", "District of Columbia")

########################################
# base: med income
########################################

oh.counties = geo.make(state="Ohio",county="*")
med_income_df <- acs.fetch(geography=oh.counties, table.number="B19013", endyear='2015', span=5)

med_income_df <- data.frame(cbind(data.frame(med_income_df@geography), data.frame(med_income_df@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    median_household_income=B19013_001
  )


mi_creator <- function(state_p, table_p) {
temp.counties = geo.make(state=paste0(state_p),county="*")
temp_med_income_df_1 <- acs.fetch(geography=temp.counties, table.number=paste0(table_p), endyear='2015', span=5)

temp_med_income_df <- data.frame(cbind(data.frame(temp_med_income_df_1@geography), data.frame(temp_med_income_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    median_household_income=B19013_001
  )

  return(rbind(med_income_df, temp_med_income_df))
}

# for (i in states) {
#   med_income_df <- mi_creator(paste0(i), "B19013")
#   print(i) 
# }

########################################
# base: percent high school grad
########################################

oh.counties = geo.make(state="Ohio",county="*")
perc_grad_df_1 <- acs.fetch(geography=oh.counties, table.number="B06009", endyear='2015', span=5)

perc_grad_df <- data.frame(cbind(data.frame(perc_grad_df_1@geography), data.frame(perc_grad_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    percent_grad_high_school = (B06009_003 + B06009_004 + B06009_005 + B06009_006)/(B06009_002 + B06009_003 + B06009_004 + B06009_005 + B06009_006)
  )

grad_creator <- function(state_p) {
  temp.counties = geo.make(state=paste0(state_p),county="*")
  temp_home_lang_df_1 <- acs.fetch(geography=temp.counties, table.number="B06009", endyear='2015', span=5)
  
  temp_home_lang_df <- data.frame(cbind(data.frame(temp_home_lang_df_1@geography), data.frame(temp_home_lang_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      percent_grad_high_school = (B06009_003 + B06009_004 + B06009_005 + B06009_006)/(B06009_002 + B06009_003 + B06009_004 + B06009_005 + B06009_006)
    )
  
  return(rbind(perc_grad_df, temp_home_lang_df))
}

# for (i in states) {
#   perc_grad_df <- grad_creator(paste0(i))
#   print(i) 
# }


########################################
# base: health care coverage
########################################

oh.counties = geo.make(state="Ohio",county="*")
health_coverage_df_1 <- acs.fetch(geography=oh.counties, table.number="B27001", endyear='2015', span=5)

health_coverage_df <- data.frame(cbind(data.frame(health_coverage_df_1@geography), data.frame(health_coverage_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    percent_with_healthcare = (B27001_028 + B27001_056)/(B27001_028 + B27001_056 + B27001_029 + B27001_057)
  )

health_coverage_creator <- function(state_p) {
  temp.counties = geo.make(state=paste0(state_p),county="*")
  temp_health_coverage_df_1 <- acs.fetch(geography=temp.counties, table.number="B27001", endyear='2015', span=5)
  
  temp_health_coverage_df <- data.frame(cbind(data.frame(temp_health_coverage_df_1@geography), data.frame(temp_health_coverage_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      percent_with_healthcare = (B27001_028 + B27001_056)/(B27001_028 + B27001_056 + B27001_029 + B27001_057)
    )

  return(rbind(health_coverage_df, temp_health_coverage_df))
}

# for (i in states) {
#   health_coverage_df <- health_coverage_creator(paste0(i))
#   print(i) 
# }

########################################
# base: total population
########################################

oh.counties = geo.make(state="Ohio",county="*")
total_population_df_1 <- acs.fetch(geography=oh.counties, table.number="B01003", endyear='2015', span=5)

total_population_df <- data.frame(cbind(data.frame(total_population_df_1@geography), data.frame(total_population_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    total_population = B01003_001
  )

total_pop_creator <- function(state_p) {
  temp.counties = geo.make(state=paste0(state_p),county="*")
  temp_total_population_df_1 <- acs.fetch(geography=temp.counties, table.number="B01003", endyear='2015', span=5)
  
  temp_total_population_df <- data.frame(cbind(data.frame(temp_total_population_df_1@geography), data.frame(temp_total_population_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      total_population = B01003_001
    )
  
  return(rbind(total_population_df, temp_total_population_df))

}

# for (i in states) {
#   total_population_df <- total_pop_creator(paste0(i))
#   print(i) 
# }

########################################
# base: average household size
########################################

oh.counties = geo.make(state="Ohio",county="*")
avg_household_size_df_1 <- acs.fetch(geography=oh.counties, table.number="B25010", endyear='2015', span=5)

avg_household_size_df <- data.frame(cbind(data.frame(avg_household_size_df_1@geography), data.frame(avg_household_size_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    average_household_size = B25010_001
  )

avg_household_size_creator <- function(state_p) {
  temp.counties = geo.make(state=paste0(state_p),county="*")
  temp_average_household_size_df_1 <- acs.fetch(geography=temp.counties, table.number="B25010", endyear='2015', span=5)
  
  temp_average_household_size_df <- data.frame(cbind(data.frame(temp_average_household_size_df_1@geography), data.frame(temp_average_household_size_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      average_household_size = B25010_001
    )
  return(rbind(avg_household_size_df, temp_average_household_size_df))
}

# for (i in states) {
#   avg_household_size_df <- avg_household_size_creator(paste0(i))
#   print(i) 
# }

########################################
# base: percent foreign born
########################################

oh.counties = geo.make(state="Ohio",county="*")
perc_foreign_born_df_1 <- acs.fetch(geography=oh.counties, table.number="B05002", endyear='2015', span=5)

perc_foreign_born_df <- data.frame(cbind(data.frame(perc_foreign_born_df_1@geography), data.frame(perc_foreign_born_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    perc_foreign_born = (B05002_013 / B05002_001 )
  )

perc_foreign_born_creator <- function(state_p) {
  temp.counties = geo.make(state=paste0(state_p),county="*")
  temp_perc_foreign_born_df_1 <- acs.fetch(geography=temp.counties, table.number="B05002", endyear='2015', span=5)
  
  temp_perc_foreign_born_df <- data.frame(cbind(data.frame(temp_perc_foreign_born_df_1@geography), data.frame(temp_perc_foreign_born_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      perc_foreign_born = (B05002_013 / B05002_001 )
    )
  return(rbind(perc_foreign_born_df, temp_perc_foreign_born_df))
}

#for (i in states) {
#  perc_foreign_born_df <- perc_foreign_born_creator(paste0(i))
#  print(i) 
#}


########################################
# base: percent Male
########################################


oh.counties = geo.make(state="Ohio",county="*")
perc_male_df_1 <- acs.fetch(geography=oh.counties, table.number="B01001", endyear='2015', span=5)

perc_male_df <- data.frame(cbind(data.frame(perc_male_df_1@geography), data.frame(perc_male_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    perc_male = (B01001_002 / B01001_001 )
  )

perc_male_creator <- function(state_p) {
  temp.counties = geo.make(state=paste0(state_p),county="*")
  temp_perc_male_df_1 <- acs.fetch(geography=temp.counties, table.number="B01001", endyear='2015', span=5)
  
  temp_perc_male_df <- data.frame(cbind(data.frame(temp_perc_male_df_1@geography), data.frame(temp_perc_male_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      perc_male = (B01001_002 / B01001_001 )
    )
  return(rbind(perc_male_df, temp_perc_male_df))
}

# for (i in states) {
#   perc_male_df <- perc_male_creator(paste0(i))
#   print(i) 
# }

########################################
# base: median age
########################################

oh.counties = geo.make(state="Ohio",county="*")
median_age_df_1 <- acs.fetch(geography=oh.counties, table.number="B01002", endyear='2015', span=5)

median_age_df <- data.frame(cbind(data.frame(median_age_df_1@geography), data.frame(median_age_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    median_age = (B01002_001 )
  )

median_age_creator <- function(state_p) {
  temp.counties = geo.make(state=paste0(state_p),county="*")
  temp_median_age_df_1 <- acs.fetch(geography=oh.counties, table.number="B01002", endyear='2015', span=5)
  
  temp_median_age_df <- data.frame(cbind(data.frame(temp_median_age_df_1@geography), data.frame(temp_median_age_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      median_age = (B01002_001 )
    )
  return(rbind(median_age_df, temp_median_age_df))
}

# for (i in states_mnm) {
#   median_age_df <- perc_male_creator(paste0(i))
#   print(i) 
# }

########################################
# base: poverty level
########################################

oh.counties = geo.make(state="Ohio",county="*")
poverty_df_1 <- acs.fetch(geography=oh.counties, table.number="B06012", endyear='2015', span=5)

poverty_df <- data.frame(cbind(data.frame(poverty_df_1@geography), data.frame(poverty_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    poverty_population = (B06012_002)
  )

poverty_df_creator <- function(state_p) {
  oh.counties = geo.make(state=paste0(state_p),county="*")
  temp_poverty_df_1 <- acs.fetch(geography=oh.counties, table.number="B06012", endyear='2015', span=5)
  
  temp_poverty_df <- data.frame(cbind(data.frame(temp_poverty_df_1@geography), data.frame(temp_poverty_df_1@estimate))) %>%
    rowwise() %>% summarize(
      fips=paste0(state, county),
      poverty_population = (B06012_002)
    )
  return(rbind(poverty_df, temp_poverty_df))
}

for (i in states) {
#  med_income_df <- mi_creator(paste0(i), "B19013")
#  perc_grad_df <- grad_creator(paste0(i))
#  health_coverage_df <- health_coverage_creator(paste0(i))
#  total_population_df <- total_pop_creator(paste0(i))
#  avg_household_size_df <- avg_household_size_creator(paste0(i))
#  perc_foreign_born_df <- perc_foreign_born_creator(paste0(i))
#  perc_male_df <- perc_male_creator(paste0(i))
  median_age_df <- median_age_creator(paste0(i))
#  poverty_df <- poverty_df_creator(paste0(i))
  print(i) 
}

final_data <- inner_join(med_income_df, perc_grad_df, by="fips")
final_data <- inner_join(final_data, health_coverage_df, by="fips")
final_data <- inner_join(final_data, total_population_df, by="fips")
final_data <- inner_join(final_data, avg_household_size_df, by="fips")
final_data <- inner_join(final_data, perc_foreign_born_df, by="fips")
final_data <- inner_join(final_data, perc_male_df, by="fips")
final_data <- inner_join(final_data, median_age_df, by="fips")
final_data <- inner_join(final_data, poverty_df, by="fips")

write_csv(final_data, "final_data.csv")

library(tidyverse)
final_data <- read_csv("FINAL_DATA/final_data.csv")
presc_data <- read_csv("FINAL_DATA/presc_data.csv")

final_data <- inner_join(final_data, presc_data)

write_csv(final_data, "final_data_2.csv")
