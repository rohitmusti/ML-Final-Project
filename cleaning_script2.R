library(tidyverse)
library(acs)
library(gtools)

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

# base med income
oh.counties = geo.make(state="Ohio",county="*")
med_income_df <- acs.fetch(geography=oh.counties, table.number="B19013", endyear='2015', span=5)

med_income_df <- data.frame(cbind(data.frame(med_income_df@geography), data.frame(med_income_df@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    median_household_income=B19013_001
  )


mi_creator <- function(state_p, table_p) {
temp.oh.counties = geo.make(state=paste0(state_p),county="*")
temp_med_income_df_1 <- acs.fetch(geography=temp.oh.counties, table.number=paste0(table_p), endyear='2015', span=5)

temp_med_income_df <- data.frame(cbind(data.frame(temp_med_income_df_1@geography), data.frame(temp_med_income_df_1@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    median_household_income=B19013_001
  )

  return(rbind(med_income_df, temp_med_income_df))
}

for (i in states) {
  med_income_df <- mi_creator(paste0(i), "B19013")
  print(i) 
}

# base Lang spoken at home
oh.counties = geo.make(state="Ohio",county="*")
perc_grad_df <- acs.fetch(geography=oh.counties, table.number="B06009", endyear='2015', span=5)

perc_grad_df <- data.frame(cbind(data.frame(perc_grad_df@geography), data.frame(perc_grad_df@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    percent_grad_high_school = (B06009_003 + B06009_004 + B06009_005 + B06009_006)/(B06009_002 + B06009_003 + B06009_004 + B06009_005 + B06009_006)
  )

grad_creator <- function(state_p) {
temp.oh.counties = geo.make(state=paste0(state_p),county="*")
temp.home_lang_df <- acs.fetch(geography=oh.counties, table.number="B06009", endyear='2015', span=5)

temp.home_lang_df <- data.frame(cbind(data.frame(temp.home_lang_df@geography), data.frame(temp.home_lang_df@estimate))) %>%
  rowwise() %>% summarize(
    fips=paste0(state, county),
    percent_grad_high_school = (B06009_003 + B06009_004 + B06009_005 + B06009_006)/(B06009_002 + B06009_003 + B06009_004 + B06009_005 + B06009_006)
  )

  return(rbind(perc_grad_df, temp.home_lang_df))
}

for (i in states) {
  perc_grad_df <- grad_creator(paste0(i))
  print(i) 
}

