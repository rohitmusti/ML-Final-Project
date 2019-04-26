library(tidyverse)

cl_op <- read.csv("clean_opioid.csv")
nat <- read.csv("FINAL_DATA/all_data_ready_for_anal.csv")

va_national_data <- nat[nat$fips %in% cl_op$fips,]
va_national_data <- va_national_data[!is.na(va_national_data$opioid_death_rate),]


non_va_national_data <- nat[!(nat$fips %in% cl_op$fips),]
non_va_national_data <- non_va_national_data[!is.na(non_va_national_data$opioid_death_rate),]

va_national_data <- na.omit(va_national_data)
non_va_national_data <- na.omit(non_va_national_data)

write_csv(va_national_data, "virginia_national_data.csv")
write_csv(non_va_national_data, "non-virginia_national_data.csv")
