
library(tidyverse)

orx <- read.csv(file="original_Opioid_rx.csv")
acs <- read.csv(file="original_ACS.csv")
odash <- read.csv(file="original_Opioid_Dashboard.csv")

orx <- orx %>%
    rename(fips=FIPS.County.Code,
               p_rate=X2016.Prescribing.Rate) %>%
    select(fips, p_rate)

acs <- acs %>%
    rename(year=ACS.Year,
           locality_name=NAME,
           fips=Full_FIPS) %>%
    filter(year==2016) %>%
    select(-year, -ACS.5.Yr.VA.Variable.Value) %>%
    spread(key=ACS.5.Yr.FIPS.Variable.Name, value=ACS.5.Yr.FIPS.Variable.Value) %>%
    rename(
        "Total population" = `DP05 0001E`,
        "Average Household size"= `DP02 0015E`	,
       	"Foreign Born"= `DP02 0092PE`,
       	"Health Insurance Coverage" = `DP03 0096PE`,
       	"Health Insurance Coverage - Public" = `DP03 0098PE`,
        "High School graduate or higher" = `DP02 0066PE`,
        "Language spoken at home - English"= `DP02 0111PE`,
        "Language spoken at home - Non English" = `DP02 0112PE`,
        "Mean travel time to work"= `DP03 0025E`	,
        "Median Age"= `DP05 0017E`	,
        "Median home value"= `DP04 0089E`	,
        "Median household income"= `DP03 0062E`	,
        "Unemployment Rate"= `DP03 0009PE`,
       	"Veterans" = `DP02 0069PE`,
       	"With Disability" = `DP02 0071PE`,
       	"Below poverty" = `DP03 0128PE`
    )

odash <- odash %>%
    rename(year=Year,
           fips=FIPS.Code,
           type=Type,
           all_case_count=Case.Count.Display,
           all_od_rate=Rate) %>%
    filter(year==2016) %>%
    select(fips, type, all_od_rate) %>%
    select(-`Neonatal Abstinance Syndrome`)

clean_opioid <- full_join(odash,orx)

clean_opioid <- full_join(clean_opioid, acs)

#write_csv(x=clean_opioid, path="clean_opioid.csv")


#desired.cols.acs <- c("DP05%200001E","DP02%200015E",
#       	"DP02%200092PE","DP03%200096PE","DP03%200098PE","DP02%200066PE",
#        "DP02%200111PE","DP02%200112PE","DP03%200025E","DP05%200017E",
#        "DP04%200089E","DP03%200062E","DP03%200009PE","DP02%200069PE",
#       	"DP02%200071PE","DP03%200128PE")
#
#desired.cols.acs.dirty <- c("DP05 0001E","DP02 0015E",
#       	"DP02 0092PE","DP03 0096PE","DP03 0098PE","DP02 0066PE",
#        "DP02 0111PE","DP02 0112PE","DP03 0025E","DP05 0017E",
#        "DP04 0089E","DP03 0062E","DP03 0009PE","DP02 0069PE",
#       	"DP02 0071PE","DP03 0128PE")


