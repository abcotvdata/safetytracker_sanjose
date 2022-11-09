library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(rvest)
library(XML)
library(sf)
library(zoo)

# load RDS for the annual and the latest weekly
recent_crime_all <- readRDS("scripts/rds/oakland_crime_recent.rds")
annual_crime_all <- readRDS("scripts/rds/oakland_crime_annual.rds")

# join the two files
oakland_crime <- left_join(annual_crime_all,recent_crime_all %>% select(4,5,9,10,12),
                           by=c("description"="description","district"="district")) 

# Extract the last 12 months into a new column
oakland_crime$last12mos <- (oakland_crime$total21-oakland_crime$ytd21)+oakland_crime$ytd22
oakland_crime <- oakland_crime %>% select(7:9,1:6,11,10,13,12)

# write csv of Oakland crime as a backup
# worthwhile to think through if the full csv is even necessary to save; maybe for redundancy
write_csv(oakland_crime,"data/output/oakland_crime.csv")

# Set variable of Chicago population
# likely needs added to the tracker itself
oakland_population <- 433823

# San Francisco police districts geo file with populations
districts_geo <- readRDS("scripts/rds/oakland_districts.rds")

# Divide into citywide_crime and district_crime files
citywide_crime <- oakland_crime %>% filter(district=="Citywide")
district_crime <- oakland_crime %>% filter(district!="Citywide")

# add zeros where there were no crimes tallied that year
#citywide_crime[is.na(citywide_crime)] <- 0
#district_crime[is.na(district_crime)] <- 0

### DISTRICT CRIME TOTALS AND OUTPUT

# Join to Oakland crime districts
district_crime$district <- sub("Area ","",district_crime$district)
district_crime <- full_join(districts_geo, district_crime, by="district")
# add zeros where there were no crimes tallied that year
district_crime[is.na(district_crime)] <- 0

# add 3-year totals and annualized averages
district_crime$total_prior3years <- district_crime$total19+
  district_crime$total20+
  district_crime$total21
district_crime$avg_prior3years <- round(((district_crime$total19+
                                            district_crime$total20+
                                            district_crime$total21)/3),1)
# now add the increases or change percentages
district_crime$inc_19to21 <- round(district_crime$total21/district_crime$total19*100-100,1)
district_crime$inc_19tolast12 <- round(district_crime$last12mos/district_crime$total19*100-100,1)
district_crime$inc_21tolast12 <- round(district_crime$last12mos/district_crime$total21*100-100,1)
district_crime$inc_prior3yearavgtolast12 <- round((district_crime$last12mos/district_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
district_crime$rate19 <- round((district_crime$total19/district_crime$population)*100000,1)
district_crime$rate20 <- round((district_crime$total20/district_crime$population)*100000,1)
district_crime$rate21 <- round((district_crime$total21/district_crime$population)*100000,1)
district_crime$rate_last12 <- round((district_crime$last12mos/district_crime$population)*100000,1)
district_crime$rate_prior3years <- 
  round((district_crime$avg_prior3years/district_crime$population)*100000,1)

# Now reduce the precinct down to just the columns we likely need for the tracker pages
# district_crime <- district_crime %>% select(1,4,5,6,26:28,36:40,44:55,29,42)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# create a quick long-term annual table
district_yearly <- district_crime %>% select(1,5:11,13) %>% st_drop_geometry()
write_csv(district_yearly,"data/output/yearly/district_yearly.csv")

# add zeros where there were no crimes tallied that year
# citywide_crime[is.na(citywide_crime)] <- 0
# add 3-year annualized averages
citywide_crime$total_prior3years <- citywide_crime$total19+
  citywide_crime$total20+
  citywide_crime$total21
citywide_crime$avg_prior3years <- round(((citywide_crime$total19+
                                            citywide_crime$total20+
                                            citywide_crime$total21)/3),1)
# now add the increases or change percentages
citywide_crime$inc_19to21 <- round(citywide_crime$total21/citywide_crime$total19*100-100,1)
citywide_crime$inc_19tolast12 <- round(citywide_crime$last12mos/citywide_crime$total19*100-100,1)
citywide_crime$inc_21tolast12 <- round(citywide_crime$last12mos/citywide_crime$total21*100-100,1)
citywide_crime$inc_prior3yearavgtolast12 <- round((citywide_crime$last12mos/citywide_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
citywide_crime$rate19 <- round((citywide_crime$total19/oakland_population)*100000,1)
citywide_crime$rate20 <- round((citywide_crime$total20/oakland_population)*100000,1)
citywide_crime$rate21 <- round((citywide_crime$total21/oakland_population)*100000,1)
citywide_crime$rate_last12 <- round((citywide_crime$last12mos/oakland_population)*100000,1)
# 3 yr rate
citywide_crime$rate_prior3years <- 
  round((citywide_crime$avg_prior3years/oakland_population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_crime <- citywide_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
citywide_crime <- citywide_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# create a quick long-term annual table
citywide_yearly <- citywide_crime %>% select(4:10,12)
write_csv(citywide_yearly,"data/output/yearly/citywide_yearly.csv")

# Now make individual crime files for trackers
# filter precinct versions - using beat for code consistency
murders_district <- district_crime %>% filter(category=="Murder")
sexassaults_district <- district_crime %>% filter(category=="Sexual Assault")
robberies_district <- district_crime %>% filter(category=="Robbery")
assaults_district <- district_crime %>% filter(category=="Aggravated Assault")
burglaries_district <- district_crime %>% filter(category=="Burglary")
thefts_district <- district_crime %>% filter(category=="Larceny")
autothefts_district <- district_crime %>% filter(category=="Motor Vehicle Theft")
# filter citywide versions
murders_city <- citywide_crime %>% filter(category=="Murder")
sexassaults_city <- citywide_crime %>% filter(category=="Sexual Assault")
robberies_city <- citywide_crime %>% filter(category=="Robbery")
assaults_city <- citywide_crime %>% filter(category=="Aggravated Assault")
burglaries_city <- citywide_crime %>% filter(category=="Burglary")
thefts_city <- citywide_crime %>% filter(category=="Larceny")
autothefts_city <- citywide_crime %>% filter(category=="Motor Vehicle Theft")

# make the death rate comparables file unique to this state
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="CA")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")

#### 
# Archive latest files as csv and rds store for use in trackers
# First save the weekly files as output csvs for others to use
write_csv(district_crime,"data/output/weekly/district_crime.csv")
write_csv(citywide_crime,"data/output/weekly/citywide_crime.csv")
# Archive a year's worth of week-numbered files from the weekly updates
write_csv(district_crime,paste0("data/output/archive/district_crime_week",asofdate,".csv"))
write_csv(citywide_crime,paste0("data/output/archive/citywide_crime_week",asofdate,".csv"))
# Now save the files needed for trackers into RDS store in scripts for GH Actions
# precinct versions
saveRDS(district_crime,"scripts/rds/district_crime.rds")
saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
# city versions
saveRDS(citywide_crime,"scripts/rds/citywide_crime.rds")
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")

### Some tables for charts for our pages
# sf_crime_totals %>% write_csv("data/output/yearly/totals_by_type.csv")
murders_city %>% select(4:9,12) %>% write_csv("data/output/yearly/murders_city.csv")
sexassaults_city %>% select(4:9,12) %>%  write_csv("data/output/yearly/sexassaults_city.csv")
autothefts_city %>% select(4:9,12) %>%  write_csv("data/output/yearly/autothefts_city.csv")
thefts_city %>% select(4:9,12) %>%  write_csv("data/output/yearly/thefts_city.csv")
burglaries_city %>% select(4:9,12) %>%  write_csv("data/output/yearly/burglaries_city.csv")
robberies_city %>% select(4:9,12) %>%  write_csv("data/output/yearly/robberies_city.csv")
assaults_city %>% select(4:9,12) %>%  write_csv("data/output/yearly/assaults_city.csv")