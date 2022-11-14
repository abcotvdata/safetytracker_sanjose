library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(rvest)
library(XML)
library(sf)
library(zoo)


# OPEN WORK: Set a cron to do this twice a week, week 2 and 3 of month

# scrape the month by month table from SJPD web site
sjurl <- "https://www.sjpd.org/records/crime-stats-maps/crime-statistics-monthly"
sanjose_scrape <- sjurl %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="widget_4_178_353"]/table[1]') %>%
  html_table()
sj_crime_recent <- sanjose_scrape[[1]]

# scrape ytd second table from same page
sanjose_scrape <- sjurl %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="widget_4_178_353"]/table[2]') %>%
  html_table()
sj_crime_ytd <- sanjose_scrape[[1]]

# scrape annual table from a separate page
sj_url2 <- "https://www.sjpd.org/records/crime-stats-maps/crime-statistics-annual"
sanjose_scrape <- sj_url2 %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="widget_343_190_351"]/table[4]') %>%
  html_table()
sj_crime_annual <- sanjose_scrape[[1]]

# They publish rates on this same site; grabbing for reference/compare
sanjose_scrape <- sj_url2 %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="widget_343_190_351"]/table[5]') %>%
  html_table()
sj_crime_rates <- sanjose_scrape[[1]]

# SJPD keeps some additional dashboards here for reference
# https://www.sjpd.org/records/crime-stats-maps/police-dashboards


### OPEN WORK TO PROCESS THESE FILES INTO SAN JOSE CRIME FILE FOR TRACKER

sj_crime <- as.data.frame(t(sj_crime_annual))
row.names(sj_crime)=NULL 
names(sj_crime) <- c("category","total12","total13","total14","total15","total16","total17","total18","total19","total20","total21")
sj_crime <- sj_crime[-1,]
sj_crime$category <- case_when(str_detect(sj_crime$category, "Aggravated") ~ "Aggravated Assault",
                               str_detect(sj_crime$category, "Vehicle") ~ "Vehicle Theft",
                               TRUE ~ sj_crime$category)
sj_crime <- sj_crime %>% filter(category %in% c("Homicide","Rape","Robbery","Aggravated Assault",
                                                "Burglary","Larceny","Vehicle Theft"))
# clean up
# remove stray period and then other stray characters throughout
sj_crime$total17 <- gsub("[.]", "", sj_crime$total17)
sj_crime$total12 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total12))
sj_crime$total13 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total13))
sj_crime$total14 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total14))
sj_crime$total15 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total15))
sj_crime$total16 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total16))
sj_crime$total17 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total17))
sj_crime$total18 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total18))
sj_crime$total19 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total19))
sj_crime$total20 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total20))
sj_crime$total21 <- as.numeric(gsub("[^0-9.-]", "", sj_crime$total21))

# clean up two ytd columns to add to this
names(sj_crime_ytd) <- c("category","ytd22","ytd21","change")
sj_crime_ytd <- sj_crime_ytd %>% 
  filter(category %in% c("Homicide","Rape","Robbery",
                         "Aggravated Assault","Burglary",
                         "Larceny","Vehicle Theft")) %>%
  select(1:3)
sj_crime_ytd$ytd22 <- as.numeric(gsub("[^0-9.-]", "", sj_crime_ytd$ytd22))
sj_crime_ytd$ytd21 <- as.numeric(gsub("[^0-9.-]", "", sj_crime_ytd$ytd21))

# merge cols into main sj_crime table
sj_crime <- left_join(sj_crime,sj_crime_ytd,by="category")

# Extract the last 12 months into a new column
sj_crime$last12mos <- (sj_crime$total21-sj_crime$ytd21)+sj_crime$ytd22

# write csv of Oakland crime as a backup
# worthwhile to think through if the full csv is even necessary to save; maybe for redundancy
write_csv(sj_crime,"data/output/sj_crime.csv")

# Set variable of Chicago population
# likely needs added to the tracker itself
sanjose_population <- 1014545

# add geo file if we have something; we don't yet have a solution for San Jose
# districts_geo <- readRDS("scripts/rds/sanjose_districts.rds")

# Divide into citywide_crime and district_crime files
# citywide_crime <- sj_crime %>% filter(district=="Citywide")
# district_crime <- sj_crime %>% filter(district!="Citywide")

# add zeros where there were no crimes tallied that year
#citywide_crime[is.na(citywide_crime)] <- 0
#district_crime[is.na(district_crime)] <- 0

datecalc <- sj_crime_recent %>% summarise_all(~ sum(is.na(.)))
datecalc$empty_months <- rowSums(datecalc == 12)
datecalc$month_number <- 12-datecalc$empty_months
datecalc$date <- paste0(datecalc$month_number,"/2022")
datecalc$date <- lubridate::my(datecalc$date)
datecalc$asofdate <- (lubridate::ceiling_date(datecalc$date,unit = "month"))-1
asofdate <- datecalc$asofdate
saveRDS(asofdate,"scripts/rds/asofdate.rds")
  
### ANNUAL CRIME TALLIES FOR CITYWIDE ONLY
# YET TO SOLVE FOR DISTRICTS BECAUSE SJ DOESN'T RELEASE THE DATA RELIABLY

citywide_crime <- sj_crime

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
citywide_crime$inc_prior3yearavgtolast12 <- round((citywide_crime$last12mos/citywide_crime$avg_prior3years)*100-100,1)
# add crime rates for each year
citywide_crime$rate19 <- round((citywide_crime$total19/sanjose_population)*100000,1)
citywide_crime$rate20 <- round((citywide_crime$total20/sanjose_population)*100000,1)
citywide_crime$rate21 <- round((citywide_crime$total21/sanjose_population)*100000,1)
citywide_crime$rate_last12 <- round((citywide_crime$last12mos/sanjose_population)*100000,1)
# 3 yr rate
citywide_crime$rate_prior3years <- 
  round((citywide_crime$avg_prior3years/sanjose_population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_crime <- citywide_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
citywide_crime <- citywide_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# create a quick long-term annual table
citywide_yearly <- citywide_crime %>% select(1:11,14)

# add additional years from state archive of reported ucr crimes back to 2000
yearly_archive <- read_csv("data/source/annual/sj_annual_state.csv")
# yearly_archive$category <- ifelse(yearly_archive$category=="Homicide","Murder",yearly_archive$category)
# yearly_archive$category <- ifelse(yearly_archive$category=="Rape","Sexual Assault",yearly_archive$category)
citywide_yearly <- right_join(citywide_yearly,yearly_archive %>% select(1:12,23),by="category") %>% 
  select(1,13:24,2:12)
# save for annual charts  
write_csv(citywide_yearly,"data/output/yearly/citywide_yearly.csv")




# Now make individual crime files for trackers
murders_city <- citywide_crime %>% filter(category=="Homicide")
sexassaults_city <- citywide_crime %>% filter(category=="Rape")
robberies_city <- citywide_crime %>% filter(category=="Robbery")
assaults_city <- citywide_crime %>% filter(category=="Aggravated Assault")
burglaries_city <- citywide_crime %>% filter(category=="Burglary")
thefts_city <- citywide_crime %>% filter(category=="Larceny")
autothefts_city <- citywide_crime %>% filter(category=="Vehicle Theft")

# make the death rate comparables file unique to this state
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="CA")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")

#### 
# Archive latest files as csv and rds store for use in trackers
# First save the weekly files as output csvs for others to use
write_csv(citywide_crime,"data/output/citywide_crime.csv")
# Archive a year's worth of week-numbered files from the weekly updates
write_csv(citywide_crime,paste0("data/output/archive/citywide_crime_week","SEPT22",".csv"))

# Now save the files needed for trackers into RDS store in scripts for GH Actions
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
citywide_yearly %>% filter(category=="Homicide") %>% write_csv("data/output/yearly/murders_city.csv")
citywide_yearly %>% filter(category=="Rape") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_yearly %>% filter(category=="Motor Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_yearly %>% filter(category=="Larceny") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_yearly %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_yearly %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_yearly %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")
