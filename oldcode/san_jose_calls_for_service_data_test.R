library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(rvest)
library(XML)
library(sf)
library(zoo)

# scrape the month by month table from SJPD web site
# OPEN WORK: Set a cron to do this twice a week, week 2 and 3 of month
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
# OPEN WORK: They publish rates on this same site; grabbing to compare
sanjose_scrape <- sj_url2 %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="widget_343_190_351"]/table[5]') %>%
  html_table()
sj_crime_rates <- sanjose_scrape[[1]]
# SJPD keeps some additional dashboards here for reference
# https://www.sjpd.org/records/crime-stats-maps/police-dashboards

# OPEN WORK: They publish calls for service here; would need major adjustment
# https://data.sanjoseca.gov/dataset/police-calls-for-service/resource/721b045a-51f2-4e58-b571-4628f7248783

download.file("https://data.sanjoseca.gov/dataset/c5929f1b-7dbe-445e-83ed-35cca0d3ca8b/resource/721b045a-51f2-4e58-b571-4628f7248783/download/policecalls2022.csv",
              "data/source/sanjose/sanjose_policecalls_2022.csv")


sj_calls <- read_csv("data/source/sanjose/sanjose_policecalls_2022.csv", 
                     col_types = cols(EID = col_character(), 
                                      OFFENSE_TIME = col_character())) %>% janitor::clean_names()

sj_calls <- sj_calls %>% select(-3) %>% unique
sj_calls$report_date <- mdy_hms(sj_calls$report_date)
sj_calls$offense_date <- mdy_hms(sj_calls$offense_date)
sj_calls$offense_month <- month(sj_calls$offense_date)
sj_calls$offense_time <- hms(sj_calls$offense_time)
sj_calls$hour <- hour(sj_calls$offense_time)


sanjose_crime_summary <- sj_calls %>% 
  group_by(call_type, final_dispo) %>%
  summarise(count=n())
sanjose_crime_address <- sj_calls %>% 
  group_by(address) %>%
  summarise(count=n())
sanjose_crime_summary_placename <- sj_calls %>% 
  group_by(common_place_name) %>%
  summarise(count=n())


# start date is a repeated item throughout; if eliminated, mostly unique records
# call number is repeated; if eliminated after first unique count and take the last incident?



sanjose_crime_address <- sanjose_crime_summary_address %>%
  mutate(id=seq.int(nrow(sanjose_crime_address))) %>%
  select(3,1) %>% 
  janitor::clean_names()
sanjose_crime_address$city <- "San Jose"
sanjose_crime_address$state <- "CA"
sanjose_crime_address$zip <- ""
sanjose_crime_address$address <- str_replace_all(sanjose_crime_address$address,"\\]","")
sanjose_crime_address$address <- str_replace_all(sanjose_crime_address$address,"\\[","")
# sanjose_crime_addresses$address <- str_replace_all(sanjose_crime_addresses$address,".*-","")



write_csv(head(sanjose_crime_address,9999),"sanjose_addresses.csv")

library(censusxy)
sanjose_crimexy2 <- cxy_geocode(sanjose_crime_address, street = "address", city = "city", state = "state", 
                                output = "simple", class = "sf")

addresses_unlocated <- left_join(sanjose_crime_address,sanjose_crimexy2,by="address")
addresses_unlocated <- addresses_unlocated %>% filter(is.na(id.y))

map_sjcrime <- leaflet(sanjose_crimexy2) %>% 
  setView(-121.9, 37.3, zoom = 11) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addCircleMarkers(color = "green", clusterOptions = markerClusterOptions(),
                   popup = ~ paste(address, geometry))
map_sjcrime



