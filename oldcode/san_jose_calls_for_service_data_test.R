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





# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- max(chicago_crime$date)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# write csv of Chicago crime as a backup
# worthwhile to think through if the full csv is even necessary to save; maybe for redundancy
write_csv(chicago_crime,"data/output/chicago_crime.csv")
saveRDS(chicago_crime,"scripts/rds/chicago_crime.rds")

# Extract the last 12 months into a separate file
chicago_crime_last12 <- chicago_crime %>% filter(date>(max(chicago_crime$date)-31536000))

### CITYWIDE CRIME TOTALS AND OUTPUT

# Set variable of Chicago population
# likely needs added to the tracker itself
chicago_population <- 2696561

# Calculate of each detailed offense type CITYWIDE
citywide_detailed <- chicago_crime %>%
  group_by(category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_detailed <- citywide_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_detailed_last12 <- chicago_crime_last12 %>%
  group_by(category,description) %>%
  summarise(last12mos = n())
citywide_detailed <- left_join(citywide_detailed,citywide_detailed_last12,by=c("category","description"))
# add zeros where there were no crimes tallied that year
citywide_detailed[is.na(citywide_detailed)] <- 0
rm(citywide_detailed_last12)
# Calculate a total across the 3 prior years
citywide_detailed$total_prior3years <- citywide_detailed$total19+citywide_detailed$total20+citywide_detailed$total21
citywide_detailed$avg_prior3years <- round(citywide_detailed$total_prior3years/3,1)
# calculate increases
citywide_detailed$inc_19to21 <- round(citywide_detailed$total21/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_19tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_21tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total21*100-100,1)
citywide_detailed$inc_prior3yearavgtolast12 <- round((citywide_detailed$last12mos/citywide_detailed$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_detailed$rate19 <- round(citywide_detailed$total19/chicago_population*100000,1)
citywide_detailed$rate20 <- round(citywide_detailed$total20/chicago_population*100000,1)
citywide_detailed$rate21 <- round(citywide_detailed$total21/chicago_population*100000,1)
citywide_detailed$rate_last12 <- round(citywide_detailed$last12mos/chicago_population*100000,1)
# calculate a multiyear rate
citywide_detailed$rate_prior3years <- round(citywide_detailed$avg_prior3years/chicago_population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate of each detailed offense type CITYWIDE
citywide_detailed_monthly <- chicago_crime %>%
  group_by(category,description,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_detailed_monthly <- citywide_detailed_monthly %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_detailed_monthly$rollavg_3month <- round(citywide_detailed_monthly$rollavg_3month,0)
# write to save for charts for detailed monthly
write_csv(citywide_detailed_monthly,"data/output/monthly/citywide_detailed_monthly.csv")

# Calculate of each category of offense CITYWIDE
citywide_category <- chicago_crime %>%
  group_by(category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_category <- citywide_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_category_last12 <- chicago_crime_last12 %>%
  group_by(category) %>%
  summarise(last12mos = n())
citywide_category <- left_join(citywide_category,citywide_category_last12,by=c("category"))
# add zeros where there were no crimes tallied that year
citywide_category[is.na(citywide_category)] <- 0
# Calculate a total across the 3 prior years
citywide_category$total_prior3years <- citywide_category$total19+citywide_category$total20+citywide_category$total21
citywide_category$avg_prior3years <- round(citywide_category$total_prior3years/3,1)
# calculate increases
citywide_category$inc_19to21 <- round(citywide_category$total21/citywide_category$total19*100-100,1)
citywide_category$inc_19tolast12 <- round(citywide_category$last12mos/citywide_category$total19*100-100,1)
citywide_category$inc_21tolast12 <- round(citywide_category$last12mos/citywide_category$total21*100-100,1)
citywide_category$inc_prior3yearavgtolast12 <- round((citywide_category$last12mos/citywide_category$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_category$rate19 <- round(citywide_category$total19/chicago_population*100000,1)
citywide_category$rate20 <- round(citywide_category$total20/chicago_population*100000,1)
citywide_category$rate21 <- round(citywide_category$total21/chicago_population*100000,1)
citywide_category$rate_last12 <- round(citywide_category$last12mos/chicago_population*100000,1)
# calculate a multiyear rate
citywide_category$rate_prior3years <- round(citywide_category$avg_prior3years/chicago_population*100000,1)

# Calculate monthly totals for categories of crimes CITYWIDE
citywide_category_monthly <- chicago_crime %>%
  group_by(category,month) %>%
  summarise(count = n())
# add rolling average of 3 months for chart trend line & round to clean
citywide_category_monthly <- citywide_category_monthly %>%
  arrange(category,month) %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_category_monthly$rollavg_3month <- round(citywide_category_monthly$rollavg_3month,0)

# write series of monthly files for charts (NOTE murder is written above in detailed section)
write_csv(citywide_category_monthly,"data/output/monthly/citywide_category_monthly.csv")
citywide_category_monthly %>% filter(category=="Criminal Sexual Assault") %>% write_csv("data/output/monthly/sexassaults_monthly.csv")
citywide_category_monthly %>% filter(category=="Motor Vehicle Theft") %>% write_csv("data/output/monthly/autothefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Theft Over $500") %>% write_csv("data/output/monthly/thefts_monthly.csv")
citywide_category_monthly %>% filter(category=="Burglary") %>% write_csv("data/output/monthly/burglaries_monthly.csv")
citywide_category_monthly %>% filter(category=="Robbery") %>% write_csv("data/output/monthly/robberies_monthly.csv")
citywide_category_monthly %>% filter(category=="Aggravated Battery") %>% write_csv("data/output/monthly/batteries_monthly.csv")
citywide_category_monthly %>% filter(category=="Murder") %>% write_csv("data/output/monthly/murders_monthly.csv")

# Calculate of each type of crime CITYWIDE
citywide_type <- chicago_crime %>%
  group_by(type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_type <- citywide_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_type_last12 <- chicago_crime_last12 %>%
  group_by(type) %>%
  summarise(last12mos = n())
citywide_type <- left_join(citywide_type,citywide_type_last12,by=c("type"))
# Calculate a total across the 3 prior years
citywide_type$total_prior3years <- citywide_type$total19+citywide_type$total20+citywide_type$total21
citywide_type$avg_prior3years <- round(citywide_type$total_prior3years/3,1)
# add zeros where there were no crimes tallied that year
citywide_type[is.na(citywide_type)] <- 0
# calculate increases
citywide_type$inc_19to21 <- round(citywide_type$total21/citywide_type$total19*100-100,1)
citywide_type$inc_19tolast12 <- round(citywide_type$last12mos/citywide_type$total19*100-100,1)
citywide_type$inc_21tolast12 <- round(citywide_type$last12mos/citywide_type$total21*100-100,1)
citywide_type$inc_prior3yearavgtolast12 <- round((citywide_type$last12mos/citywide_type$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_type$rate19 <- round(citywide_type$total19/chicago_population*100000,1)
citywide_type$rate20 <- round(citywide_type$total20/chicago_population*100000,1)
citywide_type$rate21 <- round(citywide_type$total21/chicago_population*100000,1)
citywide_type$rate_last12 <- round(citywide_type$last12mos/chicago_population*100000,1)
# calculate a multiyear rate
citywide_type$rate_prior3years <- round(citywide_type$avg_prior3years/chicago_population*100000,1)

### CHICAGO POLICE BEAT CRIME TOTALS AND OUTPUT

# MERGE WITH BEATS GEOGRAPHY AND POPULATION
# Geography and populations processed separately in 
# source(process_chicago_areas_map.R)
areas <- st_read("data/source/geo/areas.geojson")

# we need these unique lists for making the beat tables below
# this ensures that we get crime details for beats even with zero
# incidents of certain types over the entirety of the time period
list_area_category <- crossing(community_area = unique(chicago_crime$community_area), category = unique(chicago_crime$category))
list_area_type <- crossing(community_area = unique(chicago_crime$community_area), type = unique(chicago_crime$type))

# Test that all beats show in data and identify beat #s that do not
# areasindata <- chicago_crime %>% group_by(community_area,year) %>% summarise(count=n()) %>% pivot_wider(names_from=year, values_from=count)
# anti_join(areasindata,areas,by="community_area")
# OPEN WORK: Only 1 total record in 2020; we can go back and manually geocode later

# Calculate total of each detailed offense type by community area
area_detailed <- chicago_crime %>%
  group_by(community_area,category,description,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
area_detailed <- area_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
area_detailed_last12 <- chicago_crime_last12 %>%
  group_by(community_area,category,description) %>%
  summarise(last12mos = n())
area_detailed <- left_join(area_detailed,area_detailed_last12,by=c("community_area","category","description"))
rm(area_detailed_last12)
# add zeros where there were no crimes tallied that year
area_detailed[is.na(area_detailed)] <- 0
# Calculate a total across the 3 prior years
area_detailed$total_prior3years <- area_detailed$total19+area_detailed$total20+area_detailed$total21
area_detailed$avg_prior3years <- round(area_detailed$total_prior3years/3,1)
# calculate increases
area_detailed$inc_19to21 <- round(area_detailed$total21/area_detailed$total19*100-100,1)
area_detailed$inc_19tolast12 <- round(area_detailed$last12mos/area_detailed$total19*100-100,1)
area_detailed$inc_21tolast12 <- round(area_detailed$last12mos/area_detailed$total21*100-100,1)
area_detailed$inc_prior3yearavgtolast12 <- round((area_detailed$last12mos/area_detailed$avg_prior3years)*100-100,0)
# add population for beats
area_detailed <- full_join(areas,area_detailed,by=c("community"="community_area"))
# calculate the beat by beat rates PER 1K people
area_detailed$rate19 <- round(area_detailed$total19/area_detailed$population*100000,1)
area_detailed$rate20 <- round(area_detailed$total20/area_detailed$population*100000,1)
area_detailed$rate21 <- round(area_detailed$total21/area_detailed$population*100000,1)
area_detailed$rate_last12 <- round(area_detailed$last12mos/area_detailed$population*100000,1)
# calculate a multiyear rate
area_detailed$rate_prior3years <- round(area_detailed$avg_prior3years/area_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
area_detailed <- area_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
area_detailed <- area_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each category of offense BY POLICE BEAT
area_category <- chicago_crime %>%
  group_by(community_area,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every category_name
area_category <- left_join(list_area_category,area_category,by=c("community_area"="community_area","category"="category"))
# rename the year columns
area_category <- area_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
area_category_last12 <- chicago_crime_last12 %>%
  group_by(community_area,category) %>%
  summarise(last12mos = n())
area_category <- left_join(area_category,area_category_last12,by=c("community_area","category"))
rm(area_category_last12)
# add zeros where there were no crimes tallied that year
area_category[is.na(area_category)] <- 0
# Calculate a total across the 3 prior years
area_category$total_prior3years <- area_category$total19+area_category$total20+area_category$total21
area_category$avg_prior3years <- round(area_category$total_prior3years/3,1)
# calculate increases
area_category$inc_19to21 <- round(area_category$total21/area_category$total19*100-100,1)
area_category$inc_19tolast12 <- round(area_category$last12mos/area_category$total19*100-100,1)
area_category$inc_21tolast12 <- round(area_category$last12mos/area_category$total21*100-100,1)
area_category$inc_prior3yearavgtolast12 <- round((area_category$last12mos/area_category$avg_prior3years)*100-100,0)
# add population for beats
area_category <- full_join(areas,area_category,by=c("community_area"="community_area"))
# calculate the beat by beat rates PER 1K people
area_category$rate19 <- round(area_category$total19/area_category$population*100000,1)
area_category$rate20 <- round(area_category$total20/area_category$population*100000,1)
area_category$rate21 <- round(area_category$total21/area_category$population*100000,1)
area_category$rate_last12 <- round(area_category$last12mos/area_category$population*100000,1)
# calculate a multiyear rate
area_category$rate_prior3years <- round(area_category$avg_prior3years/area_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
area_category <- area_category %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
area_category <- area_category %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each type of crime BY POLICE BEAT
area_type <- chicago_crime %>%
  group_by(community_area,type,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every type
area_type <- left_join(list_area_type,area_type,by=c("community_area"="community_area","type"="type"))
# rename the year columns
area_type <- area_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
area_type_last12 <- chicago_crime_last12 %>%
  group_by(community_area,type) %>%
  summarise(last12mos = n())
area_type <- left_join(area_type,area_type_last12,by=c("community_area","type"))
rm(area_type_last12)
# add zeros where there were no crimes tallied that year
area_type[is.na(area_type)] <- 0
# Calculate a total across the 3 prior years
area_type$total_prior3years <- area_type$total19+area_type$total20+area_type$total21
area_type$avg_prior3years <- round(area_type$total_prior3years/3,1)
# calculate increases
area_type$inc_19to21 <- round(area_type$total21/area_type$total19*100-100,1)
area_type$inc_19tolast12 <- round(area_type$last12mos/area_type$total19*100-100,1)
area_type$inc_21tolast12 <- round(area_type$last12mos/area_type$total21*100-100,1)
area_type$inc_prior3yearavgtolast12 <- round((area_type$last12mos/area_type$avg_prior3years)*100-100,0)
# add population for beats
area_type <- full_join(areas,area_type,by=c("community_area"="community_area"))
# calculate the beat by beat rates PER 1K people
area_type$rate19 <- round(area_type$total19/area_type$population*100000,1)
area_type$rate20 <- round(area_type$total20/area_type$population*100000,1)
area_type$rate21 <- round(area_type$total21/area_type$population*100000,1)
area_type$rate_last12 <- round(area_type$last12mos/area_type$population*100000,1)
# calculate a multiyear rate
area_type$rate_prior3years <- round(area_type$avg_prior3years/area_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
area_type <- area_type %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
area_type <- area_type %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# output various csvs for basic tables to be made with crime totals
# we are dropping geometry for beats here because this is just for tables
area_detailed %>% st_drop_geometry() %>% write_csv("data/output/areas/area_detailed.csv")
area_category %>% st_drop_geometry() %>% write_csv("data/output/areas/area_category.csv")
area_type %>% st_drop_geometry() %>% write_csv("data/output/areas/area_type.csv")
citywide_detailed %>% write_csv("data/output/city/citywide_detailed.csv")
citywide_category %>% write_csv("data/output/city/citywide_category.csv")
citywide_type %>% write_csv("data/output/city/citywide_type.csv")

# Create individual spatial tables of crimes by major categories and types
murders_area <- area_category %>% filter(category=="Murder")
sexassaults_area <- area_category %>% filter(category=="Criminal Sexual Assault")
autothefts_area <- area_category %>% filter(category=="Motor Vehicle Theft")
thefts_area <- area_category %>% filter(category=="Theft Over $500")
burglaries_area <- area_category %>% filter(category=="Burglary")
robberies_area <- area_category %>% filter(category=="Robbery")
batteries_area <- area_category %>% filter(category=="Aggravated Battery")
violence_area <- area_type %>% filter(type=="People")
property_area <- area_type %>% filter(type=="Property")
# Create same set of tables for citywide figures
murders_city <- citywide_detailed %>% filter(category=="Murder")
sexassaults_city <- citywide_category %>% filter(category=="Criminal Sexual Assault")
autothefts_city <- citywide_category %>% filter(category=="Motor Vehicle Theft")
thefts_city <- citywide_category %>% filter(category=="Theft Over $500")
burglaries_city <- citywide_category %>% filter(category=="Burglary")
robberies_city <- citywide_category %>% filter(category=="Robbery")
batteries_city <- citywide_category %>% filter(category=="Aggravated Battery")
violence_city <- citywide_type %>% filter(type=="People")
property_city <- citywide_type %>% filter(type=="Property")

# Using premise to identify the kinds of places where murders happen
where_murders_happen <- chicago_crime %>%
  filter(category=="Murder") %>%
  group_by(year,location_description) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
# Using premise to identify the kinds of places where murders happen
where_murders_happen_last12 <- chicago_crime_last12 %>%
  filter(category=="Murder") %>%
  group_by(location_description) %>%
  summarise(last12=n())
# merge last 12 into the table
where_murders_happen <- full_join(where_murders_happen,where_murders_happen_last12,by="location_description")
# add zeros where there were no crimes tallied that year
where_murders_happen[is.na(where_murders_happen)] <- 0
rm(where_murders_happen_last12)

# Using hour to identify the hours of day when murders happen
when_murders_happen <- chicago_crime %>%
  filter(category=="Murder") %>%
  group_by(hour) %>%
  summarise(count=n()) %>% 
  arrange(hour)
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")
when_murders_happen$timeframe <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight from 9 p.m. to 5 a.m.",
                                           when_murders_happen$hour %in% c("5","6","7","8","9","10","11") ~ "Morning from 5 a.m. to 12 p.m.",
                                           when_murders_happen$hour %in% c("12","13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening from 12 p.m. to 9 p.m.",
                                           TRUE ~ "Other")
when_murders_happen <- when_murders_happen %>%
  group_by(timeframe) %>%
  summarise(total=sum(count))

# Create individual spatial tables of crimes by major categories and types
murders_area %>% st_drop_geometry() %>% write_csv("data/output/areas/murders_area.csv")
sexassaults_area %>% st_drop_geometry() %>% write_csv("data/output/areas/sexassaults_area.csv")
autothefts_area %>% st_drop_geometry() %>% write_csv("data/output/areas/autothefts_area.csv")
thefts_area %>% st_drop_geometry() %>% write_csv("data/output/areas/thefts_area.csv")
burglaries_area %>% st_drop_geometry() %>% write_csv("data/output/areas/burglaries_area.csv")
robberies_area %>% st_drop_geometry() %>% write_csv("data/output/areas/robberies_area.csv")
batteries_area %>% st_drop_geometry() %>% write_csv("data/output/areas/batteries_area.csv")
violence_area %>% st_drop_geometry() %>% write_csv("data/output/areas/violence_area.csv")
property_area %>% st_drop_geometry() %>% write_csv("data/output/areas/property_area.csv")

# TEST TEST TEST OF WHETHER RDS WILL WORK FOR TRACKERS IN AUTOMATION
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(batteries_city,"scripts/rds/batteries_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")

saveRDS(murders_area,"scripts/rds/murders_area.rds")
saveRDS(batteries_area,"scripts/rds/batteries_area.rds")
saveRDS(sexassaults_area,"scripts/rds/sexassaults_area.rds")
saveRDS(autothefts_area,"scripts/rds/autothefts_area.rds")
saveRDS(thefts_area,"scripts/rds/thefts_area.rds")
saveRDS(burglaries_area,"scripts/rds/burglaries_area.rds")
saveRDS(robberies_area,"scripts/rds/robberies_area.rds")

# additional table exports for specific charts
where_murders_happen %>% write_csv("data/output/city/where_murders_happen.csv")
when_murders_happen %>% write_csv("data/output/city/when_murders_happen.csv")

# deaths cause data update for TX specific table
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="IL")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")
