# USPS Change of Address 2018-2021 Merging and Analysis
# Created by: Sonia Torres Rodríguez
# Create date: October 12, 2021
# 
#--------------------------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(dplyr)

#Set working directory
setwd("~/GitHub/COA/prog")

# Choose where to save outputs. Default is output folder in the github repo folder. 
path <- c("../output/")

#Manually adjusted column names to get rid of spaces

# Read in city list and coordinates
year_2018 <- read_csv("../y2018.csv") 
year_2019 <- read_csv("../y2019.csv")
year_2020 <- read_csv("../y2020.csv")
year_2021 <- read_csv("../y2021.csv")

#Create net variables 
year_2018$NET_ZIP <-year_2018$TOTAL_TO_ZIP - year_2018$TOTAL_FROM_ZIP 
year_2018$NET_BUSINESS <- year_2018$TOTAL_BUSINESS_1 - year_2018$TOTAL_BUSINESS 
year_2018$NET_FAMILY <- year_2018$TOTAL_FAMILY_1 - year_2018$TOTAL_FAMILY 
year_2018$NET_INDIVIDUAL <- year_2018$TOTAL_INDIVIDUAL_1 - year_2018$TOTAL_INDIVIDUAL
year_2018$NET_PERM <- year_2018$TOTAL_PERM_1 - year_2018$TOTAL_PERM 
year_2018$NET_TEMP <- year_2018$TOTAL_TEMP_1 - year_2018$TOTAL_TEMP

year_2019$NET_ZIP <-year_2019$TOTAL_TO_ZIP - year_2019$TOTAL_FROM_ZIP 
year_2019$NET_BUSINESS <- year_2019$TOTAL_BUSINESS_1 - year_2019$TOTAL_BUSINESS 
year_2019$NET_FAMILY <- year_2019$TOTAL_FAMILY_1 - year_2019$TOTAL_FAMILY 
year_2019$NET_INDIVIDUAL <- year_2019$TOTAL_INDIVIDUAL_1 - year_2019$TOTAL_INDIVIDUAL
year_2019$NET_PERM <- year_2019$TOTAL_PERM_1 - year_2019$TOTAL_PERM 
year_2019$NET_TEMP <- year_2019$TOTAL_TEMP_1 - year_2019$TOTAL_TEMP

year_2020$NET_ZIP <-year_2020$TOTAL_TO_ZIP - year_2020$TOTAL_FROM_ZIP 
year_2020$NET_BUSINESS <- year_2020$TOTAL_BUSINESS_1 - year_2020$TOTAL_BUSINESS 
year_2020$NET_FAMILY <- year_2020$TOTAL_FAMILY_1 - year_2020$TOTAL_FAMILY 
year_2020$NET_INDIVIDUAL <- year_2020$TOTAL_INDIVIDUAL_1 - year_2020$TOTAL_INDIVIDUAL
year_2020$NET_PERM <- year_2020$TOTAL_PERM_1 - year_2020$TOTAL_PERM 
year_2020$NET_TEMP <- year_2020$TOTAL_TEMP_1 - year_2020$TOTAL_TEMP

year_2021$NET_ZIP <-year_2021$TOTAL_TO_ZIP - year_2021$TOTAL_FROM_ZIP 
year_2021$NET_BUSINESS <- year_2021$TOTAL_BUSINESS_1 - year_2021$TOTAL_BUSINESS 
year_2021$NET_FAMILY <- year_2021$TOTAL_FAMILY_1 - year_2021$TOTAL_FAMILY 
year_2021$NET_INDIVIDUAL <- year_2021$TOTAL_INDIVIDUAL_1 - year_2021$TOTAL_INDIVIDUAL
year_2021$NET_PERM <- year_2021$TOTAL_PERM_1 - year_2021$TOTAL_PERM 
year_2021$NET_TEMP <- year_2021$TOTAL_TEMP_1 - year_2021$TOTAL_TEMP

#Combine into one file with all years, includes monthly counts by zip code
list <- list(year_2018,year_2019,year_2020,year_2021)
do.call("rbind", list)
list.df <- do.call("rbind", lapply(list, as.data.frame)) %>%
  rename(FROM_BUSINESS = TOTAL_BUSINESS) %>%
  rename(FROM_FAMILY = TOTAL_FAMILY) %>%
  rename(FROM_INDIVIDUAL = TOTAL_INDIVIDUAL) %>%
  rename(FROM_PERM = TOTAL_PERM) %>%
  rename(FROM_TEMP = TOTAL_TEMP) %>%
  rename(TO_BUSINESS = TOTAL_BUSINESS_1) %>%
  rename(TO_FAMILY = TOTAL_FAMILY_1) %>%
  rename(TO_INDIVIDUAL = TOTAL_INDIVIDUAL_1) %>%
  rename(TO_PERM = TOTAL_PERM_1) %>%
  rename(TO_TEMP = TOTAL_TEMP_1) 
list.df$ZIPCODE<-as.character(list.df$ZIPCODE)
write.csv(list.df, file.path(path, "COA_monthbyzip.csv"))

#Make annual counts for each year (for destined, not originated, COAs)
yearlycount_2018 <- year_2018 %>% 
  group_by(ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(TOTAL_BUSINESS),
            FROM_FAMILY = sum(TOTAL_FAMILY),
            FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
            FROM_PERM = sum(TOTAL_PERM),
            FROM_TEMP = sum(TOTAL_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TOTAL_BUSINESS_1),
            TO_FAMILY = sum(TOTAL_FAMILY_1),
            TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
            TO_PERM = sum(TOTAL_PERM_1),
            TO_TEMP = sum(TOTAL_TEMP_1),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP)) 
yearlycount_2018$Year <- "2018"

yearlycount_2019 <- year_2019 %>% 
  group_by(ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(TOTAL_BUSINESS),
            FROM_FAMILY = sum(TOTAL_FAMILY),
            FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
            FROM_PERM = sum(TOTAL_PERM),
            FROM_TEMP = sum(TOTAL_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TOTAL_BUSINESS_1),
            TO_FAMILY = sum(TOTAL_FAMILY_1),
            TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
            TO_PERM = sum(TOTAL_PERM_1),
            TO_TEMP = sum(TOTAL_TEMP_1),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP))  
yearlycount_2019$Year <- "2019"

yearlycount_2020 <- year_2020 %>% 
  group_by(ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(TOTAL_BUSINESS),
            FROM_FAMILY = sum(TOTAL_FAMILY),
            FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
            FROM_PERM = sum(TOTAL_PERM),
            FROM_TEMP = sum(TOTAL_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TOTAL_BUSINESS_1),
            TO_FAMILY = sum(TOTAL_FAMILY_1),
            TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
            TO_PERM = sum(TOTAL_PERM_1),
            TO_TEMP = sum(TOTAL_TEMP_1),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP)) 
yearlycount_2020$Year <- "2020"

# Sum the amounts across 2018-2020 
list1 <- list(yearlycount_2018,yearlycount_2019,yearlycount_2020)
do.call("rbind", list1)
list1.df <- do.call("rbind", lapply(list1, as.data.frame)) 
write.csv(list1.df, file.path(path, "check.csv"))
COA_yearly <- list1.df %>% 
  group_by(Year,ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(FROM_BUSINESS),
            FROM_FAMILY = sum(FROM_FAMILY),
            FROM_INDIVIDUAL = sum(FROM_INDIVIDUAL),
            FROM_PERM = sum(FROM_PERM),
            FROM_TEMP = sum(FROM_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TO_BUSINESS),
            TO_FAMILY = sum(TO_FAMILY),
            TO_INDIVIDUAL = sum(TO_INDIVIDUAL),
            TO_PERM = sum(TO_PERM),
            TO_TEMP = sum(TO_TEMP),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP)) 
write.csv(COA_yearly, file.path(path, "COA_yearlybyzip.csv"))

#Report quarterly counts by zip code

#Pulling out the month - creating a column of quarters for 2018-2021
quarter_2018 <- year_2018 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2018$Quarter <- ifelse(quarter_2018$division<=1, 1, 2)
quarter_2018$Quarter <- ifelse(quarter_2018$division>2, 3, quarter_2018$Quarter)
quarter_2018$Quarter <- ifelse(quarter_2018$division>3, 4, quarter_2018$Quarter)

quarter_2019 <- year_2019 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2019$Quarter <- ifelse(quarter_2019$division<=1, 1, 2)
quarter_2019$Quarter <- ifelse(quarter_2019$division>2, 3, quarter_2019$Quarter)
quarter_2019$Quarter <- ifelse(quarter_2019$division>3, 4, quarter_2019$Quarter)

quarter_2020 <- year_2020 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2020$Quarter <- ifelse(quarter_2020$division<=1, 1, 2)
quarter_2020$Quarter <- ifelse(quarter_2020$division>2, 3, quarter_2020$Quarter)
quarter_2020$Quarter <- ifelse(quarter_2020$division>3, 4, quarter_2020$Quarter)

quarter_2021 <- year_2021 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2021$Quarter <- ifelse(quarter_2021$division<=1, 1, 2)
quarter_2021$Quarter <- ifelse(quarter_2021$division>2, 3, quarter_2021$Quarter)
quarter_2021$Quarter <- ifelse(quarter_2021$division>3, 4, quarter_2021$Quarter)

#Creating quarter sums for 2018
quartercount_2018 <- quarter_2018 %>% 
  group_by(Quarter, ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(TOTAL_BUSINESS),
            FROM_FAMILY = sum(TOTAL_FAMILY),
            FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
            FROM_PERM = sum(TOTAL_PERM),
            FROM_TEMP = sum(TOTAL_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TOTAL_BUSINESS_1),
            TO_FAMILY = sum(TOTAL_FAMILY_1),
            TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
            TO_PERM = sum(TOTAL_PERM_1),
            TO_TEMP = sum(TOTAL_TEMP_1),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP))  
quartercount_2018$Year <- "2018"

quartercount_2019 <- quarter_2019 %>% 
  group_by(Quarter, ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(TOTAL_BUSINESS),
            FROM_FAMILY = sum(TOTAL_FAMILY),
            FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
            FROM_PERM = sum(TOTAL_PERM),
            FROM_TEMP = sum(TOTAL_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TOTAL_BUSINESS_1),
            TO_FAMILY = sum(TOTAL_FAMILY_1),
            TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
            TO_PERM = sum(TOTAL_PERM_1),
            TO_TEMP = sum(TOTAL_TEMP_1),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP))
quartercount_2019$Year <- "2019"

quartercount_2020 <- quarter_2020 %>% 
  group_by(Quarter, ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(TOTAL_BUSINESS),
            FROM_FAMILY = sum(TOTAL_FAMILY),
            FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
            FROM_PERM = sum(TOTAL_PERM),
            FROM_TEMP = sum(TOTAL_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TOTAL_BUSINESS_1),
            TO_FAMILY = sum(TOTAL_FAMILY_1),
            TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
            TO_PERM = sum(TOTAL_PERM_1),
            TO_TEMP = sum(TOTAL_TEMP_1),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP))
quartercount_2020$Year <- "2020"

quartercount_2021 <- quarter_2021 %>% 
  group_by(Quarter, ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(TOTAL_BUSINESS),
            FROM_FAMILY = sum(TOTAL_FAMILY),
            FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
            FROM_PERM = sum(TOTAL_PERM),
            FROM_TEMP = sum(TOTAL_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TOTAL_BUSINESS_1),
            TO_FAMILY = sum(TOTAL_FAMILY_1),
            TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
            TO_PERM = sum(TOTAL_PERM_1),
            TO_TEMP = sum(TOTAL_TEMP_1),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP))
quartercount_2021$Year <- "2021"

# Sum the quarter amounts across 2018-2021 
list2 <- list(quartercount_2018,quartercount_2019,quartercount_2020,quartercount_2021)
do.call("rbind", list2)
list2.df <- do.call("rbind", lapply(list2, as.data.frame)) 
write.csv(list2.df, file.path(path, "check.csv"))
COA_quarterly <- list2.df %>% 
  group_by(Year,Quarter,ZIPCODE) %>%
  summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(FROM_BUSINESS),
            FROM_FAMILY = sum(FROM_FAMILY),
            FROM_INDIVIDUAL = sum(FROM_INDIVIDUAL),
            FROM_PERM = sum(FROM_PERM),
            FROM_TEMP = sum(FROM_TEMP),
            TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TO_BUSINESS),
            TO_FAMILY = sum(TO_FAMILY),
            TO_INDIVIDUAL = sum(TO_INDIVIDUAL),
            TO_PERM = sum(TO_PERM),
            TO_TEMP = sum(TO_TEMP),
            NET_ZIP = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP)) 
write.csv(COA_quarterly, file.path(path, "COA_quarterlybyzip.csv"))

##########################################################################################
#Making county estimates with a crosswalk

#Read in crosswalk file
crosswalk <- read_csv("../zip_county_crosswalk.csv") 

#Rename zipcode to match the original data 
 crosswalk <- crosswalk %>% 
   rename(ZIPCODE = ZIP) %>%
   mutate(ZIPCODE = as.numeric(ZIPCODE))
 
year_2019 <- year_2019 %>%
  mutate(ZIPCODE = as.numeric(ZIPCODE))
 
#Left join the original year data to the crosswalk
zcta_join2018 <- year_2018 %>%
  left_join(crosswalk, by="ZIPCODE") 
zcta_join2018$Year <- "2018"

zcta_join2019 <- year_2019 %>%
  left_join(crosswalk, by="ZIPCODE") 
zcta_join2019$Year <- "2019"

zcta_join2020 <- year_2020 %>%
  left_join(crosswalk, by="ZIPCODE") 
zcta_join2020$Year <- "2020"

zcta_join2021 <- year_2021 %>%
  left_join(crosswalk, by="ZIPCODE")
zcta_join2021$Year <- "2021"
 
#Combine the monthly data for 2018-2021 to make monthly estimates
list3 <- list(zcta_join2018,zcta_join2019,zcta_join2020,zcta_join2021)
do.call("rbind", list3)
list3.df <- do.call("rbind", lapply(list3, as.data.frame)) %>%
  rename(FROM_BUSINESS = TOTAL_BUSINESS) %>%
  rename(FROM_FAMILY = TOTAL_FAMILY) %>%
  rename(FROM_INDIVIDUAL = TOTAL_INDIVIDUAL) %>%
  rename(FROM_PERM = TOTAL_PERM) %>%
  rename(FROM_TEMP = TOTAL_TEMP) %>%
  rename(TO_BUSINESS = TOTAL_BUSINESS_1) %>%
  rename(TO_FAMILY = TOTAL_FAMILY_1) %>%
  rename(TO_INDIVIDUAL = TOTAL_INDIVIDUAL_1) %>%
  rename(TO_PERM = TOTAL_PERM_1) %>%
  rename(TO_TEMP = TOTAL_TEMP_1) 
write.csv(list3.df, file.path(path, "check.csv"))
list3.df <- list3.df %>% 
  group_by(YYYYMM, COUNTYNM) %>%
  mutate(TOTAL_FROM_COUNTY = sum(TOTAL_FROM_ZIP),
            FROM_BUSINESS = sum(FROM_BUSINESS),
            FROM_FAMILY = sum(FROM_FAMILY),
            FROM_INDIVIDUAL = sum(FROM_INDIVIDUAL),
            FROM_PERM = sum(FROM_PERM),
            FROM_TEMP = sum(FROM_TEMP),
            TOTAL_TO_COUNTY = sum(TOTAL_TO_ZIP),
            TO_BUSINESS = sum(TO_BUSINESS),
            TO_FAMILY = sum(TO_FAMILY),
            TO_INDIVIDUAL = sum(TO_INDIVIDUAL),
            TO_PERM = sum(TO_PERM),
            TO_TEMP = sum(TO_TEMP),
            NET_COUNTY = sum(NET_ZIP),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP))
list3.df <- list3.df %>% 
  relocate(COUNTYNM, .before = CITY.x) %>% 
  relocate(TOTAL_FROM_COUNTY, .before = FROM_BUSINESS) %>% 
  relocate(TOTAL_TO_COUNTY, .before = TO_BUSINESS) %>%
  relocate(NET_COUNTY, .before = NET_BUSINESS) %>%
  select(-TOTAL_FROM_ZIP) %>%
  select(-TOTAL_TO_ZIP) %>%
  select(-NET_ZIP) %>%
  distinct(COUNTYNM,.keep_all= TRUE)%>%
  rename(COUNTY_NAME = COUNTYNM) 
county_monthly <- list3.df %>%
  select(-ZIPCODE)
write.csv(county_monthly, file.path(path, "COA_monthbycounty.csv"))

#Make yearly estimates by county
county_yearly <- list3.df %>% 
  group_by(Year, COUNTY_NAME) %>%
  mutate(TOTAL_FROM_COUNTY = sum(TOTAL_FROM_COUNTY),
         FROM_BUSINESS = sum(FROM_BUSINESS),
         FROM_FAMILY = sum(FROM_FAMILY),
         FROM_INDIVIDUAL = sum(FROM_INDIVIDUAL),
         FROM_PERM = sum(FROM_PERM),
         FROM_TEMP = sum(FROM_TEMP),
         TOTAL_TO_COUNTY = sum(TOTAL_TO_COUNTY),
         TO_BUSINESS = sum(TO_BUSINESS),
         TO_FAMILY = sum(TO_FAMILY),
         TO_INDIVIDUAL = sum(TO_INDIVIDUAL),
         TO_PERM = sum(TO_PERM),
         TO_TEMP = sum(TO_TEMP),
         NET_COUNTY = sum(NET_COUNTY),
         NET_BUSINESS = sum(NET_BUSINESS),
         NET_FAMILY = sum(NET_FAMILY),
         NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
         NET_PERM = sum(NET_PERM),
         NET_TEMP = sum(NET_TEMP))

county_yearly <- county_yearly %>%
  select(-ZIPCODE) 

write.csv(county_yearly, file.path(path, "COA_yearbycounty.csv"))

#Make quarterly estimates by county
quarter <- list3.df %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter$Quarter <- ifelse(quarter$division<=1, 1, 2)
quarter$Quarter <- ifelse(quarter$division>2, 3, quarter$Quarter)
quarter$Quarter <- ifelse(quarter$division>3, 4, quarter$Quarter)

quarter_county <- quarter %>% 
  group_by(Year,Quarter,COUNTY_NAME) %>%
  mutate(TOTAL_FROM_COUNTY = sum(TOTAL_FROM_COUNTY),
            FROM_BUSINESS = sum(FROM_BUSINESS),
            FROM_FAMILY = sum(FROM_FAMILY),
            FROM_INDIVIDUAL = sum(FROM_INDIVIDUAL),
            FROM_PERM = sum(FROM_PERM),
            FROM_TEMP = sum(FROM_TEMP),
            TOTAL_TO_COUNTY = sum(TOTAL_TO_COUNTY),
            TO_BUSINESS = sum(TO_BUSINESS),
            TO_FAMILY = sum(TO_FAMILY),
            TO_INDIVIDUAL = sum(TO_INDIVIDUAL),
            TO_PERM = sum(TO_PERM),
            TO_TEMP = sum(TO_TEMP),
            NET_COUNTY = sum(NET_COUNTY),
            NET_BUSINESS = sum(NET_BUSINESS),
            NET_FAMILY = sum(NET_FAMILY),
            NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP)) 
write.csv(quarter_county, file.path(path, "COA_quarterlybycounty.csv"))

######################################################################################
#Create percentages:

#Get denominator of addresses by zipcode and county, create rates of mobility
#tidy census total households by zcta

#Validate with other data



##########################################################################################
# #Take zipcode estimates to county level (crosswalk and spatial join)
# library(tigris)
# library(ggplot2)
# library(urbnmapr)
# library(urbnthemes)
# library(sf)
# set_urbn_defaults(style = "map")
# 
# #Pull zcta shape files - make an sf object
# options(tigris_use_cache = FALSE)
# zcta <- zctas(cb = FALSE, starts_with = NULL, year = NULL, state = NULL)
# #zcta_2018<- st_as_sf(zcta_2018)
# 
# #Pull county shape files - make into sf objects
# counties <-counties(cb = FALSE, year = NULL, state = NULL) 
# 
# #Left join usps data to zcta shape file (need to rename and mutate)
# year_2018 <- year_2018 %>% 
#  rename(ZCTA5CE10 = ZIPCODE) %>%
#   mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
# 
# year_2019 <- year_2019 %>% 
#   rename(ZCTA5CE10 = ZIPCODE) %>%
#   mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
# 
# year_2020 <- year_2020 %>% 
#   rename(ZCTA5CE10 = ZIPCODE) %>%
#   mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
# 
# #Left join the original year data to the zcta data
# zcta_join2018 <- zcta %>%
#   left_join(year_2018, by="ZCTA5CE10")
# 
# zcta_join2019 <- zcta %>%
#   left_join(year_2019, by="ZCTA5CE10")
# 
# zcta_join2020 <- zcta %>%
#   left_join(year_2020, by="ZCTA5CE10")
# 
# #Join this new zcta + year data with county - transform to have the same crs - %>% st_tranform(4326)
# zctacounty_2018 <- zcta_join2018  %>%
#   st_join(counties, join = st_intersects) 
# 
# zctacounty_2019 <- zcta_join2019  %>%
#   st_join(counties, join = st_intersects)
# 
# zctacounty_2020 <- zcta_join2020  %>%
#   st_join(counties, join = st_intersects) 
# 
# #Step 5 - monthly counts
# list3 <- list(zctacounty_2018,zctacounty_2019,zctacounty_2020)
# do.call("rbind", list3)
# list3.df <- do.call("rbind", lapply(list3, as.data.frame)) 
# list3.df <- list3.df %>% 
#   group_by(NAMELSAD) %>%
#   summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
#             FROM_BUSINESS = sum(TOTAL_BUSINESS),
#             FROM_FAMILY = sum(TOTAL_FAMILY),
#             FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
#             FROM_PERM = sum(TOTAL_PERM),
#             FROM_TEMP = sum(TOTAL_TEMP),
#             TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
#             TO_BUSINESS = sum(TOTAL_BUSINESS_1),
#             TO_FAMILY = sum(TOTAL_FAMILY_1),
#             TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
#             TO_PERM = sum(TOTAL_PERM_1),
#             TO_TEMP = sum(TOTAL_TEMP_1),
#             NET_ZIP = sum(NET_ZIP),
#             NET_BUSINESS = sum(NET_BUSINESS),
#             NET_FAMILY = sum(NET_FAMILY),
#             NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
#             NET_PERM = sum(NET_PERM),
#             NET_TEMP = sum(NET_TEMP))
# 
# write.csv(list3.df, file.path(path, "COA_monthbycounty.csv"))
# 
# #step 6 - group by county, summarize yearly counts - aggregation?
# yearcounty_2018 <- zctacounty_2018 %>% 
#   group_by(NAMELSAD) %>%
#   summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
#             FROM_BUSINESS = sum(TOTAL_BUSINESS),
#             FROM_FAMILY = sum(TOTAL_FAMILY),
#             FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
#             FROM_PERM = sum(TOTAL_PERM),
#             FROM_TEMP = sum(TOTAL_TEMP),
#             TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
#             TO_BUSINESS = sum(TOTAL_BUSINESS_1),
#             TO_FAMILY = sum(TOTAL_FAMILY_1),
#             TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
#             TO_PERM = sum(TOTAL_PERM_1),
#             TO_TEMP = sum(TOTAL_TEMP_1),
#             NET_ZIP = sum(NET_ZIP),
#             NET_BUSINESS = sum(NET_BUSINESS),
#             NET_FAMILY = sum(NET_FAMILY),
#             NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
#             NET_PERM = sum(NET_PERM),
#             NET_TEMP = sum(NET_TEMP))
# yearcounty_2018$Year <- "2018"
# 
# yearcounty_2019 <- zctacounty_2019 %>% 
#   group_by(NAMELSAD) %>%
#   summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
#             FROM_BUSINESS = sum(TOTAL_BUSINESS),
#             FROM_FAMILY = sum(TOTAL_FAMILY),
#             FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
#             FROM_PERM = sum(TOTAL_PERM),
#             FROM_TEMP = sum(TOTAL_TEMP),
#             TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
#             TO_BUSINESS = sum(TOTAL_BUSINESS_1),
#             TO_FAMILY = sum(TOTAL_FAMILY_1),
#             TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
#             TO_PERM = sum(TOTAL_PERM_1),
#             TO_TEMP = sum(TOTAL_TEMP_1),
#             NET_ZIP = sum(NET_ZIP),
#             NET_BUSINESS = sum(NET_BUSINESS),
#             NET_FAMILY = sum(NET_FAMILY),
#             NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
#             NET_PERM = sum(NET_PERM),
#             NET_TEMP = sum(NET_TEMP))
# yearcounty_2019$Year <- "2019"
# 
# yearcounty_2020 <- zctacounty_2020 %>% 
#   group_by(NAMELSAD) %>%
#   summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
#             FROM_BUSINESS = sum(TOTAL_BUSINESS),
#             FROM_FAMILY = sum(TOTAL_FAMILY),
#             FROM_INDIVIDUAL = sum(TOTAL_INDIVIDUAL),
#             FROM_PERM = sum(TOTAL_PERM),
#             FROM_TEMP = sum(TOTAL_TEMP),
#             TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
#             TO_BUSINESS = sum(TOTAL_BUSINESS_1),
#             TO_FAMILY = sum(TOTAL_FAMILY_1),
#             TO_INDIVIDUAL = sum(TOTAL_INDIVIDUAL_1),
#             TO_PERM = sum(TOTAL_PERM_1),
#             TO_TEMP = sum(TOTAL_TEMP_1),
#             NET_ZIP = sum(NET_ZIP),
#             NET_BUSINESS = sum(NET_BUSINESS),
#             NET_FAMILY = sum(NET_FAMILY),
#             NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
#             NET_PERM = sum(NET_PERM),
#             NET_TEMP = sum(NET_TEMP))
# yearcounty_2020$Year <- "2020"
# 
# #Join into one data frame
# list4 <- list(yearcounty_2018,yearcounty_2019,yearcounty_2020)
# do.call("rbind", list4)
# list4.df <- do.call("rbind", lapply(list4, as.data.frame)) 
# write.csv(list4.df, file.path(path, "check.csv"))
# COA_yearbycounty <- list4.df %>% 
#   group_by(Year,NAMELSAD) %>%
#   summarise(TOTAL_FROM_ZIP = sum(TOTAL_FROM_ZIP),
#             FROM_BUSINESS = sum(FROM_BUSINESS),
#             FROM_FAMILY = sum(FROM_FAMILY),
#             FROM_INDIVIDUAL = sum(FROM_INDIVIDUAL),
#             FROM_PERM = sum(FROM_PERM),
#             FROM_TEMP = sum(FROM_TEMP),
#             TOTAL_TO_ZIP = sum(TOTAL_TO_ZIP),
#             TO_BUSINESS = sum(TO_BUSINESS),
#             TO_FAMILY = sum(TO_FAMILY),
#             TO_INDIVIDUAL = sum(TO_INDIVIDUAL),
#             TO_PERM = sum(TO_PERM),
#             TO_TEMP = sum(TO_TEMP),
#             NET_ZIP = sum(NET_ZIP),
#             NET_BUSINESS = sum(NET_BUSINESS),
#             NET_FAMILY = sum(NET_FAMILY),
#             NET_INDIVIDUAL = sum(NET_INDIVIDUAL),
#             NET_PERM = sum(NET_PERM),
#             NET_TEMP = sum(NET_TEMP))
# write.csv(COA_yearbycounty, file.path(path, "COA_yearlybycounty.csv"))

#quarterly counts


#pivot wider function - get everything in one place and break into different columns for months and quarters 

#Get denominator of addresses by zipcode and county, create rates of mobility
#tidy census total households by zcta

#Validate with other data


