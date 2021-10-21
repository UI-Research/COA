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

#Combine into one file with all years, includes monthly counts by zip code
list <- list(year_2018,year_2019,year_2020,year_2021)
do.call("rbind", list)
list.df <- do.call("rbind", lapply(list, as.data.frame)) 
write.csv(list.df, file.path(path, "COA_monthbyzip.csv"))

#Check - table(year2018$YYYYMM)

#Make annual counts for each year (for destined, not originated, COAs)
yearlycount_2018 <- year_2018 %>% 
  group_by(ZIPCODE) %>%
  summarise(sum_total = sum(TOTAL_FROM_ZIP),
            sum_business = sum(TOTAL_BUSINESS),
            sum_family = sum(TOTAL_FAMILY),
            sum_individual = sum(TOTAL_INDIVIDUAL),
            sum_perm = sum(TOTAL_PERM),
            sum_temp = sum(TOTAL_TEMP)) 
yearlycount_2018$Year <- "2018"

yearlycount_2019 <- year_2019 %>% 
  group_by(ZIPCODE) %>%
  summarise(sum_total = sum(TOTAL_FROM_ZIP),
            sum_business = sum(TOTAL_BUSINESS),
            sum_family = sum(TOTAL_FAMILY),
            sum_individual = sum(TOTAL_INDIVIDUAL),
            sum_perm = sum(TOTAL_PERM),
            sum_temp = sum(TOTAL_TEMP))
yearlycount_2019$Year <- "2019"

yearlycount_2020 <- year_2020 %>% 
  group_by(ZIPCODE) %>%
  summarise(sum_total = sum(TOTAL_FROM_ZIP),
            sum_business = sum(TOTAL_BUSINESS),
            sum_family = sum(TOTAL_FAMILY),
            sum_individual = sum(TOTAL_INDIVIDUAL),
            sum_perm = sum(TOTAL_PERM),
            sum_temp = sum(TOTAL_TEMP))
yearlycount_2020$Year <- "2020"

# Sum the amounts across 2018-2020 
list1 <- list(yearlycount_2018,yearlycount_2019,yearlycount_2020)
do.call("rbind", list1)
list1.df <- do.call("rbind", lapply(list1, as.data.frame)) 
write.csv(list1.df, file.path(path, "check.csv"))
COA_yearly <- list1.df %>% 
  group_by(Year,ZIPCODE) %>%
  summarise(total = sum(sum_total),
            business = sum(sum_business),
            family = sum(sum_family),
            individual = sum(sum_individual),
            perm = sum(sum_perm),
            temp = sum(sum_temp))
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
  summarise(sum_total = sum(TOTAL_FROM_ZIP),
            sum_business = sum(TOTAL_BUSINESS),
            sum_family = sum(TOTAL_FAMILY),
            sum_individual = sum(TOTAL_INDIVIDUAL),
            sum_perm = sum(TOTAL_PERM),
            sum_temp = sum(TOTAL_TEMP)) 
quartercount_2018$Year <- "2018"

quartercount_2019 <- quarter_2019 %>% 
  group_by(Quarter, ZIPCODE) %>%
  summarise(sum_total = sum(TOTAL_FROM_ZIP),
            sum_business = sum(TOTAL_BUSINESS),
            sum_family = sum(TOTAL_FAMILY),
            sum_individual = sum(TOTAL_INDIVIDUAL),
            sum_perm = sum(TOTAL_PERM),
            sum_temp = sum(TOTAL_TEMP)) 
quartercount_2019$Year <- "2019"

quartercount_2020 <- quarter_2020 %>% 
  group_by(Quarter, ZIPCODE) %>%
  summarise(sum_total = sum(TOTAL_FROM_ZIP),
            sum_business = sum(TOTAL_BUSINESS),
            sum_family = sum(TOTAL_FAMILY),
            sum_individual = sum(TOTAL_INDIVIDUAL),
            sum_perm = sum(TOTAL_PERM),
            sum_temp = sum(TOTAL_TEMP)) 
quartercount_2020$Year <- "2020"

quartercount_2021 <- quarter_2021 %>% 
  group_by(Quarter, ZIPCODE) %>%
  summarise(sum_total = sum(TOTAL_FROM_ZIP),
            sum_business = sum(TOTAL_BUSINESS),
            sum_family = sum(TOTAL_FAMILY),
            sum_individual = sum(TOTAL_INDIVIDUAL),
            sum_perm = sum(TOTAL_PERM),
            sum_temp = sum(TOTAL_TEMP)) 
quartercount_2021$Year <- "2021"

# Sum the quarter amounts across 2018-2021 
list2 <- list(quartercount_2018,quartercount_2019,quartercount_2020,quartercount_2021)
do.call("rbind", list2)
list2.df <- do.call("rbind", lapply(list2, as.data.frame)) 
write.csv(list2.df, file.path(path, "check.csv"))
COA_quarterly <- list2.df %>% 
  group_by(Year,Quarter,ZIPCODE) %>%
  summarise(total = sum(sum_total),
            business = sum(sum_business),
            family = sum(sum_family),
            individual = sum(sum_individual),
            perm = sum(sum_perm),
            temp = sum(sum_temp))
write.csv(COA_quarterly, file.path(path, "COA_quarterlybyzip.csv"))

##########################################################################################
#Take zipcode estimates to county level (crosswalk and spatial join)
library(tigris)
library(ggplot2)
library(urbnmapr)
library(urbnthemes)
library(sf)
set_urbn_defaults(style = "map")

#Pull zcta shape files - make an sf object
zcta_2018 <- zctas(cb = FALSE, starts_with = NULL, year = 2018, state = NULL)
zcta_2018<- st_as_sf(zcta_2018)

zcta_2019 <-zctas(cb = FALSE, starts_with = NULL, year = 2019, state = NULL)
zcta_2019<- st_as_sf(zcta_2019)

zcta_2020 <-zctas(cb = FALSE, starts_with = NULL, year = 2020, state = NULL)
zcta_2020<- st_as_sf(zcta_2020)

zcta_2021 <-zctas(cb = FALSE, starts_with = NULL, year = 2021, state = NULL)
zcta_2021<- st_as_sf(zcta_2021)

#Pull county shape files - make into sf objects
counties_2018 <-counties(cb = FALSE, year = 2018, state = NULL) 
counties_2018<- st_as_sf(counties_2018)

counties_2019 <-counties(cb = FALSE, year = 2019, state = NULL) 
counties_2019<- st_as_sf(counties_2019)

counties_2020 <-counties(cb = FALSE, year = 2020, state = NULL)
counties_2020<- st_as_sf(counties_2020)

counties_2021 <-counties(cb = FALSE, year = 2021, state = NULL) 
counties_2021<- st_as_sf(counties_2021)

#Left join usps data to zcta shape file
rename(year_2018, ZCTA5CE10=ZIPCODE)
rename(year_2019, ZCTA5CE10=ZIPCODE)
rename(year_2020, ZCTA5CE10=ZIPCODE)
rename(year_2021, ZCTA5CE10=ZIPCODE)

zctajoin_2018 <- st_join(year_2018, zcta_2018, join = st_intersects)

#step 4 - spatial joining joined zcta to counties 

#step 5 - group by county, summarize key counts - aggregation?

#pivot wider function - get everything in one place and break into different columns for months and quarters 

#Get denominator of addresses by zipcode and county, create rates of mobility
#tidy census total households by zzta

#Validate with other data


