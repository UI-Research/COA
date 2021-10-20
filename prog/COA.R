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
#Sort by zipcode to report all years together
year_2018 %>% arrange(ZIPCODE) 

#Attempt 1- Create month vector 1-12 - Issue: 29768*12 = 357216, but there's less rows than that in year_2018
#month <- rep(c(1,2,3,4,5,6,7,8,9,10,11,12), times=29768)
#year_2018$Month <- month

#Attempt 2 - Make a new column based off month in YYYYMM - Issue: Error in as.Date.default(., YYYYMM, format = "%Y%m%d") : do not know how to convert '.' to class "Date"
#year_2018 %>% as.Date(YYYYMM, format = "%Y%m")
#Group months by quarters some how

#Take these yearly, quarterly, monthly zipcode counts, and estimate for county level (crosswalk)

#Get denominator of addresses by zipcode and county, create rates of mobility

#Validate with other data


