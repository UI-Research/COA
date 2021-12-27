# USPS Change of Address 2018-2021 Open Data File
# Created by: Sonia Torres Rodriguez
# Create date: December 17, 2021
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

#Manually adjusted column names to get rid of spaces. Named the "to" variables with a "_1" name

# Read in city list and coordinates
year_2018 <- read.csv("../y2018.csv", check.names = FALSE) 
year_2019 <- read.csv("../y2019.csv", check.names = FALSE)
year_2020 <- read.csv("../y2020.csv", check.names = FALSE)
year_2021 <- read.csv("../y2021.csv", check.names = FALSE)

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

#Create net residential variables
year_2018$NET_RESIDENTIAL <- year_2018$NET_FAMILY + year_2018$NET_INDIVIDUAL
year_2019$NET_RESIDENTIAL <- year_2019$NET_FAMILY + year_2019$NET_INDIVIDUAL
year_2020$NET_RESIDENTIAL <- year_2020$NET_FAMILY + year_2020$NET_INDIVIDUAL
year_2021$NET_RESIDENTIAL <- year_2021$NET_FAMILY + year_2021$NET_INDIVIDUAL

#Only keep variables of interest (net_zip, net_perm, net_temp, net_residential)
year_2018 <- year_2018 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE,NET_ZIP, NET_PERM, NET_TEMP, NET_RESIDENTIAL)

year_2019 <- year_2019 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE,NET_ZIP, NET_PERM, NET_TEMP, NET_RESIDENTIAL)

year_2020 <- year_2020 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE,NET_ZIP, NET_PERM, NET_TEMP, NET_RESIDENTIAL)

year_2021 <- year_2021 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE,NET_ZIP, NET_PERM, NET_TEMP, NET_RESIDENTIAL)

#Join data frames of all years
list <- list(year_2018,year_2019,year_2020,year_2021)
do.call("rbind", list)
COA_monthly_zip <- do.call("rbind", lapply(list, as.data.frame)) 

COA_monthly_zip %>%
  count(YYYYMM)

#Adjust type to be able to transpose
COA_monthly_zip <- COA_monthly_zip %>%
  mutate(YYYYMM = as.numeric(YYYYMM),
         NET_ZIP = as.numeric(NET_ZIP),
         NET_PERM = as.numeric(NET_PERM),
         NET_TEMP = as.numeric(NET_TEMP),
         NET_RESIDENTIAL = as.numeric(NET_RESIDENTIAL), 
         #This is the zipcode padding to make standardize zipcode format so pivot_wider works
         ZIPCODE = str_pad(ZIPCODE, 5, side = c("left"), pad = "0"))%>% 
  relocate(ZIPCODE, .before = YYYYMM)

write.csv(COA_monthly_zip, file.path(path, "check.csv"))

#Transposing for each variable and merging again
NET_ZIP <- COA_monthly_zip %>%
  select(ZIPCODE, CITY, STATE, YYYYMM, NET_ZIP) %>% 
  pivot_wider(names_from = YYYYMM, values_from = NET_ZIP, names_prefix = "NET_ZIP_")

NET_PERM <- COA_monthly_zip %>%
  select(ZIPCODE, CITY, STATE, YYYYMM, NET_PERM) %>%
  pivot_wider(names_from = YYYYMM, values_from = NET_PERM, names_prefix = "NET_PERM_") 
  
NET_TEMP <- COA_monthly_zip %>%
  select(ZIPCODE,CITY, STATE, YYYYMM, NET_TEMP) %>%
  pivot_wider(names_from = YYYYMM, values_from = NET_TEMP, names_prefix = "NET_TEMP_") 

NET_RES <- COA_monthly_zip%>%
  select(ZIPCODE,CITY, STATE,YYYYMM, NET_RESIDENTIAL) %>%
  pivot_wider(names_from = YYYYMM, values_from = NET_RESIDENTIAL, names_prefix = "NET_RES_")

#Merging the short data sets to create one data file
open_data_file <- NET_ZIP %>% 
  full_join(NET_PERM, by = "ZIPCODE")%>% 
  full_join(NET_TEMP, by = "ZIPCODE")%>% 
  full_join(NET_RES, by = "ZIPCODE") %>% 
  distinct(ZIPCODE,.keep_all= TRUE) %>% 
  select(-CITY.y, -STATE.y, -CITY.x.x, -STATE.x.x,-CITY.y.y, -STATE.y.y) %>% 
  rename(CITY = CITY.x) %>%
  rename(STATE = STATE.x)
write.csv(open_data_file, file.path(path, "COA_opendatafile.csv"))

####
#Validating USPS COA counts with census data

library(tidycensus)
library(tidyverse)

# Use Census API key
census_api_key("e0e50dbbc586496e193c885ba478cae24f3150b3", overwrite = FALSE, install = FALSE)

#

