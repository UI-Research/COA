# USPS Change of Address 2018-2021 Open Data for Quarters with Vacancy Data
# Created by: Sonia Torres Rodriguez
# Create date: December 21, 2021
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

#Pulling out the month - creating a column of quarters for 2018-2021
quarter_2018 <- year_2018 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2018$Quarter <- ifelse(quarter_2018$division<=1, 1, 2)
quarter_2018$Quarter <- ifelse(quarter_2018$division>2, 3, quarter_2018$Quarter)
quarter_2018$Quarter <- ifelse(quarter_2018$division>3, 4, quarter_2018$Quarter)
quarter_2018$Year <- "2018"

quarter_2019 <- year_2019 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2019$Quarter <- ifelse(quarter_2019$division<=1, 1, 2)
quarter_2019$Quarter <- ifelse(quarter_2019$division>2, 3, quarter_2019$Quarter)
quarter_2019$Quarter <- ifelse(quarter_2019$division>3, 4, quarter_2019$Quarter)
quarter_2019$Year <- "2019"

quarter_2020 <- year_2020 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2020$Quarter <- ifelse(quarter_2020$division<=1, 1, 2)
quarter_2020$Quarter <- ifelse(quarter_2020$division>2, 3, quarter_2020$Quarter)
quarter_2020$Quarter <- ifelse(quarter_2020$division>3, 4, quarter_2020$Quarter)
quarter_2020$Year <- "2020"

quarter_2021 <- year_2021 %>%
  mutate(Month=as.numeric(substring(YYYYMM, 5,6)), 
         division = Month/3) 
quarter_2021$Quarter <- ifelse(quarter_2021$division<=1, 1, 2)
quarter_2021$Quarter <- ifelse(quarter_2021$division>2, 3, quarter_2021$Quarter)
quarter_2021$Quarter <- ifelse(quarter_2021$division>3, 4, quarter_2021$Quarter)
quarter_2021$Year <- "2021"

#Create summary counts by quarter
quarter_2018 <- quarter_2018 %>% 
  group_by(Quarter, ZIPCODE) %>%
  mutate(NET_ZIP = sum(NET_ZIP),
            NET_PERM = sum(NET_PERM),
            NET_TEMP = sum(NET_TEMP),
            NET_RESIDENTIAL = sum(NET_RESIDENTIAL)) %>% 
  mutate(ZIPCODE = as.numeric(ZIPCODE))%>% 
  relocate(Year, Quarter, .before = ZIPCODE) %>% 
  distinct(ZIPCODE, .keep_all = TRUE) %>% 
  select(-YYYYMM, -Month, -division)

quarter_2019 <- quarter_2019 %>% 
  group_by(Quarter, ZIPCODE) %>%
  mutate(NET_ZIP = sum(NET_ZIP),
         NET_PERM = sum(NET_PERM),
         NET_TEMP = sum(NET_TEMP),
         NET_RESIDENTIAL = sum(NET_RESIDENTIAL)) %>% 
  mutate(ZIPCODE = as.numeric(ZIPCODE))%>% 
  relocate(Year, Quarter, .before = ZIPCODE) %>% 
  distinct(ZIPCODE, .keep_all = TRUE) %>% 
  select(-YYYYMM, -Month, -division)

quarter_2020 <- quarter_2020 %>% 
  group_by(Quarter, ZIPCODE) %>%
  mutate(NET_ZIP = sum(NET_ZIP),
         NET_PERM = sum(NET_PERM),
         NET_TEMP = sum(NET_TEMP),
         NET_RESIDENTIAL = sum(NET_RESIDENTIAL)) %>% 
  mutate(ZIPCODE = as.numeric(ZIPCODE))%>% 
  relocate(Year, Quarter, .before = ZIPCODE) %>% 
  distinct(ZIPCODE, .keep_all = TRUE) %>% 
  select(-YYYYMM, -Month, -division)

quarter_2021 <- quarter_2021 %>% 
  group_by(Quarter, ZIPCODE) %>%
  mutate(NET_ZIP = sum(NET_ZIP),
         NET_PERM = sum(NET_PERM),
         NET_TEMP = sum(NET_TEMP),
         NET_RESIDENTIAL = sum(NET_RESIDENTIAL)) %>% 
  mutate(ZIPCODE = as.numeric(ZIPCODE))%>% 
  relocate(Year, Quarter, .before = ZIPCODE) %>% 
  distinct(ZIPCODE, .keep_all = TRUE) %>% 
  select(-YYYYMM, -Month, -division)

#Join data frames of all years
list <- list(quarter_2018,quarter_2019,quarter_2020,quarter_2021)
do.call("rbind", list)
COA_quarterly_zip <- do.call("rbind", lapply(list, as.data.frame)) 

#Adjust type to be able to transpose
COA_quarterly_zip <- COA_quarterly_zip  %>%
  mutate(NET_ZIP = as.numeric(NET_ZIP),
         NET_PERM = as.numeric(NET_PERM),
         NET_TEMP = as.numeric(NET_TEMP),
         NET_RESIDENTIAL = as.numeric(NET_RESIDENTIAL), 
         #This is the zipcode padding to make standardize zipcode format so pivot_wider works
         ZIPCODE = str_pad(ZIPCODE, 5, side = c("left"), pad = "0"),
         Quarter = str_pad(Quarter, 2, side = c("left"), pad = "0"))

#Create new variable joining year and quarter
COA_quarterly_zip$YYYYQQ <- paste(COA_quarterly_zip$Year,COA_quarterly_zip$Quarter, sep= "")
COA_quarterly_zip <- COA_quarterly_zip  %>%
  relocate(YYYYQQ, .before = ZIPCODE) %>%
  select(-Year, -Quarter)

write.csv(COA_quarterly_zip, file.path(path, "check.csv"))

#Transposing for each variable and merging again
NET_ZIP <- COA_quarterly_zip %>%
  select(ZIPCODE, CITY, STATE, YYYYQQ, NET_ZIP) %>% 
  pivot_wider(names_from = YYYYQQ, values_from = NET_ZIP, names_prefix = "NET_ZIP_")

NET_PERM <- COA_quarterly_zip %>%
  select(ZIPCODE, CITY, STATE, YYYYQQ, NET_PERM) %>%
  pivot_wider(names_from = YYYYQQ, values_from = NET_PERM, names_prefix = "NET_PERM_") 

NET_TEMP <- COA_quarterly_zip %>%
  select(ZIPCODE,CITY, STATE, YYYYQQ, NET_TEMP) %>%
  pivot_wider(names_from = YYYYQQ, values_from = NET_TEMP, names_prefix = "NET_TEMP_") 

NET_RES <- COA_quarterly_zip %>%
  select(ZIPCODE,CITY, STATE,YYYYQQ, NET_RESIDENTIAL) %>%
  pivot_wider(names_from = YYYYQQ, values_from = NET_RESIDENTIAL, names_prefix = "NET_RES_")

#Merging the short data sets to create one data file
COA_quarterly <- NET_ZIP %>% 
  full_join(NET_PERM, by = "ZIPCODE")%>% 
  full_join(NET_TEMP, by = "ZIPCODE")%>% 
  full_join(NET_RES, by = "ZIPCODE") %>% 
  distinct(ZIPCODE,.keep_all= TRUE) %>% 
  select(-CITY.y, -STATE.y, -CITY.x.x, -STATE.x.x,-CITY.y.y, -STATE.y.y,) %>% 
  rename(CITY = CITY.x) %>%
  rename(STATE = STATE.x)

############################################################################
#Merging in the USPS vacancy data 
library(foreign)

#Read in HUD vacancy data (in quarters) 
#Key variable of interest: AMS_RES, total count of residential addresses; AMS_BUS, total count of businesses 
#Note: Excluded q3 in 2021 because HUD data is incomplete
vacancy_q1_2018 <- read.dbf("../usps_vac_032018.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q1_2018$Year <- "2018"
vacancy_q1_2018$Quarter <- "1"

vacancy_q2_2018 <- read.dbf("../usps_vac_062018.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q2_2018$Year <- "2018"
vacancy_q2_2018$Quarter <- "2"

vacancy_q3_2018 <- read.dbf("../usps_vac_092018.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q3_2018$Year <- "2018"
vacancy_q3_2018$Quarter <- "3"

vacancy_q4_2018 <- read.dbf("../usps_vac_122018.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q4_2018$Year <- "2018"
vacancy_q4_2018$Quarter <- "4"

vacancy_q1_2019 <- read.dbf("../usps_vac_032019.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q1_2019$Year <- "2019"
vacancy_q1_2019$Quarter <- "1"

vacancy_q2_2019 <- read.dbf("../usps_vac_062019.dbf")%>%
  select(geoid,ams_res, ams_bus,ams_oth) 
vacancy_q2_2019$Year <- "2019"
vacancy_q2_2019$Quarter <- "2"

vacancy_q3_2019 <- read.dbf("../usps_vac_092019.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q3_2019$Year <- "2019"
vacancy_q3_2019$Quarter <- "3"

vacancy_q4_2019 <- read.dbf("../usps_vac_122019.dbf")%>%
  select(geoid,ams_res, ams_bus,ams_oth) 
vacancy_q4_2019$Year <- "2019"
vacancy_q4_2019$Quarter <- "4"

vacancy_q1_2020 <- read.dbf("../usps_vac_032020.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q1_2020$Year <- "2020"
vacancy_q1_2020$Quarter <- "1"

vacancy_q2_2020 <- read.dbf("../usps_vac_062020.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth) 
vacancy_q2_2020$Year <- "2020"
vacancy_q2_2020$Quarter <- "2"

vacancy_q3_2020 <- read.dbf("../usps_vac_092020.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q3_2020$Year <- "2020"
vacancy_q3_2020$Quarter <- "3"

vacancy_q4_2020 <- read.dbf("../usps_vac_122020.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q4_2020$Year <- "2020"
vacancy_q4_2020$Quarter <- "4"

vacancy_q1_2021 <- read.dbf("../usps_vac_032021.dbf") %>%
  select(geoid,ams_res, ams_bus,ams_oth)
vacancy_q1_2021$Year <- "2021"
vacancy_q1_2021$Quarter <- "1"

vacancy_q2_2021 <- read.dbf("../usps_vac_062021.dbf")%>%
  select(geoid,ams_res, ams_bus,ams_oth) 
vacancy_q2_2021$Year <- "2021"
vacancy_q2_2021$Quarter <- "2"

#Roll Up the quarterly data
list4 <- list(vacancy_q1_2018,vacancy_q2_2018,vacancy_q3_2018,vacancy_q4_2018, vacancy_q1_2019,vacancy_q2_2019,vacancy_q3_2019,vacancy_q4_2019,vacancy_q1_2020,vacancy_q2_2020,vacancy_q3_2020,vacancy_q4_2020,vacancy_q1_2021,vacancy_q2_2021)
do.call("rbind", list4)
total_addresses_quarters <- do.call("rbind", lapply(list4, as.data.frame))

#Create new variable joining year and quarter
total_addresses_quarters <- total_addresses_quarters  %>%
  mutate(Quarter = str_pad(Quarter, 2, side = c("left"), pad = "0"))

total_addresses_quarters$YYYYQQ <- paste(total_addresses_quarters$Year,total_addresses_quarters$Quarter, sep= "")
total_addresses_quarters <- total_addresses_quarters  %>%
  relocate(YYYYQQ, .before = geoid) 

#Create new variable for total addresses by census tract
total_addresses_quarters$ams_total <- total_addresses_quarters$ams_res + total_addresses_quarters$ams_bus + total_addresses_quarters$ams_oth

#Crosswalk of zipcode and census tract data
#Read in crosswalk file
tract_crosswalk <- read_csv("../tract_zip_crosswalk.csv") 

#Rename zipcode to match the original data 
tract_crosswalk <- tract_crosswalk %>% 
  rename(geoid = TRACT) %>%
  mutate(geoid = as.factor(geoid))%>%
  select(-RES_RATIO, -BUS_RATIO, -OTH_RATIO, -TOT_RATIO)

#Left join the original year data to the census tract + zipcode crosswalk
tract_join_quarters <- total_addresses_quarters %>%
  left_join(tract_crosswalk) %>%
  rename(ZIPCODE = ZIP) %>%
  select(-ams_bus,-ams_oth)

#Remove census tracts without matching zipcode in provided census crosswalk
tract_join_quarters <- na.omit(tract_join_quarters) 

#Group by for zipcode and date to get the total of residences in a zipcode
tract_join_quarters <- tract_join_quarters %>%
  group_by(YYYYQQ, ZIPCODE) %>%
  mutate(ams_res = sum(ams_res),
         ams_total = sum(ams_total)) %>%
  select(-geoid) %>%
  relocate(ZIPCODE, .before = ams_res) %>%
  relocate(Year, .before = ams_res)%>%
  relocate(Quarter, .before = ams_res)

#Create a single number for the total residence and total addresses in a zipcode
addresses_in_zip <- tract_join_quarters %>%
  filter(Quarter == "01") %>%
  ungroup() %>%
  select(-YYYYQQ, -Quarter, -USPS_ZIP_PREF_CITY, -USPS_ZIP_PREF_STATE) %>% 
  distinct()

write.csv(addresses_in_zip, file.path(path, "check.csv"))

#Make variables short - total_res_2018, etc ; total_zip_2018, etc
res_denom <- addresses_in_zip %>% 
  select(ZIPCODE, Year, ams_res) %>% 
  pivot_wider(names_from = Year, values_from =ams_res, names_prefix = "total_res_")

total_denom <- addresses_in_zip %>% 
  select(ZIPCODE, Year, ams_total) %>% 
  pivot_wider(names_from = Year, values_from =ams_total, names_prefix = "total_zip_")

#Merge these two new data frames 
all_denom <- res_denom %>% 
  full_join(total_denom, by = "ZIPCODE")

#Merge with the original data set, adding a total of eight new variables per zipcode
final <- COA_quarterly %>% 
  full_join(all_denom, by = "ZIPCODE")

write.csv(final, file.path(path, "COA_quarters_opendatafile.csv"))

#Consider renaming the columns with a Q1 instead of 01 - make "Q" the joiner of YYYYQQ

#Consider generating a rate again

#Validate with census data

