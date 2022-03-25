# USPS Change of Address 2018-2021 Research Design
# Created by: Sonia Torres Rodriguez
# Create date: December 17, 2021

#--------------------------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)

#Set working directory
setwd("~/GitHub/COA/prog")

# Choose where to save outputs. Default is output folder in the github repo folder. 
path <- c("../output/")

# Read in city list and coordinates
year_2018 <- read.csv("../y2018.csv", check.names = FALSE) 
year_2019 <- read.csv("../y2019.csv", check.names = FALSE)
year_2020 <- read.csv("../y2020.csv", check.names = FALSE)
year_2021 <- read.csv("../y2021.csv", check.names = FALSE)

#Create net variables
year_2018$NET_FAMILY <- year_2018$TOTAL_FAMILY_1 - year_2018$TOTAL_FAMILY 
year_2018$NET_INDIVIDUAL <- year_2018$TOTAL_INDIVIDUAL_1 - year_2018$TOTAL_INDIVIDUAL
year_2018$NET_PERM <- year_2018$TOTAL_PERM_1 - year_2018$TOTAL_PERM 

year_2019$NET_FAMILY <- year_2019$TOTAL_FAMILY_1 - year_2019$TOTAL_FAMILY 
year_2019$NET_INDIVIDUAL <- year_2019$TOTAL_INDIVIDUAL_1 - year_2019$TOTAL_INDIVIDUAL
year_2019$NET_PERM <- year_2019$TOTAL_PERM_1 - year_2019$TOTAL_PERM 

year_2020$NET_FAMILY <- year_2020$TOTAL_FAMILY_1 - year_2020$TOTAL_FAMILY 
year_2020$NET_INDIVIDUAL <- year_2020$TOTAL_INDIVIDUAL_1 - year_2020$TOTAL_INDIVIDUAL
year_2020$NET_PERM <- year_2020$TOTAL_PERM_1 - year_2020$TOTAL_PERM 

year_2021$NET_FAMILY <- year_2021$TOTAL_FAMILY_1 - year_2021$TOTAL_FAMILY 
year_2021$NET_INDIVIDUAL <- year_2021$TOTAL_INDIVIDUAL_1 - year_2021$TOTAL_INDIVIDUAL
year_2021$NET_PERM <- year_2021$TOTAL_PERM_1 - year_2021$TOTAL_PERM 

#Create to residential to and from counts
year_2018$TOTAL_RESIDENTIAL <- year_2018$TOTAL_FAMILY + year_2018$TOTAL_INDIVIDUAL
year_2018$TOTAL_RESIDENTIAL_1 <- year_2018$TOTAL_FAMILY_1 + year_2018$TOTAL_INDIVIDUAL_1

year_2019$TOTAL_RESIDENTIAL <- year_2019$TOTAL_FAMILY + year_2019$TOTAL_INDIVIDUAL
year_2019$TOTAL_RESIDENTIAL_1 <- year_2019$TOTAL_FAMILY_1 + year_2019$TOTAL_INDIVIDUAL_1

year_2020$TOTAL_RESIDENTIAL <- year_2020$TOTAL_FAMILY + year_2020$TOTAL_INDIVIDUAL
year_2020$TOTAL_RESIDENTIAL_1 <- year_2020$TOTAL_FAMILY_1 + year_2020$TOTAL_INDIVIDUAL_1

year_2021$TOTAL_RESIDENTIAL <- year_2021$TOTAL_FAMILY + year_2021$TOTAL_INDIVIDUAL
year_2021$TOTAL_RESIDENTIAL_1 <- year_2021$TOTAL_FAMILY_1 + year_2021$TOTAL_INDIVIDUAL_1

#Create net residential variables
year_2018$NET_RESIDENTIAL <- year_2018$NET_FAMILY + year_2018$NET_INDIVIDUAL
year_2019$NET_RESIDENTIAL <- year_2019$NET_FAMILY + year_2019$NET_INDIVIDUAL
year_2020$NET_RESIDENTIAL <- year_2020$NET_FAMILY + year_2020$NET_INDIVIDUAL
year_2021$NET_RESIDENTIAL <- year_2021$NET_FAMILY + year_2021$NET_INDIVIDUAL

#Only keep variables of interest (net_zip, net_perm, net_temp, net_residential)
#Need to rename the to and from variables 
year_2018 <- year_2018 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE, TOTAL_PERM, TOTAL_PERM_1, NET_PERM, TOTAL_RESIDENTIAL, TOTAL_RESIDENTIAL_1, NET_RESIDENTIAL) %>% 
  rename(FROM_PERM = TOTAL_PERM) %>% 
  rename(TO_PERM = TOTAL_PERM_1) %>% 
  rename(FROM_RESIDENTIAL = TOTAL_RESIDENTIAL) %>% 
  rename(TO_RESIDENTIAL = TOTAL_RESIDENTIAL_1)

year_2019 <- year_2019 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE, TOTAL_PERM, TOTAL_PERM_1, NET_PERM, TOTAL_RESIDENTIAL, TOTAL_RESIDENTIAL_1, NET_RESIDENTIAL) %>% 
  rename(FROM_PERM = TOTAL_PERM) %>% 
  rename(TO_PERM = TOTAL_PERM_1) %>% 
  rename(FROM_RESIDENTIAL = TOTAL_RESIDENTIAL) %>% 
  rename(TO_RESIDENTIAL = TOTAL_RESIDENTIAL_1)

year_2020 <- year_2020 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE, TOTAL_PERM, TOTAL_PERM_1, NET_PERM, TOTAL_RESIDENTIAL, TOTAL_RESIDENTIAL_1, NET_RESIDENTIAL) %>% 
  rename(FROM_PERM = TOTAL_PERM) %>% 
  rename(TO_PERM = TOTAL_PERM_1) %>% 
  rename(FROM_RESIDENTIAL = TOTAL_RESIDENTIAL) %>% 
  rename(TO_RESIDENTIAL = TOTAL_RESIDENTIAL_1)

year_2021 <- year_2021 %>%
  select(YYYYMM, ZIPCODE, CITY, STATE, TOTAL_PERM, TOTAL_PERM_1, NET_PERM, TOTAL_RESIDENTIAL, TOTAL_RESIDENTIAL_1, NET_RESIDENTIAL) %>% 
  rename(FROM_PERM = TOTAL_PERM) %>% 
  rename(TO_PERM = TOTAL_PERM_1) %>% 
  rename(FROM_RESIDENTIAL = TOTAL_RESIDENTIAL) %>% 
  rename(TO_RESIDENTIAL = TOTAL_RESIDENTIAL_1)

#Join data frames of all years
list <- list(year_2018,year_2019,year_2020,year_2021)
do.call("rbind", list)
COA_monthly_zip <- do.call("rbind", lapply(list, as.data.frame)) 

##### Need to raise this data up to a county (not zip) and yearly (not monthly) level

#Read in crosswalk file, from here: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
#County FIPS for DC is 11001

crosswalk <- read_csv("../ZIP_COUNTY_032018.csv") 

#Rename zip code to match the original data 
crosswalk <- crosswalk %>% 
  rename(ZIPCODE = zip) %>%
  mutate(ZIPCODE = as.numeric(ZIPCODE),
         ZIPCODE = str_pad(ZIPCODE, 5, side = c("left"), pad = "0"))

#Convert the monthly zip flows from USPS COA to yearly county flows - only calling year 2018 because that will be first comparison to the Census data
zip_join2018 <- year_2018 %>%
  mutate(ZIPCODE = as.numeric(ZIPCODE),
         ZIPCODE = str_pad(ZIPCODE, 5, side = c("left"), pad = "0")) %>%
  left_join(crosswalk, by="ZIPCODE") 

county_COA_2018 <- zip_join2018 %>% 
  group_by(county) %>% 
  mutate(NET_PERM = sum(NET_PERM),
         NET_RESIDENTIAL = sum(NET_RESIDENTIAL)) %>% 
  select(county,STATE,NET_PERM, NET_RESIDENTIAL) %>% 
  distinct(county, .keep_all= TRUE) 

#Extract state from county - filter to only Greater DC region
county_COA_2018$state = substr(county_COA_2018$county,1,2)
county_COA_2018 <- county_COA_2018 %>% 
  filter(state == 11 | state == 24 |state == 51) 

####Bring in Census Data
#Validating USPS COA counts with census data

library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_use_cache = TRUE)

# Use Census API key
census_api_key("e0e50dbbc586496e193c885ba478cae24f3150b3", overwrite = FALSE, install = FALSE)

#Reference: https://walker-data.com/tidycensus/articles/other-datasets.html#migration-flows-1 

#Call migration flows (lowest level is county) - only available up to year 2019
flows_2018 <- get_flows(
  geography = "county",
  year = 2018
)

flows_2018 <- flows_2018 %>% 
  filter(variable == "MOVEDNET") %>% 
  arrange(estimate) %>% 
  rename(county = GEOID1)

#Extract state from county
flows_2018$state = substr(flows_2018$county,1,2)

#Need to calculate the aggregate MOE - square each value in the column - see reference: https://www.census.gov/content/dam/Census/library/publications/2020/acs/acs_general_handbook_2020_ch08.pdf
flows_2018$moe_sq ='^'(flows_2018$moe,2)

greaterdc_flows_2018 <- flows_2018 %>% 
  select(-GEOID2) %>%
  filter(state == 11 | state == 24 |state == 51) %>%
  drop_na() %>% 
  group_by(county) %>% 
  mutate(estimate = sum(estimate),
         moe_sq = sum(moe_sq)) %>% 
  mutate(moesr=sqrt(moe_sq)) %>% 
  distinct(county, .keep_all= TRUE) %>% 
  select(-FULL2_NAME, -moe, -moe_sq) %>% 
  rename(flow = estimate)

#Call county income level, create three tiers
my_states <- c("DC", "MD", "VA")

med_inc <- get_acs(
  geography = "county",
  state = my_states,
  variables = "B19013_001",
  year = 2018
)

#Create three arbitrary flags- Less than $50K is low income, $50-$100K is mid, above $100k is high
med_inc$indicator <- as.factor(ifelse(med_inc$estimate < 50000, 1, 2))
med_inc$indicator <- as.factor(ifelse(med_inc$estimate > 75000, 3, med_inc$indicator)) #Last value keeps previous column

med_inc <- med_inc %>% 
  rename(county= GEOID) %>% 
  rename(median_income = estimate) %>% 
  select(-moe)

#Join income income data to Census Mobility Data
census <- left_join(greaterdc_flows_2018, med_inc, by='county')

#Join the merged Census Data to the USPS COA data
merged <- left_join(county_COA_2018, census, by= 'county')

merged <- merged %>% 
  select(-STATE, -state.x, -FULL1_NAME, -variable.x, -state.y, -variable.y)

#Run correlate for 3 different income levels
merged_low_inc <- merged %>% 
  filter(indicator == "1")

merged_mid_inc <- merged %>% 
  filter(indicator == "2")

merged_high_inc <- merged %>% 
  filter(indicator == "3")

#Calculate correlate, reference: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r
library("ggpubr")
ggscatter(merged_low_inc, x = "NET_PERM", y = "flow", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Greater DC Low Income Counties",
          xlab = "USPS COA 2018 net", ylab = "Census Mobility 2018 net flow")

ggscatter(merged_mid_inc, x = "NET_PERM", y = "flow", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Greater DC Middle Income Counties",
          xlab = "USPS COA 2018 net", ylab = "Census Mobility 2018 net flow")
  
ggscatter(merged_high_inc, x = "NET_PERM", y = "flow", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Greater DC High Income Counties",
          xlab = "USPS COA 2018 net", ylab = "Census Mobility 2018 net flow")

ggscatter(merged, x = "NET_PERM", y = "flow", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          title = "Greater DC All Counties",
          xlab = "USPS COA 2018 net", ylab = "Census Mobility 2018 net flow")

#If want to come back and calculate poverty rates and create flags
# poverty <- get_acs(
#   geography = "county",
#   state = my_states,
#   variables = "B17010_002",
#   year = 2018
# )
# 
# pop<- get_acs(
#   geography = "county",
#   state = my_states,
#   variables = "B17010_001",
#   year = 2018
# )
# 
# poverty_stat <- left_join(poverty, pop, by='GEOID')
# 
# poverty_stat <- poverty_stat %>% 
#   select(-variable.x, -NAME.y, -variable.y) %>% 
#   rename(county = GEOID) %>% 
#   rename(NAME = NAME.x) %>% 
#   rename(poverty = estimate.x) %>% 
#   rename(poverty_moe = moe.x) %>% 
#   rename(pop = estimate.y) %>% 
#   rename(pop_moe = moe.y)
# 
# poverty_stat$pov_rate <- poverty_stat$poverty / poverty_stat$pop

