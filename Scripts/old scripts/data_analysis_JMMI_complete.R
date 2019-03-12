rm(list=ls())

# install.packages(c("dplyr", "reshape2", "data.table", "stringr", "tidyr"))

library("dplyr")
library("tidyr")
library("reshape2")
library("data.table")
library("stringr")
library("reachR")

working.directory <- "C:/Users/REACH/Documents/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis" 
setwd(working.directory)

## Import csv ## Remember to update the input's file name
library(readr)
df <- read_csv("Inputs/reach_jmmi_markets_validated_jan.csv")

########################################################
## Calculating median prices per district/governorate ##
########################################################

## Delete districts with less than two observations/prepare data for analysis ##

data.selected <- df %>%
  select("governorate_ID", "governorate_name", "district_ID", "district_name", contains("normalised"), "cost_cubic_meter", "exchange_rate_result")

## Calculating median per district ##

disaggregation_variable <-"district_ID"

medians.by.districts <- data.selected %>% aggregate_median(disaggregation_variable)

write.csv(medians.by.districts, file = 'Outputs/median_district_result_jan.csv', row.names = FALSE)

### Before proceeding remember to move the newly created district dataframe to the inputs folder ###

## Calculating median per governorate ##
rm(list=ls())
library(readr)
df <- read_csv("Inputs/median_district_result_jan.csv")

data.selected <- df %>%
  select("governorate_ID", "governorate_name", "district_ID", "district_name", contains("normalised"), "cost_cubic_meter", "exchange_rate_result")

disaggregation_variable <-"governorate_ID"

medians.by.governorate <- data.selected %>% aggregate_median(disaggregation_variable)

write.csv(medians.by.governorate, file = 'Outputs/median_governorate_result_jan.csv')

  
## Calculating avg restocking times per governorate ##
######################################################

## Pivoting to select the columns of interest, and pivoting by grouping them by name and calculating the avg ##

df.restock.avg <- df %>%
  select(governorate_name, contains("restock")) %>%
  group_by(governorate_name) %>%
  summarise_all(funs(mean), na.rm = TRUE)

write.csv(df.restock.avg, file = 'Outputs/avg_restocking_times_result.csv')

######################################
## Proportion challenges per vendor ##
######################################

## Manipulation of data: selection of challenge columns ##

df.challenges <- df %>%
  unite(AllConstraints, c(fuel_constraints_multiple, wash_constraints_multiple, constraints_multiple), sep = " ", remove = TRUE) %>% select(AllConstraints)

## TRUE/FALSE when challenge is mentionned in row ##

df.challenges$challengepriceinflation <- str_detect(df.challenges$AllConstraints, "Price") == TRUE
df.challenges$challengeliquidityshortage <- str_detect(df.challenges$AllConstraints, "Liquidity") == TRUE
df.challenges$challengeshortagedemand <- str_detect(df.challenges$AllConstraints, "demand") == TRUE
df.challenges$challengeinsecurityinstability <- str_detect(df.challenges$AllConstraints, "Insecurity") == TRUE
df.challenges$challengesupplyshortage <- str_detect(df.challenges$AllConstraints, "Supply") == TRUE
df.challenges$challengegovernmentregulations <- str_detect(df.challenges$AllConstraints, "Government") == TRUE
df.challenges$challengetransportationissues <- str_detect(df.challenges$AllConstraints, "Transportation") == TRUE
df.challenges$challengeother <- str_detect(df.challenges$AllConstraints, "Other") == TRUE
df.challenges$challengedonotknow <- str_detect(df.challenges$AllConstraints, "Do_not_know") == TRUE
df.challenges$challengenoconstraints <- str_detect(df.challenges$AllConstraints, "No_constraints") == TRUE
df.challenges$challengevendordidnotanswer <- str_detect(df.challenges$AllConstraints, "Vendor") == TRUE

## Calculate the proportion of challenges ##

mean_challenges <- colMeans(df.challenges == "TRUE", na.rm = FALSE)

write.csv(mean_challenges, file = 'Outputs/challenges_result.csv')

###########################################
## Summary statistics for water trucking ##
###########################################
df.water.trucking.median <- df %>%
  select(capacity_truck,	location_source,	additional_cost_5,	additional_cost_10) %>%
  summarise_all(funs(median), na.rm = TRUE)

write.csv(df.water.trucking.median, file = 'Outputs/water_trucking_median_jan.csv')


#####################################
## Calculation of CASH proportions ##
#####################################
df.cash <- df %>%
  select(contains("cash_feasibility"))

mean.cash <- colMeans(df.cash == TRUE, na.rm = TRUE)

write.csv(mean.cash, file = 'Outputs/cash_result_jan.csv')

##################################
## Proportion of water trucking ##
##################################

## Proportion of water source mentioned ##

df.water.source <- df %>%
  select(type_water) %>%
  filter(!is.na(type_water)) %>%
  group_by(type_water) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.water.source, file = 'Outputs/water_source_jan.csv')

## Proportion of delivery costs ##

df.delivery.costs <- df %>%
  select(distance_price) %>%
  filter(!is.na(distance_price)) %>%
  group_by(distance_price) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.delivery.costs, file = 'Outputs/delivery_costs_jan.csv')

## Proportion of chlorinated water ##

df.chlorinated.water <- df %>%
  select(water_chlorinated) %>%
  filter(!is.na(water_chlorinated)) %>%
  group_by(water_chlorinated) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.chlorinated.water, file = 'Outputs/df_chlorinated_water_jan.csv')

## Proportion of type of owner ##
df.owner <- df %>% 
  select(type_owner) %>%
  filter(!is.na(type_owner)) %>%
  group_by(type_owner) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

write.csv(df.owner, file = 'Outputs/type_owner_jan.csv')


#################################
## Pivoting for the supply map ##
#################################

