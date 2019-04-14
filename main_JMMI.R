## Main Yemen Market Monitoring Script ##
# clear R environment
rm(list=ls())

# set wd to this script's locations
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)

# Load dependencies
source("./Scripts/basic scripts/load_dependencies.R")

# Load data preparation scripts
source("./Scripts/basic scripts/add_pcodes.R")
source('./Scripts/basic scripts/add_locations.R')
source("./Scripts/basic scripts/moveme.R")
source("./Scripts/basic scripts/delete_observations.R")

# Load calculators
source("./Scripts/basic scripts/calculate_smeb.R")
source("./Scripts/basic scripts/calculate_medians.R")


# Months -> update according to the month being analyzed
current_month <- "march_2019"

# Load cleaned dataset
data.frame <- read_csv("./Inputs/march2019.csv")
data.frame

# Add Pcodes or Locatin names to data.frame and move them at the beginning of the data.frame
#data.frame.named <- add.pcodes(data.frame.validated)
#data.frame.named <- data.frame.pcodes[moveme(names(data.frame.pcodes), "country_name after date_survey; country_ID after country_name; governorate_ID after governorate_name; district_ID after district_name")]

                      
## Delete the empty columns of price display ##
data.frame.validated <- dplyr::select(data.frame, -c("wash_list", "price_cubic_meter", "note_exchange_rate", contains("display"), contains("normalised")))

## Delete districts that have less than 3 observations (see. methodology)
data.frame.validated <- delete.districts(data.frame.validated, "district_ID", 3)

## Save final dataset ###
write.csv(data.frame.validated, file = paste0("./Outputs/final_validated_",current_month,".csv"), row.names = FALSE)
write.csv(data.frame.validated, file = paste0("./Inputs/final_validated_",current_month,".csv"), row.names = FALSE)

# Medians for dataset
data.medians.district <- calculate.medians(data.frame.validated, "district_ID")
write.csv(data.medians.district, file = paste0("Outputs/data_district_medians_",current_month,".csv"), row.names = FALSE)

data.medians.governorate <- calculate.medians(data.medians.district, "governorate_ID")
write.csv(data.medians.governorate , file = paste0("Outputs/data_governorate_medians_",current_month,".csv"), row.names = FALSE)

# SMEB for dataset
data.smeb.district <- calculate.smeb(data.medians.district, "district_ID")
write.csv(data.smeb.district, file = paste0("Outputs/data_district_SMEB_",current_month,".csv"), row.names = FALSE)

data.smeb.governorate <- calculate.smeb(data.medians.governorate, "governorate_ID")
write.csv(data.smeb.governorate, file = paste0("Outputs/data_governorate_SMEB_",current_month,".csv"), row.names = FALSE)


###################
## Data Analysis ##
###################

## Select only consecutive months
# Import CVS file ##
# Load previous and current month from cleaned datasets -> needs to be updated
previous.month <- read_csv("Outputs/final_validated_february_2019.csv")
current.month <- read_csv("Outputs/final_validated_march_2019.csv")

# Select unique ID from current month to match on previous month
uniqueID <- unique(previous.month$district_ID)

current.month.analysis <- current.month %>% subset(district_ID %in% uniqueID)

# Save matched file
write.csv(current.month.analysis, file = paste0("./Outputs/analysis_",current_month,".csv"), row.names = FALSE)
write.csv(current.month.analysis, file = paste0("./Inputs/analysis_",current_month,".csv"), row.names = FALSE)

# Medians calculations #
# Calculate district, governorate, national medians using the matched file
district_medians <- calculate.medians(current.month.analysis, "district_ID")
write.csv(district_medians, file = paste0("Outputs/district_medians_",current_month,".csv"), row.names = FALSE)

governorate_medians <- calculate.medians(district_medians, "governorate_ID")
write.csv(governorate_medians, file = paste("Outputs/governorate_medians_",current_month,".csv"), row.names = FALSE)

country_medians <- calculate.medians(governorate_medians, "country_ID")
write.csv(country_medians, file = paste("Outputs/country_medians_",current_month,".csv"), row.names = FALSE)

# SMEB calculations #
# Calculate district, governorate, national WASH SMEB using the matched file
district_smeb <- calculate.smeb(current.month.analysis, "district_ID")
write.csv(district_smeb, file = paste0("Outputs/district_smeb_",current_month,".csv"), row.names = FALSE)

governorate_smeb <- calculate.smeb(district_smeb, "governorate_ID")
write.csv(governorate_smeb, file = paste0("Outputs/governorate_smeb_",current_month,".csv"), row.names = FALSE)

country_smeb <- calculate.smeb(governorate_smeb, "country_ID")
write.csv(country_smeb, file = paste0("Outputs/country_smeb_",current_month,".csv"), row.names = FALSE)


# Calculate percentage change (I still need to create a function for this)
# Import CVS file - the file needs to be created manually by merging the governorate level medians##
ts <- read_csv("Inputs/timeseries/JMMI_timeseries_R_v2.csv")
#ts$date <- as.Date(ts$date, format="%d/%m/%Y")
ts$date <- lubridate::dmy(ts$date)

## Calculate month to month percentage change ##
pct <- function(x) {((x/lag(x))-1)*100}


# District level #
district_ts <- ts %>%
            group_by(district_ID) %>%
            arrange(date) %>%
            mutate_at(vars(calc_price_petrol, calc_price_diesel,	calc_price_bottled_water,	calc_price_treated_water,	calc_price_soap,	calc_price_laundry,	calc_price_sanitary,	cost_cubic_meter,	exchange_rate), pct)


write.csv(district_ts, file = paste0("Outputs/ts_district_",current_month,".csv"), row.names = FALSE)


# Governorate level #
governorate_ts <- ts %>% 
  group_by(governorate_ID) %>% 
  arrange(date) %>%
  mutate_at(vars(calc_price_petrol, calc_price_diesel, calc_price_bottled_water, calc_price_treated_water, calc_price_soap, calc_price_laundry, calc_price_sanitary, cost_cubic_meter, exchange_rate), pct)

write.csv(governorate_ts, file = paste0("Outputs/ts_governorate_",current_month,".csv"), row.names = FALSE)


## Calculate national percentage change ##
national_ts <- read_csv("Inputs/timeseries/JMMI_national_timeseries.csv")

national_ts <- national_ts %>% 
  mutate_at(vars(calc_price_petrol, calc_price_diesel, calc_price_bottled_water, calc_price_treated_water, calc_price_soap, calc_price_laundry, calc_price_sanitary, cost_cubic_meter, exchange_rate), pct)

write.csv(national_ts, file = paste0("Outputs/ts_country_",current_month,".csv"), row.names = FALSE)

################################################################################
## Other variables (not sure if it makes sense to create functions for these) ##
################################################################################

## Calculating avg restocking times per governorate 
## Pivoting to select the columns of interest, and pivoting by grouping them by name and calculating the avg ##
df.restock.avg <- current.month.analysis %>%
  dplyr::select(governorate_name, contains("restock")) %>%
  group_by(governorate_name) %>%
  summarise_all(funs(mean), na.rm = TRUE)

write.csv(df.restock.avg, file = paste0("Outputs/restock_times_",current_month,".csv"), row.names = FALSE)

## Proportion challenges per vendor 
## Manipulation of data: selection of challenge columns
df.challenges <- current.month.analysis %>%
  tidyr::unite(AllConstraints, c(fuel_constraints_multiple, wash_constraints_multiple, constraints_multiple), sep = " ", remove = TRUE) %>% dplyr::select(AllConstraints)

## TRUE/FALSE when challenge is mentionned in row
df.challenges$challengepriceinflation <- stringr::str_detect(df.challenges$AllConstraints, "Price") == TRUE
df.challenges$challengeliquidityshortage <- stringr::str_detect(df.challenges$AllConstraints, "Liquidity") == TRUE
df.challenges$challengeshortagedemand <- stringr::str_detect(df.challenges$AllConstraints, "demand") == TRUE
df.challenges$challengeinsecurityinstability <- stringr::str_detect(df.challenges$AllConstraints, "Insecurity") == TRUE
df.challenges$challengesupplyshortage <- stringr::str_detect(df.challenges$AllConstraints, "Supply") == TRUE
df.challenges$challengegovernmentregulations <- stringr::str_detect(df.challenges$AllConstraints, "Government") == TRUE
df.challenges$challengetransportationissues <- stringr::str_detect(df.challenges$AllConstraints, "Transportation") == TRUE
df.challenges$challengeother <- stringr::str_detect(df.challenges$AllConstraints, "Other") == TRUE
df.challenges$challengedonotknow <- stringr::str_detect(df.challenges$AllConstraints, "Do_not_know") == TRUE
df.challenges$challengenoconstraints <- stringr::str_detect(df.challenges$AllConstraints, "No_constraints") == TRUE
df.challenges$challengevendordidnotanswer <- stringr::str_detect(df.challenges$AllConstraints, "Vendor") == TRUE

## Calculate the proportion of challenges
mean_challenges <- colMeans(df.challenges == "TRUE", na.rm = FALSE)

write.csv(mean_challenges, file = paste0("Outputs/challenges_",current_month,".csv"), row.names = FALSE)


## Calculation of CASH proportions
df.cash <- current.month.analysis %>%
  dplyr::select(contains("cash_feasibility"))

mean.cash <- colMeans(df.cash == TRUE, na.rm = TRUE)

write.csv(mean.cash, file = paste0("Outputs/cash_",current_month,".csv"), row.names = FALSE)

## Water trucking analysis
## Summary statistics for water trucking (median truck capacity, median distance from location, median additional costs)
df.water.trucking.median <- current.month.analysis %>%
  dplyr::select(capacity_truck,	location_source,	additional_cost_5,	additional_cost_10) %>%
  summarise_all(funs(median), na.rm = TRUE)

write.csv(df.water.trucking.median, file = paste0("Outputs/water_trucking_",current_month,".csv"), row.names = FALSE)


## Proportion of water source mentioned ##
df.water.source <- current.month.analysis %>%
  dplyr::select(type_water) %>%
  filter(!is.na(type_water)) %>%
  group_by(type_water) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.water.source, file = paste0("Outputs/water_source_",current_month,".csv"), row.names = FALSE)

## Proportion of vendors applying an increase due to distance
df.delivery.costs <- current.month.analysis %>%
  dplyr::select(distance_price) %>%
  filter(!is.na(distance_price)) %>%
  group_by(distance_price) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.delivery.costs, file = paste0("Outputs/delivery_cost_",current_month,".csv"), row.names = FALSE)

## Proportion of chlorinated water
df.chlorinated.water <- current.month.analysis %>%
  dplyr::select(water_chlorinated) %>%
  filter(!is.na(water_chlorinated)) %>%
  group_by(water_chlorinated) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.chlorinated.water, file = paste0("Outputs/chlorination_",current_month,".csv"), row.names = FALSE)

## Proportion of type of owner ##
df.owner <- current.month.analysis %>% 
  dplyr::select(type_owner) %>%
  filter(!is.na(type_owner)) %>%
  group_by(type_owner) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

write.csv(df.owner, file = paste0("Outputs/type_owner_",current_month,".csv"), row.names = FALSE)


## Issues affecting supply chain ##
## Manipulation of data: selection of challenge columns
mrkt.supply.issues <- current.month.analysis %>%
  tidyr::unite(AllConstraints, mrk_supply_issues, sep = " ", remove = TRUE) %>% dplyr::select(AllConstraints)

## TRUE/FALSE when issue is mentionned in row
mrkt.supply.issues$issue.dmg.market <- stringr::str_detect(mrkt.supply.issues$AllConstraints, "dmg_infra_market") == TRUE
mrkt.supply.issues$issue.dmg.area <- stringr::str_detect(mrkt.supply.issues$AllConstraints, "dmg_infra_sorrounding") == TRUE
mrkt.supply.issues$issue.dmg.storage <- stringr::str_detect(mrkt.supply.issues$AllConstraints, "dmg_storage") == TRUE
mrkt.supply.issues$issue.movement <- stringr::str_detect(mrkt.supply.issues$AllConstraints, "move_restriction") == TRUE
mrkt.supply.issues$isssue.noanswer <- stringr::str_detect(mrkt.supply.issues$AllConstraints, "did_not_answer") == TRUE

## Calculate the proportion of challenges
mean.supply.issues <- colMeans(mrkt.supply.issues == "TRUE", na.rm = FALSE)

write.csv(mean.supply.issues, file = paste0("Outputs/market_supply_issues_",current_month,".csv"), row.names = FALSE)


## Infrastrcutural damage ##
infra.damage <- current.month.analysis %>%
  tidyr::unite(AllConstraints, mrk_dmg_infra, sep = " ", remove = TRUE) %>% dplyr::select(AllConstraints)

## TRUE/FALSE when issue is mentionned in row
infra.damage$dmg.road <- stringr::str_detect(infra.damage$AllConstraints, "road") == TRUE
infra.damage$dmg.electrical <- stringr::str_detect(infra.damage$AllConstraints, "electrical") == TRUE
infra.damage$dmg.water <- stringr::str_detect(infra.damage$AllConstraints, "water") == TRUE
infra.damage$dmg.communication <- stringr::str_detect(infra.damage$AllConstraints, "communication") == TRUE
infra.damage$dmg.other <- stringr::str_detect(infra.damage$AllConstraints, "other") == TRUE

write.csv(infra.damage, file = paste0("Outputs/infra_damage_",current_month,".csv"), row.names = FALSE)

## Supply increase: FUEL ##
supply.increase.fuel50 <- current.month.analysis %>%
              dplyr::select(mrk_increse_fuel_50) %>%
              filter(!is.na(mrk_increse_fuel_50)) %>%
              group_by(mrk_increse_fuel_50) %>%
              summarise(n=n()) %>%
              mutate(freq = n/sum(n))

supply.increase.fuel100 <- current.month.analysis %>%
              dplyr::select(mrk_increse_fuel_100) %>%
              filter(!is.na(mrk_increse_fuel_100)) %>%
              group_by(mrk_increse_fuel_100) %>%
              summarise(n=n()) %>%
              mutate(freq = n/sum(n))

supply.increase.fuel <- bind_rows(supply.increase.fuel50, supply.increase.fuel100)

write.csv(supply.increase.fuel, file = paste0("Outputs/supply_increase_fuel_",current_month,".csv"), row.names = FALSE)


## Supply increase: SMEB ##
supply.increase.WASH50 <- current.month.analysis %>%
  dplyr::select(mrk_increse_WASH_50) %>%
  filter(!is.na(mrk_increse_WASH_50)) %>%
  group_by(mrk_increse_WASH_50) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.WASH100 <- current.month.analysis %>%
  dplyr::select(mrk_increse_WASH_100) %>%
  filter(!is.na(mrk_increse_WASH_100)) %>%
  group_by(mrk_increse_WASH_100) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.wash <- bind_rows(supply.increase.WASH50, supply.increase.WASH100)

write.csv(supply.increase.wash, file = paste0("Outputs/supply_increase_WASH_",current_month,".csv"), row.names = FALSE)


## Supply increase: WATER ##
supply.increase.water50 <- current.month.analysis %>%
  dplyr::select(mrk_increse_water_50) %>%
  filter(!is.na(mrk_increse_water_50)) %>%
  group_by(mrk_increse_water_50) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.water100 <- current.month.analysis %>%
  dplyr::select(mrk_increse_water_100) %>%
  filter(!is.na(mrk_increse_water_100)) %>%
  group_by(mrk_increse_water_100) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

supply.increase.wash <- bind_rows(supply.increase.water50, supply.increase.water100)

write.csv(supply.increase.wash, file = paste0("Outputs/supply_increase_water_",current_month,".csv"), row.names = FALSE)
