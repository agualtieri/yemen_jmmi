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
source("./Scripts/basic scripts/moveme.R")
source("./Scripts/basic scripts/delete_observations.R")

# Load calculators
source("./Scripts/basic scripts/calculate_smeb.R")
source("./Scripts/basic scripts/calculate_medians.R")


# Months -> update according to the month being analyzed
current_month <- "february_2019"


# Load cleaned dataset
data.frame <- read_csv("./Inputs/february2019_test.csv")
View(data.frame)

# Add Pcodes to data.frame and move them at the beginning of the data.frame
data.frame.pcodes <- add.pcodes(data.frame)
data.frame.pcodes <- data.frame.pcodes[moveme(names(data.frame.pcodes), "country_name after date_survey; country_ID after country_name; governorate_ID after governorate_name; district_ID after district_name")]

                      
## Delete the empty columns of price display ##
#data.frame.validated <- dplyr::select(data.frame.pcodes, -c("wash_list", "price_cubic_meter", contains("display")))

## Delete districts that have less than 3 observations (see. methodology)
data.frame.validated <- delete.districts(data.frame.pcodes, "district_ID", 3)

## Save final dataset ###
write.csv(data.frame.validated, file = paste0("./Outputs/final_validated_",current_month,".csv"), row.names = FALSE)
write.csv(data.frame.validated, file = paste0("./Inputs/final_validated_",current_month,".csv"), row.names = FALSE)

## Data Analysis

## Select only consecutive months
# Import CVS file ##
# Load previous and current month from cleaned datasets
previous.month <- read_csv("Inputs/final_validated_january_2019.csv")
current.month <- read_csv("Inputs/final_validated_february_2019.csv")

# Select unique ID from current month to match on previous month
uniqueID <- unique(previous.month$district_ID)

current.month.analysis <- current.month %>% subset(district_ID %in% uniqueID)

# Save matched file
write.csv(current.month.analysis, file = paste0("./Outputs/analysis_",current_month,".csv"), row.names = FALSE)
write.csv(current.month.analysis, file = paste0("./Inputs/analysis_",current_month,".csv"), row.names = FALSE)

# Medians calculations #
# Calculate district, governorate, national medians using the matched file
district_medians <- medians.calc(current.month.analysis, "district_ID")
write.csv(district_medians, file = paste0("Outputs/district_medians_",current_month,".csv"), row.names = FALSE)

governorate_medians <- medians.calc(district_medians, "governorate_ID")
write.csv(governorate_medians, file = paste("Outputs/governorate_medians_",current_month,".csv"), row.names = FALSE)

country_medians <- medians.calc(governorate_medians, "country_ID")
write.csv(national_medians, file = paste("Outputs/country_median_",current_month,".csv"), row.names = FALSE)

# SMEB calculations #
# Calculate district, governorate, national WASH SMEB using the matched file
district_smeb <- calculate.smeb(current.month.analysis, "district_ID")
write.csv(district_smeb, file = paste0("Outputs/district_smeb_",current_month,".csv"), row.names = FALSE)

governorate_smeb <- calculate.smeb(current.month.analysis, "governorate_ID")
write.csv(governorate_smeb, file = paste0("Outputs/governorate_smeb_",current_month,".csv"), row.names = FALSE)

country_smeb <- calculate.smeb(current.month.analysis, "country_ID")
write.csv(governorate_smeb, file = paste0("Outputs/country_smeb_",current_month,".csv"), row.names = FALSE)

# Calculate percentage change (I still need to create a function for this)

# Import CVS file - the file needs to be created manually by merging the governorate level medians##
ts <- read_csv("Inputs/JMMI_timeseries_R.csv")
ts$date <- as.Date(ts$date, "%m/%d/%Y")

## Calculate month to month percentage change ##
pct <- function(x) {((x/lag(x))-1)*100}

# District level #
pchg_ts <- ts %>% 
  group_by(district_ID) %>% 
  arrange(date, .by_group = T) %>%
  mutate_at(funs(pct), .vars=vars(price_petrol_normalised, price_diesel_normalised, price_bottled_water_normalised, price_treated_water_normalised, price_soap_normalised, price_laundry_powder_normalised, price_sanitary_napkins_normalised, cost_cubic_meter, exchange_rate))

write.csv(pchg_ts, file = paste0("Outputs/ts_district_",current_month,".csv"), row.names = FALSE)

# Governorate level #
pchg_ts <- ts %>% 
  group_by(governorate_ID) %>% 
  arrange(date, .by_group = T) %>%
  mutate_at(funs(pct), .vars=vars(price_petrol_normalised, price_diesel_normalised, price_bottled_water_normalised, price_treated_water_normalised, price_soap_normalised, price_laundry_powder_normalised, price_sanitary_napkins_normalised, cost_cubic_meter, exchange_rate))

write.csv(pchg_ts, file = paste0("Outputs/ts_governorate_",current_month,".csv"), row.names = FALSE)


## Calculate national percentage change ##
ts_med_pchg <- tst_med %>% 
  mutate_at(vars(price_petrol_normalised:exchange_rate), pct)

write.csv(ts_med_pchg, file = paste0("Outputs/ts_country_",current_month,".csv"), row.names = FALSE)

################################################################################
## Other variables (not sure if it makes sense to create functions for these) ##
################################################################################

## Calculating avg restocking times per governorate 
## Pivoting to select the columns of interest, and pivoting by grouping them by name and calculating the avg ##
df.restock.avg <- current.month.analysis %>%
  select(governorate_name, contains("restock")) %>%
  group_by(governorate_name) %>%
  summarise_all(funs(mean), na.rm = TRUE)

write.csv(df.restock.avg, file = paste0("Outputs/restock_times_",current_month,".csv"), row.names = FALSE)


## Proportion challenges per vendor 
## Manipulation of data: selection of challenge columns
df.challenges <- current.month.analysis %>%
  unite(AllConstraints, c(fuel_constraints_multiple, wash_constraints_multiple, constraints_multiple), sep = " ", remove = TRUE) %>% select(AllConstraints)

## TRUE/FALSE when challenge is mentionned in row
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

## Calculate the proportion of challenges
mean_challenges <- colMeans(df.challenges == "TRUE", na.rm = FALSE)

write.csv(mean_challenges, file = paste0("Outputs/challenges_",current_month,".csv"), row.names = FALSE)


## Calculation of CASH proportions
df.cash <- current.month.analysis %>%
  select(contains("cash_feasibility"))

mean.cash <- colMeans(df.cash == TRUE, na.rm = TRUE)

write.csv(mean.cash, file = paste0("Outputs/cash_",current_month,".csv"), row.names = FALSE)

## Water trucking analysis
## Summary statistics for water trucking (median truck capacity, median distance from location, median additional costs)
df.water.trucking.median <- current.month.analysis %>%
  select(capacity_truck,	location_source,	additional_cost_5,	additional_cost_10) %>%
  summarise_all(funs(median), na.rm = TRUE)

write.csv(df.water.trucking.median, file = paste0("Outputs/water_trucking_",current_month,".csv"), row.names = FALSE)


## Proportion of water source mentioned ##
df.water.source <- current.month.analysis %>%
  select(type_water) %>%
  filter(!is.na(type_water)) %>%
  group_by(type_water) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.water.source, file = paste0("Outputs/water_source_",current_month,".csv"), row.names = FALSE)

## Proportion of vendors applying an increase due to distance
df.delivery.costs <- current.month.analysis %>%
  select(distance_price) %>%
  filter(!is.na(distance_price)) %>%
  group_by(distance_price) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.delivery.costs, file = paste0("Outputs/delivery_cost_",current_month,".csv"), row.names = FALSE)

## Proportion of chlorinated water
df.chlorinated.water <- current.month.analysis %>%
  select(water_chlorinated) %>%
  filter(!is.na(water_chlorinated)) %>%
  group_by(water_chlorinated) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.chlorinated.water, file = paste0("Outputs/chlorination_",current_month,".csv"), row.names = FALSE)

## Proportion of type of owner ##
df.owner <- df %>% 
  select(type_owner) %>%
  filter(!is.na(type_owner)) %>%
  group_by(type_owner) %>%
  summarise(n=n()) %>%
  mutate(freq = n/sum(n))

write.csv(df.owner, file = paste0("Outputs/type_owner_",current_month,".csv"), row.names = FALSE)