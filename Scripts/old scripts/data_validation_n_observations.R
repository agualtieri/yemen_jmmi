## Scripts to aggregate price values, and prepare final dataset for validation

rm(list=ls())

# installed.packages("reshape2")
library("tidyverse")
library("readr")

# set wd to this script's folder
setwd("~/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis/yemen_jmmi")


# Import using other code - change name of the file
df <- read_csv("Inputs/february2019.csv")

#####################################
## Preparing data for the analysis ##
#####################################

## Delete districts with only one or two observation ##
data <- df %>%
  group_by(district_ID) %>%
  filter(n() >= 3)

## Delete the empty columns of price display ##
data <- select(df.validated, -c("wash_list", "price_cubic_meter", contains("display")))


## Merge columns of non-calculated prices and calculated prices ##

## Idea: if-else structure - if price for standard wight copy if else copy price from non-standard weight.
## NaN should be there already now that I required all the questions and people cannot skip anymore.
## Test next week or when you have some time this week (RCM week)


## Petrol ##
data$price_petrol_normalised <- ifelse(data$calc_price_petrol == "NaN", data$price_petrol, data$calc_price_petrol)

## Diesel ##
data$price_diesel_normalised <- ifelse(data$calc_price_diesel == "NaN", data$price_diesel, data$calc_price_diesel)

## Bottled water ##
data$price_bottled_water_normalised <- ifelse(data$calc_price_bottled_water == "NaN", data$price_bottled_water, data$calc_price_bottled_water)

## Treated water ##
data$price_treated_water_normalised <- ifelse(data$calc_price_treated_water == "NaN", data$price_treated_water, data$calc_price_treated_water)

## Soap ##
data$price_soap_normalised <- ifelse(data$calc_price_soap == "NaN", data$price_soap, data$calc_price_soap)

## Sanitary Napkins ##
data$price_sanitary_napkins_normalised <- ifelse(data$calc_price_sanitary == "NaN", data$price_sanitary_napkins, data$calc_price_sanitary)

## Laundry powder ##
data$price_laundry_powder_normalised <- ifelse(data$calc_price_laundry == "NaN", data$price_laundry_powder, data$calc_price_laundry)

## Print csv ##
write.csv(data, file = 'Outputs/reach_jmmi_markets_validated_feb.csv', row.names = FALSE)

  