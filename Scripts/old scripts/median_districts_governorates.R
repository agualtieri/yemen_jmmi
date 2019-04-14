rm(list=ls())

# install.packages("reachR")

library("reachR")
library("data.table")
library("reachR")
library("dplyr")
library("reshape2")

working.directory <- "C:/Users/REACH/Documents/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis" 
setwd(working.directory)

## Import csv ##
library(readr)
df <- read_csv("Inputs/reach_jmmi_jan_markets_validated.csv")


## Delete districts with less than two observations/prepare data for analysis ##

data.selected <- data.frame.validated %>% dplyr::select("governorate_ID", "governorate_name", "district_ID", "district_name", contains("calc_price"), "cost_cubic_meter", "exchange_rate_result")

## Calculating medians per district ##

disaggregation_variable <-"district_ID"

medians.by.districts <- data.selected %>% aggregate_median(disaggregation_variable)

write.csv(medians.by.districts, file = 'Outputs/median_district_result_march.csv', col.names = FALSE)

## calculating median per governorate ##
rm(list=ls())

library("reachR")
library("data.table")
library("reachR")
library("dplyr")
library("reshape2")

working.directory <- "C:/Users/REACH/Documents/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis" 
setwd(working.directory)

## First move the median by district file into the inputs folder ##
##Import csv ##
library(readr)
df <- read_csv("Inputs/median_district_result_marchmarchmarchmarch.csv")


disaggregation_variable <-"governorate_ID"

medians.by.governorate <- df %>% aggregate_median(disaggregation_variable)

write.csv(medians.by.governorate, file = 'Outputs/median_governorate_result_march.csv', row.names = FALSE)

