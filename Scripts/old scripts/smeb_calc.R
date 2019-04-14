rm(list=ls())

# install.packages("reachR")
library("reachR")
library("data.table")
library("reachR")
library("dplyr")
library("reshape2")
library("miscTools")
library("readr")

setwd("~/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis/yemen_jmmi")


### DISTRICT LEVEL SMEB CALCULATIONS ###
## Import csv ##
df <- read_csv("Inputs/median_district_result_feb.csv")

## Portion dataset for calculation ##
data.smeb <- data.medians.district %>% dplyr::select("governorate_ID", "governorate_name","district_ID", "district_name", "calc_price_soap", "calc_price_laundry", "calc_price_sanitary", "cost_cubic_meter")

## Soap - Calculate weighted value of Soap and replace NA with governorate median ##
data.smeb$smeb_soap <- data.smeb$calc_price_soap*10.5
data.smeb <- data.smeb %>% group_by(governorate_ID) %>% mutate(smeb_soap= ifelse(is.na(smeb_soap), median(smeb_soap, na.rm=T), smeb_soap))

## Laundry Powder - Calculate weighted value of Laundry Powder and replace NA with governorate median ##
data.smeb$smeb_laundry <- data.smeb$calc_price_laundry*20
data.smeb <- data.smeb %>% group_by(governorate_ID) %>% mutate(smeb_laundry= ifelse(is.na(smeb_laundry), median(smeb_laundry, na.rm=T), smeb_laundry))

## Sanitary Napkins - Calcualte the weighted value of Sanitary Napkins and replace NA with governorate median ##
data.smeb$smeb_napkins <- data.smeb$calc_price_sanitary*2
data.smeb <- data.smeb %>% group_by(governorate_ID) %>% mutate(smeb_napkins= ifelse(is.na(smeb_napkins), median(smeb_napkins, na.rm=T), smeb_napkins))

## Water trucking - Calcualte the weighted value of Water Trucking and replace NA with governorate median ##
data.smeb$smeb_cubic <- data.smeb$cost_cubic_meter*3.15
data.smeb <- data.smeb %>% group_by(governorate_ID) %>% mutate(smeb_cubic= ifelse(is.na(smeb_cubic), median(cost_cubic_meter*3.15, na.rm=T), smeb_cubic))


## Total District level WASH SMEB Cost ##
data.smeb$smeb_total <- rowSums(data.smeb[,c(9,10,11,12)], na.rm=FALSE)
write.csv(data.smeb, file = 'Outputs/median_district_SMEB_result_march.csv', row.names = FALSE)


## Calculate national median ##
data.smeb$index <- 1

data.smeb.natmed <- data.smeb %>% 
                    group_by(index) %>%
                    summarise_at(funs(median), .vars=vars(smeb_soap:smeb_cubic), na.rm=TRUE)

data.smeb.natmed$smeb_total <- rowSums(data.smeb.natmed[,c(2,3,4,5)], na.rm=FALSE)

write.csv(data.smeb.natmed, file = 'Outputs/median_national_SMEB_feb.csv', row.names = FALSE)



### GOVERNORATE LEVEL SMEB CALCULATIONS ###
## Import csv and deleting unnnecessary columns##
df1 <- read_csv("Inputs/median_governorate_result_feb.csv")
df1[, c("X1_1", "district_ID", "district_name")] <- NULL

## Portion data for calculation
data.smeb1 <- df1 %>% select("governorate_ID", "governorate_name", "price_soap_normalised", "price_laundry_powder_normalised", "price_sanitary_napkins_normalised", "cost_cubic_meter")

## Soap calculations ##
data.smeb1$smeb_soap <- data.smeb1$price_soap_normalised*10.5

## Laundry Powder Calculations ##
data.smeb1$smeb_laundry <- data.smeb1$price_laundry_powder_normalised*20

## Sanitary Napkins Calculations ##
data.smeb1$smeb_napkins <- data.smeb1$price_sanitary_napkins_normalised*2

## Water trucking Calculations ##
data.smeb1$smeb_cubic <- data.smeb1$cost_cubic_meter*3.15

## Total Governorate level WASH SMEB Cost ##
data.smeb1$smeb_total <- rowSums(data.smeb1[,c(7,8,9,10)], na.rm=FALSE)
write.csv(data.smeb1, file = 'Outputs/median_governorate_SMEB_result_feb.csv', row.names = FALSE)



