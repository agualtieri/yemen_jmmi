rm(list=ls())

# installed.packages("reshape2")
library("dplyr")
library("reshape2")
library("rstudioapi")

# set wd to this script's folder
working.directory <- "C:/Users/REACH/Documents/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis" 
setwd(working.directory)

## Import ## Make sure the dataset is in the "Inputs" folder and change the code to load the correct file
#data.file <- "./Inputs/REACH_JMMI_October_Final_Translated.csv"
#data.file<-load(file="REACH_JMMI_October_Final_Translated.csv.csv")
#df.data <- read.csv(data.file,header=T,sep=",", encoding = "UTF-8", check.names=F)
#df <- as.data.frame(df.data)

# Import using other code - change name of the file
library(readr)
df <- read_csv("Inputs/january2019.csv")

#####################################
## Preparing data for the analysis ##
#####################################

## Delete districts with only one or two observation ##

df.validated <- df %>%
  group_by(district_ID) %>%
  filter(n() >= 3)

## Delete the empty columns of price display ##

df.validated2 <- select(df.validated, -contains("display"))

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
write.csv(df.validated2, file = 'Outputs/reach_jmmi_markets_validated_jan.csv', row.names = FALSE)


?write.csv
  