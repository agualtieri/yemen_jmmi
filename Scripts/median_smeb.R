### Calculate Median by column ###
rm(list=ls())

# install.packages("reachR")
library("reachR")
library("data.table")
library("reachR")
library("dplyr")
library("reshape2")
library("tidyverse")


## Set up working directory ##
working.directory <- "C:/Users/REACH/Documents/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis" 
setwd(working.directory)

ts <- read_csv("Inputs/JMMI_timeseries_R.csv")
ts$date <- as.Date(ts$date, "%m/%d/%Y")

### Calculate median by column ###

df_melt <- melt(ts, id=c("district_ID", "date"))
df_melt <- dcast(df_melt, date+district_ID ~ variable, median)


ts_med <- aggregate(cbind(price_petrol_normalised, price_diesel_normalised, price_bottled_water_normalised, price_treated_water_normalised, price_soap_normalised, price_laundry_powder_normalised, price_sanitary_napkins_normalised, cost_cubic_meter, exchange_rate) ~ date+district_ID,
data = ts, median, na.rm=TRUE)
