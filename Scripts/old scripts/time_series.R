## JMMI Timeseries Analysis ###
rm(list=ls())

# install.packages("reachR")
library("reachR")
library("data.table")
library("reachR")
library("dplyr")
library("reshape2")
library("tidyverse")
library("readr")

## Set up workin0 directory ##
setwd("~/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis/yemen_jmmi")

# Import CVS file ##
ts <- read_csv("Inputs/JMMI_timeseries_R.csv")
ts$date <- as.Date(ts$date, "%m/%d/%Y")

## Calculate month to month percentage change ##
pct <- function(x) {((x/lag(x))-1)*100}

pchg_ts <- ts %>% 
  group_by(district_ID) %>% 
  arrange(date, .by_group = T) %>%
  mutate_at(funs(pct), .vars=vars(price_petrol_normalised, price_diesel_normalised, price_bottled_water_normalised, price_treated_water_normalised, price_soap_normalised, price_laundry_powder_normalised, price_sanitary_napkins_normalised, cost_cubic_meter, exchange_rate))

write.csv(pchg_ts, file = 'Outputs/time_series_feb.csv', row.names = FALSE)


## Calculate national median ##
tst_med <- ts %>% 
  group_by(date) %>% 
  summarise_at(funs(median), .vars=vars(price_petrol_normalised, price_diesel_normalised, price_bottled_water_normalised, price_treated_water_normalised, price_soap_normalised, price_laundry_powder_normalised, price_sanitary_napkins_normalised, cost_cubic_meter, exchange_rate), na.rm=TRUE)


write.csv(tst_med, file = 'Outputs/nat_median_feb.csv', row.names = FALSE)

## Calculate national percentage change ##
ts_med_pchg <- tst_med %>% 
  mutate_at(vars(price_petrol_normalised:exchange_rate), pct)

write.csv(ts_med_pchg, file = 'Outputs/nat_change_feb.csv', row.names = FALSE)

