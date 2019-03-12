rm(list=ls())

library("tidyr")
library("dplyr")
library("stringr")

working.directory <- "C:/Users/Audrey/Desktop/Joint Market Monitoring Initiative/3. data analysis/data" 
setwd(working.directory)

## Import ##

data.file <- "reach_jmmi_sept_validated.csv"
df.data <- read.csv(data.file,header=T,sep=",", encoding = "UTF-8", check.names=F)
df <- as.data.frame(df.data)

## Calculating median for location, capacity and additional cost ##

df.water.trucking.median <- df %>%
  select(capacity_truck,	location_source,	additional_cost_10,	additional_cost_20,	additional_cost_30) %>%
  summarise_all(funs(median), na.rm = TRUE)

write.csv(df.water.trucking.median, file = 'water.trucking.median.sept.csv')