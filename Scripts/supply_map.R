rm(list=ls())

library("dplyr")
library("plyr")
library("ggplot2")
library("reshape2")

working.directory <- "C:/Users/Audrey/Desktop" 
setwd(working.directory)

#Import
data.file <- "sept_supply_map_test.csv"
df.data <- read.csv(data.file,header=T,sep=",", encoding = "UTF-8", check.names=F)
df <- as.data.frame(df.data)

#Select df

df.pivot <- df %>%
  select(district_ID, petrol_gov_origin, diesel_gov_origin) %>%
  group_by(district_ID)

df.pivot2 <- as.data.frame.matrix(with(df.pivot, table(district_ID, petrol_gov_origin)))
rename(df.pivot2, district_ID = `[EMPTY]`)

df.pivot3 <- as.data.frame.matrix(with(df.pivot, table(district_ID, diesel_gov_origin)))
rename(df.pivot3, district_ID = `[EMPTY]`)

#merge(df.pivot2, df.pivot3, by="district_ID")
df.pivot4 <- anti_join(df.pivot2, df.pivot3, by = "district_ID")

write.csv(df.pivot2, file = 'supplymappivottablepetrol.csv')
write.csv(df.pivot3, file = 'supplymappivottablediesel.csv')
write.csv(df.pivot4, file = 'testfinalsupply.csv')
#df.pivot2 <- dcast(df, district_ID ~ governorate_ID, fun.aggregate = sum, value.var=('petrol_gov_origin','diesel_gov_origin')