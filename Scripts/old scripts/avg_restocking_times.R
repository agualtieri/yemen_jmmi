rm(list=ls())

library("dplyr")
library("reshape2")

working.directory <- "C:/Users/Audrey/Desktop/Joint Market Monitoring Initiative/3. data analysis/data" 
setwd(working.directory)

## Import ##

data.file <- "reach_jmmi_sept_validated.csv"
df.data <- read.csv(data.file,header=T,sep=",", encoding = "UTF-8", check.names=F)
df <- as.data.frame(df.data)

## Preparing data frame for the analysis // avg of restocking times##

df.restock.avg <- df %>%
  select(gov.name, contains("restock")) %>%
  group_by(gov.name) %>%
  summarise_all(funs(mean), na.rm = TRUE)

write.csv(df.restock.avg, file = 'avg_restocking_times_result_sept.csv')