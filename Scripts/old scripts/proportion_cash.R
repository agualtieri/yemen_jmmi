rm(list=ls())

library("tidyr")
library("dplyr")
library("stringr")

working.directory <- "C:/Users/Audrey/Desktop/Joint Market Monitoring Initiative/3. data analysis/data" 
setwd(working.directory)

## Import csv ##

data.file <- "reach_jmmi_sept_validated.csv"
df.data <- read.csv(data.file,header=T,sep=",", encoding = "UTF-8", check.names=F)
df <- as.data.frame(df.data)

## Manipulation of data: selection of cash columns and calculating proportion of cash modalities ##

df.cash <- df %>%
  select(contains("cash_feasibility"))

mean.cash <- colMeans(df.cash == TRUE, na.rm = TRUE)

write.csv(mean.cash, file = 'cash.result.sept.csv')