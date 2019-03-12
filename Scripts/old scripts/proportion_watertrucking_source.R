rm(list=ls())

library("tidyr")
library("dplyr")
library("stringr")

working.directory <- "C:/Users/Audrey/Desktop/Joint Market Monitoring Initiative/3. data analysis/data" 
setwd(working.directory)

## Import ##

data.file <- "reach_jmmi_sept_validated.csv"
df.data <- read.csv(data.file,header=T,sep=",", encoding = "UTF-8", check.names=F, na.strings = c("", "NA"))
df <- as.data.frame(df.data)

## Proportion of water source mentioned ##

df.water.source <- df %>%
  select(type_water) %>%
  filter(!is.na(type_water)) %>%
  group_by(type_water) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.water.source, file = 'water.source.csv')

## Proportion of delivery costs ##

df.delivery.costs <- df %>%
  select(distance_price) %>%
  filter(!is.na(distance_price)) %>%
  group_by(distance_price) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.delivery.costs, file = 'delivery.costs.csv')

## Proportion of chlorinated water ##

df.chlorinated.water <- df %>%
  select(water_chlorinated) %>%
  filter(!is.na(water_chlorinated)) %>%
  group_by(water_chlorinated) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.chlorinated.water, file = 'df.chlorinated.water.csv')

## Proportion of type of owner ##

df.type.owner <- df %>%
  select(type_owner) %>%
  filter(!is.na(type_owner)) %>%
  group_by(type_owner) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

write.csv(df.type.owner, file = 'df.type.owner.csv')