## Sub-setting JMMI dataset to consecutive months
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
# Load previous month
previous.month <- read_csv("Tests/january2019.csv")
current.month <- read_csv("Tests/february2019.csv")

# Select uniq ID from current month to match on previous month

uniqueID <- unique(previous.month$district_ID)

current.month2 <- current.month %>% subset(district_ID %in% uniqueID)

data <- current.month2

