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

## Manipulation of data: selection of challenge columns ##

df.challenges <- df %>%
  unite(AllConstraints, c(petrol_constraints_multiple, diesel_constraints_multiple, bottled_constraints_multiple, treated_constraints_multiple, soap_constraints_multiple, napkins_constraints_multiple, laundry_constraints_multiple, constraints_multiple), sep = " ", remove = TRUE) %>%
  select(AllConstraints)

## TRUE/FALSE when challenge is mentionned in row ##

df.challenges$price_inflation <- str_detect(df.challenges$AllConstraints, "Price") == TRUE
df.challenges$liquidity_shortage <- str_detect(df.challenges$AllConstraints, "Liquidity") == TRUE
df.challenges$shortage_demand <- str_detect(df.challenges$AllConstraints, "demand") == TRUE
df.challenges$insecurity_instability <- str_detect(df.challenges$AllConstraints, "Insecurity") == TRUE
df.challenges$supply_shortage <- str_detect(df.challenges$AllConstraints, "Supply") == TRUE
df.challenges$government_regulations <- str_detect(df.challenges$AllConstraints, "Government") == TRUE
df.challenges$transportation_issues <- str_detect(df.challenges$AllConstraints, "Transportation") == TRUE
df.challenges$other <- str_detect(df.challenges$AllConstraints, "Other") == TRUE
df.challenges$do_not_know <- str_detect(df.challenges$AllConstraints, "Do_not_know") == TRUE
df.challenges$no_constraints <- str_detect(df.challenges$AllConstraints, "No_constraints") == TRUE
df.challenges$vendor_did_not_answer <- str_detect(df.challenges$AllConstraints, "Vendor") == TRUE

## Calculate the proportion of challenges ##

mean_challenges <- colMeans(df.challenges == "TRUE", na.rm = FALSE)

write.csv(mean_challenges, file = 'challenges_result_sept.csv')