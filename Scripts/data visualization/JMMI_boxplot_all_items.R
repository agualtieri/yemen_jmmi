rm(list=ls())

library("dplyr")
library("plyr")
library("ggplot2")
library("reshape2")
library("readr")

#Set wd
setwd("~/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis/yemen_jmmi")

#Import
df <- read_csv("Outputs/governorate_smeb_march_2019.csv")

###Drop columns with empty headers
#Each time you merge a dataframe in R, a column with row numbers is added with no header
#We do not need all these row ID numbers, so we can remove them

#Create a logical vector where empty column headers TRUE and columns w/headers FALSE
empty.cols <- colnames(df) %in% c("","n")

#Subset data frame to keep only columns that are FALSE (ie have a header) in the vector
df <- df[!empty.cols]

####################
# Boxplots  #
####################

#Create Item List
items_list <- c("calc_price_petrol",
               "calc_price_diesel",
               "calc_price_bottled_water",
               "calc_price_treated_water",
               "calc_price_soap",
               "calc_price_laundry_powder",
               "calc_price_sanitary")

#Create Metadata list of variables to subset df.
metadata_list <- c("governorate_name")

#Create New Dataframe with only necessary metadata and items prices.
df %>%
  select(one_of(metadata_list)) -> metadata

df %>%
  select(one_of(items_list)) -> items

bind_cols(metadata, items) -> items_data

#Reshape Data so can create boxplots (fuzzy on how this works)
items_tidy <- melt(items_data)

#Rename Fuel Item so as to be presentable on Plot.
items_tidy$variable<-sub("calc_price_diesel","Diesel\n(1 L)",items_tidy$variable)
items_tidy$variable<-sub("calc_price_petrol","Petrol\n(1 L)",items_tidy$variable)
items_tidy$variable<-sub("calc_price_bottled_water","Bottled\nwater\n(0.75 L)",items_tidy$variable)
items_tidy$variable<-sub("calc_price_treated_water","Treated\nwater\n(10 L)",items_tidy$variable)
items_tidy$variable<-sub("calc_price_soap","Soap\n(1 Unit)",items_tidy$variable)
items_tidy$variable<-sub("calc_price_laundry_powder","Laundry\npowder\n(100 g)",items_tidy$variable)
items_tidy$variable<-sub("calc_price_sanitary","Sanitary\nnapkins\n(10 Units)",items_tidy$variable)


############
# Boxplots #
############


#(Note, need to fix water decimals, but can do manually if needed)
# This calculates the quantiles, median, min and max #
items_tidy$variable <-with(items_tidy,reorder(variable,value,function(x) -  median(x, na.rm=TRUE)))
f <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
medians <- ddply(items_tidy, .(variable), summarise, med = median(value,na.rm=TRUE))
mins <- ddply(items_tidy, .(variable), summarise, min = min(value,na.rm=TRUE))
max <- ddply(items_tidy, .(variable), summarise, max = max(value,na.rm=TRUE))

pdf("Visuals/boxplot_all_items2.pdf", width=13, height=6)
#Graphics/Boxplots/
#C:/Users/User/Links/Desktop/

plot<-ggplot(items_tidy, aes(variable,value, width = 0.18))
plot<-plot +   
  stat_summary(fun.data = f, geom="boxplot", fill = "#D1D3D4") +
  theme_bw() +
  xlab("") +
  ylab("Price (YER)") +
  geom_text(data = mins, aes(x=variable, y = min, label = format(round(min,digits=0), nsmall=0)),size = 4, vjust = 1.5)+
  geom_text(data = medians, aes(x=variable, y = med, label = format(round(med,digits=0), nsmall=0)),size = 4, hjust = -1.2) +
  geom_text(data = max, aes(x=variable, y = max, label = format(round(max,digits=0), nsmall=0)), size = 4, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 0, size = 12, hjust = 0.5, color = "black"))
plot
dev.off()

