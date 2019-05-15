rm(list=ls())

library("dplyr")
library("plyr")
library("ggplot2")
library("reshape2")
library("readr")

#Set wd
setwd("~/REACH Yemen/2. Cash & Markets/1. Joint Market Monitoring Initiative (JMMI)/4. Data Analysis/yemen_jmmi")


#Import
library(readr)
df <- read_csv("Outputs/governorate_smeb_april_2019.csv")

# Months -> update according to the month being analyzed
current_month <- "april_2019"

###Drop columns with empty headers
#Each time you merge a dataframe in R, a column with row numbers is added with no header
#We do not need all these row ID numbers, so we can remove them

#Create a logical vector where empty column headers TRUE and columns w/headers FALSE
empty.cols <- colnames(df) %in% c("","n")

#Subset data frame to keep only columns that are FALSE (ie have a header) in the vector
df <- df[!empty.cols]

####################
# Water trucking Boxplots  #
####################

#Create Item List
truck_list <- "cost_cubic_meter"

#Create Metadata list of variables to subset df.
metadata_list <- "governorate_name"

#Create New Dataframe with only necessary metadata and food prices.
df %>%
  dplyr::select(one_of(metadata_list)) -> metadata

df %>%
  dplyr::select(one_of(truck_list)) -> truck

bind_cols(metadata, truck) -> truck_data

#Reshape Data so can create boxplots (fuzzy on how this works)
truck_tidy <- melt(truck_data)

#Rename Food Item so as to be presentable on Plot.
truck_tidy$variable<-sub("cost_cubic_meter", "", truck_tidy$variable)
                         

############
# Boxplots #
############

#(Note, need to fix water decimals, but can do manually if needed)
# This calculates the quantiles, median, min and max #
truck_tidy$variable <-with(truck_tidy,reorder(variable,value,function(x) -  median(x, na.rm=TRUE)))
f <- function(x) {
  r <- quantile(x, probs = c(0.00, 0.25, 0.5, 0.75, 1))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
medians <- ddply(truck_tidy, .(variable), summarise, med = median(value,na.rm=TRUE))
mins <- ddply(truck_tidy, .(variable), summarise, min = min(value,na.rm=TRUE))
max <- ddply(truck_tidy, .(variable), summarise, max = max(value,na.rm=TRUE))

pdf(past0("Visuals/boxplot_water_truck1_",current_month,".pdf"), width=3, height=6)
?pdf
#Graphics/Boxplots/
#C:/Users/User/Links/Desktop/

plot<-ggplot(truck_tidy, aes(variable,value, width = 0.16))
plot<-plot +   
  stat_summary(fun.data = f, geom="boxplot", fill = "#D1D3D4") +
  theme_bw() +
  xlab(expression(paste("Water trucking (1",m^3, ")" ))) +
  ylab("Price (YER)") +
  geom_text(data = mins, aes(x=variable, y = min, label = format(round(min,digits=0), nsmall=0)),size = 3.5, vjust = 1.5)+
  geom_text(data = medians, aes(x=variable, y = med, label = format(round(med,digits=0), nsmall=0)),size =3.5, hjust = -1.3) +
  geom_text(data = max, aes(x=variable, y = max, label = format(round(max,digits=0), nsmall=0)), size =3.5, vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 0, size = 12, hjust = 0.5, color = "black"))
plot
dev.off()


