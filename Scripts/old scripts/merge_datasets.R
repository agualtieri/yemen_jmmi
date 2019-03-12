## Merging JMMI datasets ###
rm(list=ls())
library("mergekobodata")
browseVignettes("mergekobodata")

## Merge all datasets
merge_kobo_data(folder = "Scripts/all JMMI datasets/", output_file = "Scripts/all JMMI datasets/jmmi_merged.csv")
