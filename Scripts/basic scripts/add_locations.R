add.location<-function(data){
  #library("dplyr")
  location.data <- read.csv("Scripts/basic scripts/pcodes/yem_admin_20171007.csv",header=T,sep=",", encoding = "UTF-8", check.names=F, stringsAsFactors=FALSE)
  location <- as.data.frame(location.data)
  
  #admin1Name
  location_merge1<-location[,c(colnames(location[c(4,3)]))]
  location_merge1 <- unique(location_merge1)
  dplyr::rename(location_merge1, governorate_ID = admin1Pcode) -> admin1_merge
  plyr::rename(admin1_merge, c("admin1Name_en" = "governorate_name")) -> admin1_merge
  
  
  #admin2Name
  location_merge2<-location[,c(colnames(location[c(7,5)]))]
  lOcation_merge2 <- unique(location_merge2)
  dplyr::rename(location_merge2, district_ID = admin2Pcode) -> admin2_merge
  plyr::rename(admin2_merge, c("admin2Name_en" = "district_name")) -> admin2_merge
  
  
  #Merge
  data <- dplyr::left_join(data, admin2_merge, by = "district_ID") 
  data <- dplyr::left_join(data, admin1_merge, by = "governorate_ID")
  
  #Add Country name and code
  data$country_name <- "Yemen"
  data$country_ID <- "YE"
  
  return(data)
}

