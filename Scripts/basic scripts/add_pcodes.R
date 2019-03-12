add.pcodes<-function(data){
  #library("dplyr")
  pcode.data <- read.csv("Scripts/basic scripts/pcodes/yem_admin_20171007.csv",header=T,sep=",", encoding = "UTF-8", check.names=F, stringsAsFactors=FALSE)
  pcode <- as.data.frame(pcode.data)
  
  #admin1Name
  pcode_merge1<-pcode[,c(colnames(pcode[c(4,3)]))]
  pcode_merge1 <- unique(pcode_merge1)
  dplyr::rename(pcode_merge1, governorate_name = admin1Name_en) -> admin1_merge
  plyr::rename(admin1_merge, c("admin1Pcode" = "governorate_ID")) -> admin1_merge

  
  #admin2Name
  pcode_merge2<-pcode[,c(colnames(pcode[c(7,5)]))]
  pcode_merge2 <- unique(pcode_merge2)
  dplyr::rename(pcode_merge2, district_name = admin2Name_en) -> admin2_merge
  plyr::rename(admin2_merge, c("admin2Pcode" = "district_ID")) -> admin2_merge
  
 
  #Merge
  data <- dplyr::left_join(data, admin2_merge, by = "district_name") 
  data <- dplyr::left_join(data, admin1_merge, by = "governorate_name")
  
  #Add Country name and code
  data$country_name <- "Yemen"
  data$country_ID <- "YE"
  
  return(data)
}


