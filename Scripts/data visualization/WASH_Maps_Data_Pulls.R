sd(data.frame.validated$distance_travelled, na.rm=T)


#Nates way
data.frame.validated$dist_score<-0
min_dist<-min(data.frame.validated$distance_travelled, na.rm=T)
max_dist<-max(data.frame.validated$distance_travelled, na.rm=T)
min_price<-min(data.frame.validated$cost_cubic_meter, na.rm=T)
max_price<-max(data.frame.validated$cost_cubic_meter, na.rm=T)

data.frame.validated$dist_score<-(data.frame.validated$cost_cubic_meter-min_price)/(data.frame.validated$distance_travelled-min_dist)


maps_dist_score_med<-aggregate(dist_score ~ district_ID, data = data.frame.validated, FUN = median, na.rm=T)
maps_dist_score_mean<-aggregate(dist_score ~ district_ID, data = data.frame.validated, FUN = mean, na.rm=T)
names(maps_dist_score_med)[names(maps_dist_score_med) == 'dist_score']<-'Single_Score_Med'
names(maps_dist_score_mean)[names(maps_dist_score_mean) == 'dist_score']<-'Single_Score_Mean'
#write.csv(maps_dist_score_med, file = paste0("Outputs/WASH Maps/maps_dist_score_med_",current_month,".csv"), row.names = FALSE)
#write.csv(maps_dist_score_mean, file = paste0("Outputs/WASH Maps/maps_dist_score_mean_",current_month,".csv"), row.names = FALSE)


#Naras Way - Quartile

#########################
#Naras district median 
 # Select data
  data.frame.validated %>% 
    dplyr::select("country_name", "country_ID", "governorate_ID", 
                  "governorate_name", "district_ID", "district_name", 
                  "cost_cubic_meter", "distance_travelled","type_area") -> dist.select
  
  #Aggregate medians
  dist.select %>% aggregate_median("district_ID")->district_median_test


district_median_test$dist_quar_score<-0
quart_dist<-quantile(district_median_test$distance_travelled, na.rm=T)
Q_D1<-as.numeric(quart_dist[2])
Q_D4<-as.numeric(quart_dist[4])

district_median_test$dist_quar_score[district_median_test$distance_travelled<=Q_D1] <- "Low"
district_median_test$dist_quar_score[district_median_test$distance_travelled>=Q_D1 & district_median_test$distance_travelled<=Q_D4] <- "Med"
district_median_test$dist_quar_score[district_median_test$distance_travelled>=Q_D4] <- "High"


district_median_test$price_quar_score<-0
quart_price<-quantile(district_median_test$cost_cubic_meter, na.rm=T)
Q_P1<-as.numeric(quart_price[2])
Q_P4<-as.numeric(quart_price[4])

district_median_test$price_quar_score[district_median_test$cost_cubic_meter<=Q_P1] <- "Low"
district_median_test$price_quar_score[district_median_test$cost_cubic_meter>=Q_P1 & district_median_test$cost_cubic_meter<=Q_P4] <- "Med"
district_median_test$price_quar_score[district_median_test$cost_cubic_meter>=Q_P4] <- "High"


district_median_test$tot_quar_score<-0
district_median_test$tot_quar_score<-paste(district_median_test$dist_quar_score, district_median_test$price_quar_score)
table(district_median_test$tot_quar_score)
names(district_median_test)[names(district_median_test) == 'dist_quar_score']<-'Dist_Quart_Score_Med'
names(district_median_test)[names(district_median_test) == 'price_quar_score']<-'Price_Quart_Score_Med'
names(district_median_test)[names(district_median_test) == 'tot_quar_score']<-'Tot_Quart_Score_Med'
#write.csv(district_median_test, file = paste0("Outputs/WASH Maps/dist_med_NARA_",current_month,".csv"), row.names = FALSE)

#########################
#Naras district mean 
# Select data
data.frame.validated %>% 
  dplyr::select("district_ID","cost_cubic_meter", "distance_travelled") -> dist.select



#Aggregate medians

district_mean_test<-aggregate(. ~ district_ID, data=dist.select, mean, na.rm=T)
  
district_mean_test<-inner_join(district_mean_test,district_median_test, by = "district_ID")

district_mean_test<-district_mean_test[,-c(8:13)]


district_mean_test$dist_quar_score<-0
quart_dist<-quantile(district_mean_test$distance_travelled, na.rm=T)
Q_D1<-as.numeric(quart_dist[2])
Q_D4<-as.numeric(quart_dist[4])

district_mean_test$dist_quar_score[district_mean_test$distance_travelled<=Q_D1] <- "Low"
district_mean_test$dist_quar_score[district_mean_test$distance_travelled>Q_D1 & district_mean_test$distance_travelled<Q_D4] <- "Med"
district_mean_test$dist_quar_score[district_mean_test$distance_travelled>=Q_D4] <- "High"


district_mean_test$price_quar_score<-0
quart_price<-quantile(district_mean_test$cost_cubic_meter, na.rm=T)
Q_P1<-as.numeric(quart_price[2])
Q_P4<-as.numeric(quart_price[4])

district_mean_test$price_quar_score[district_mean_test$cost_cubic_meter<=Q_P1] <- "Low"
district_mean_test$price_quar_score[district_mean_test$cost_cubic_meter>Q_P1 & district_mean_test$cost_cubic_meter<Q_P4] <- "Med"
district_mean_test$price_quar_score[district_mean_test$cost_cubic_meter>=Q_P4] <- "High"


district_mean_test$tot_quar_score<-0
district_mean_test$tot_quar_score<-paste(district_mean_test$dist_quar_score, district_mean_test$price_quar_score)
table(district_mean_test$tot_quar_score)
names(district_mean_test)[names(district_mean_test) == 'dist_quar_score']<-'Dist_Quart_Score_Mean'
names(district_mean_test)[names(district_mean_test) == 'price_quar_score']<-'Price_Quart_Score_Mean'
names(district_mean_test)[names(district_mean_test) == 'tot_quar_score']<-'Tot_Quart_Score_Mean'
#write.csv(district_mean_test, file = paste0("Outputs/WASH Maps/dist_mean_NARA_",current_month,".csv"), row.names = FALSE)



#Normal Mean and Median

maps_median<-aggregate(cbind(distance_travelled,cost_cubic_meter)~ district_ID, data = data.frame.validated, FUN = median, na.rm=T)

maps_mean<-aggregate(cbind(distance_travelled,cost_cubic_meter)~ district_ID, data = data.frame.validated, FUN = mean, na.rm=T)

print(maps_mean)
print(maps_median)

names(maps_mean)[names(maps_mean) == 'distance_travelled']<-'Norm_Dist_Mean'
names(maps_mean)[names(maps_mean) == 'cost_cubic_meter']<-'Norm_Price_Mean'
names(maps_median)[names(maps_median) == 'distance_travelled']<-'Norm_Dist_Median'
names(maps_median)[names(maps_median) == 'cost_cubic_meter']<-'Norm_Price_Median'

#write.csv(maps_mean, file = paste0("Outputs/WASH Maps/maps_mean_",current_month,".csv"), row.names = FALSE)
#write.csv(maps_median, file = paste0("Outputs/WASH Maps/maps_median_",current_month,".csv"), row.names = FALSE)



final_WASH_map<-Reduce(function(x,y) merge(x,y, by="district_ID",all=T),list(maps_mean,
                                                             maps_median,
                                                             district_mean_test,
                                                             district_median_test,
                                                             maps_dist_score_mean,
                                                             maps_dist_score_med))

final_WASH_map<-final_WASH_map[,!grepl(".x",colnames(final_WASH_map))]
final_WASH_map<-final_WASH_map[,!grepl("distance_travelled",colnames(final_WASH_map))]
final_WASH_map<-final_WASH_map[,!grepl("cost_cubic_meter",colnames(final_WASH_map))]

final_WASH_map<-final_WASH_map[,c(1,13,9,10,11,12,14,2,3,4,5,6,7,8,15,16,17,18,19)]
final_WASH_map$type_area[is.na(final_WASH_map$type_area)]<-"Even Obs"

write.csv(final_WASH_map, file = paste0("Outputs/WASH Maps/AGGREGATE_WASH_MAP_DATA_",current_month,".csv"), row.names = FALSE)
