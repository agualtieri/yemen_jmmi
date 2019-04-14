#Dynamic SMEB calculation
calculate.smeb <- function(data, level){
  
  #SMEB calculation according to level
  if (level == "district_ID"){
    
    #select only variable needed
    data %>% 
      dplyr::select("country_name", "country_ID", "governorate_ID", "governorate_name", "district_ID", "district_name", 
                    contains("calc_price_"), "cost_cubic_meter") -> data.select
    
    #Calculate weights
    data.select %>%
      dplyr::group_by(governorate_ID) %>%
      dplyr::mutate(
        smeb_soap = calc_price_soap*10.5,
        smeb_laundry = calc_price_laundry*20,
        smeb_napkins = calc_price_sanitary*2,
        smeb_cubic = cost_cubic_meter*3.15 
      ) -> data.select
    
    #Calculate proximity
      data.select %>% 
      dplyr::group_by(governorate_ID) %>%
      dplyr::mutate(
        smeb_soap= ifelse(is.na(smeb_soap), median(smeb_soap, na.rm=T), smeb_soap),
        smeb_laundry= ifelse(is.na(smeb_laundry), median(smeb_laundry, na.rm=T), smeb_laundry),
        smeb_napkins= ifelse(is.na(smeb_napkins), median(smeb_napkins, na.rm=T), smeb_napkins),
        smeb_cubic= ifelse(is.na(smeb_cubic), median(smeb_cubic, na.rm=T), smeb_cubic)
      ) -> data.select
      
    #Aggregate by median
      data.select %>%
        aggregate_median(level) -> data.select
    
    # Calculate SMEB
    data.select %>%
      dplyr::mutate(smeb_total = smeb_soap +
               smeb_laundry +
               smeb_napkins +
               smeb_cubic) -> data.select
    
    #view(data)
    
  } 
  else if (level == "governorate_ID"){
    
    #select only variable needed
    data %>% 
      dplyr::select("country_name", "country_ID","governorate_ID", "governorate_name",
                    contains("calc_price_"), "cost_cubic_meter") -> data.select
    
    #Calculate weights
    data.select %>%
      dplyr::group_by(governorate_ID) %>%
      dplyr::mutate(
        smeb_soap = calc_price_soap*10.5,
        smeb_laundry = calc_price_laundry*20,
        smeb_napkins = calc_price_sanitary*2,
        smeb_cubic = cost_cubic_meter*3.15 
    ) -> data.select
    
    #Calculate proximity
    data.select %>% 
      dplyr::group_by(governorate_ID) %>%
      dplyr::mutate(
        smeb_soap= ifelse(is.na(smeb_soap), median(smeb_soap, na.rm=T), smeb_soap),
        smeb_laundry= ifelse(is.na(smeb_laundry), median(smeb_laundry, na.rm=T), smeb_laundry),
        smeb_napkins= ifelse(is.na(smeb_napkins), median(smeb_napkins, na.rm=T), smeb_napkins),
        smeb_cubic= ifelse(is.na(smeb_cubic), median(smeb_cubic, na.rm=T), smeb_cubic)
      ) -> data.select
    
    #Aggregate by median
    data.select %>%
      aggregate_median(level) -> data.select
    
    # Calculate SMEB
    data.select %>%
      dplyr::mutate(smeb_total = smeb_soap +
               smeb_laundry +
               smeb_napkins +
               smeb_cubic) -> data.select
    
    #View(data)
    
  } 
  else if (level == "country_ID"){
    
    #select only variable needed
    data %>% 
      dplyr::select("country_name", "country_ID",
                    contains("calc_price_"), "cost_cubic_meter") -> data.select
    

    #Calculate weights
    data.select %>%
      dplyr::group_by(country_ID) %>%
      dplyr::mutate(
        smeb_soap = calc_price_soap*10.5,
        smeb_laundry = calc_price_laundry*20,
        smeb_napkins = calc_price_sanitary*2,
        smeb_cubic = cost_cubic_meter*3.15 
    ) -> data.select
    
    #Aggregate by median
    data.select %>%
      aggregate_median(level) -> data.select
      
    # Calculate SMEB
    data.select %>%
      dplyr::mutate(smeb_total = smeb_soap +
                 smeb_laundry +
                 smeb_napkins +
                 smeb_cubic) -> data.select
      
      #view(data)
      
  }
  else if (level == "market_name") {
    
    #select only variable needed
    data %>% 
      dplyr::select("country_name", "country_ID", "governorate_ID", "governorate_name", "district_ID", "district_name", 
                    "market_name", contains("calc_price_"), "cost_cubic_meter") -> data.select
    
    ## Created weighted values for each item
    data.select %>%
      dplyr::mutate(
        smeb_soap = calc_price_soap*10.5,
        smeb_laundry = calc_price_laundry*20,
        smeb_napkins = calc_price_sanitary*2,
        smeb_cubic = cost_cubic_meter*3.15 
      ) -> data.select
    
    #Aggregate by median
    data.select %>%
      aggregate_median(level) -> data.select
    
    # Calculate SMEB
    data.select %>%
      dplyr::mutate(smeb_total = smeb_soap +
               smeb_laundry +
               smeb_napkins +
               smeb_cubic) -> data.select
    
    #view(data)
    
  }
  
}


  
  
