### SMEB CALCULATION / FUNCTION ###

smeb_calculation <- function(data, level){
  
  `%!in%` = Negate(`%in%`)
  
  ## Load SMEB items ##
  #smeb.items <- c("country_name",
                  #"country_ID",
                  #"governorate_name",
                  #"governorate_ID",
                  #"district_name",
                  #"district_ID",
                  #"price_soap_normalised", 
                  #"price_laundry_powder_normalised", 
                  #"price_sanitary_napkins_normalised",
                  #"cost_cubic_meter")
  
  data.select <- data %>% dplyr::select("country_name", "country_ID", "market_name", "governorate_ID", "governorate_name", "district_ID", "district_name", contains("normalised"), "cost_cubic_meter", "exchange_rate_result")
  
  ## Created weighted values for each item
  data %>%
        mutate(
           smeb_soap = price_soap_normalised*10.5,
           smeb_laundry = price_laundry_powder_normalised*20,
           smeb_napkins = price_sanitary_napkins_normalised*2,
           smeb_cubic = cost_cubic_meter*3.15 
           ) -> data
  

  ## Proximity analysis -> use prices coming from other markets in the same governorate if available
  data %>% 
     dplyr::group_by(governorate_ID) %>%
       dplyr::mutate(
             smeb_soap= ifelse(is.na(smeb_soap), median(smeb_soap, na.rm=T), smeb_soap),
             smeb_laundry= ifelse(is.na(smeb_laundry), median(smeb_laundry, na.rm=T), smeb_laundry),
             smeb_napkins= ifelse(is.na(smeb_napkins), median(smeb_napkins, na.rm=T), smeb_napkins),
             smeb_cubic= ifelse(is.na(smeb_cubic), median(smeb_cubic, na.rm=T), smeb_cubic)
             ) -> data
  
  
  ## Calculate total semb  
  data %>%
    dplyr::mutate(smeb_total = smeb_soap +
             smeb_laundry +
             smeb_napkins +
             smeb_cubic) -> data
  
  print(median_smeb)
    
  }


data2 <- smeb_calculation(data)
