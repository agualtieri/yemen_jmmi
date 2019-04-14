# Calculate district medians
calculate.medians <- function(data, level){
  
      
    if (level == "district_ID") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", "governorate_ID", 
                      "governorate_name", "district_ID", "district_name", 
                      contains("calc_price_"), "cost_cubic_meter", "exchange_rate_result") -> dist.select
      
      #Aggregate medians
      dist.select %>% aggregate_median(level)
      
    } 
    else if (level == "governorate_ID") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", "governorate_ID", 
                      "governorate_name", contains("calc_price_"), 
                      "cost_cubic_meter", "exchange_rate_result") -> gov.select
      
      #Aggregate medians
      gov.select %>% aggregate_median(level) 
      
      
    } 
    else if (level == "country_ID") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", contains("calc_price_"), 
                      "cost_cubic_meter", "exchange_rate_result") -> country.select
      
      #Aggregate medians
      country.select %>% aggregate_median(level)
      
      
    } 
    else if (level == "market_name") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", "governorate_ID", 
                      "governorate_name", "district_ID", "district_name", 
                      "market_name", contains("calc_price_"), "cost_cubic_meter", 
                      "exchange_rate_result") -> market.select
      
      #Aggregate medians
      market.select %>%  aggregate_median(level)
      
      
    }

}


