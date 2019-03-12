# Calculate district medians
calculate.medians <- function(data, level){
      
    if (level == "district_ID") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", "governorate_ID", 
                      "governorate_name", "district_ID", "district_name", 
                      contains("normalised"), "cost_cubic_meter", "exchange_rate_result") -> data.select
      
      #Aggregate medians
      data.select %>% 
       aggregate_median(level) -> data.select
      
    } 
    else if (level == "governorate_ID") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", "governorate_ID", 
                      "governorate_name", contains("normalised"), 
                      "cost_cubic_meter", "exchange_rate_result") -> data.select
      
      #Aggregate medians
      data.select %>% 
        aggregate_median(level) -> data.select
      
      
    } 
    else if (level == "country_ID") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", contains("normalised"), 
                      "cost_cubic_meter", "exchange_rate_result") -> data.select
      
      #Aggregate medians
      data.select %>% 
        aggregate_median(level) -> data.select
      
      
    } 
    else if (level == "market_name") {
      
      # Select data
      data %>% 
        dplyr::select("country_name", "country_ID", "governorate_ID", 
                      "governorate_name", "district_ID", "district_name", 
                      "market_name", contains("normalised"), "cost_cubic_meter", 
                      "exchange_rate_result") -> data.select
      
      #Aggregate medians
      data.select %>% 
        aggregate_median(level) -> data.select
      
      
    }

}


