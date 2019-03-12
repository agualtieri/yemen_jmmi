# Function to delete district with less than 3 observation
delete.districts <- function(data, by, minimum_required){
  data %>%
    dplyr::group_by_(by) %>%
    filter(n() >= minimum_required) -> data
  
}






  
  



