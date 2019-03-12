# clear workspace
rm(list = ls())

  install.packages(c("rlang", "devtools","questionr" ))
  
  
  # check if package is installed
    .is.package.installed<-function(package.name){
      package.name %in% installed.packages()[,"Package"]
    }
  #  install dependencies if missing
    .install_dependencies<-function(packages.to.install){
      new.packages <- packages.to.install[!.is.package.installed(packages.to.install)]
      if(length(new.packages)) install.packages(new.packages)
      return(packages.to.install)
    }
    
  # install reachR
    
    .install_reachR<-function(reinstall_if_exists = F, branch="master"){
      if(!.is.package.installed("reachR") | reinstall_if_exists){
        # get devtools if needed
        if(!.is.package.installed("devtools")){install.packages("devtools")}
        require("devtools")
        install_github("mabafaba/reachR2",ref = branch)
        # unload devtools
        detach("package:devtools", unload=TRUE)
        if(reinstall_if_exists){
          warning("Please restart R session to update reachR documentation")
        }
      }
    }
    
    .install_reachR(T)

   
    
  require(reachR)
    
  # you might need some of these
  require("dplyr")
  require("knitr")
  require("data.table")
  require("questionr")  
  
  # set wd to this script's folder
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data:
#data<-load_data(file = "./data/october2018.csv")
  
  library(readr)
  data <- read_csv("data/january2019.csv")


# standard cleaning checks:

issues<-rbind(
  data %>% data_validation_all_others,
              data %>% data_validation_find_duplicates("row_id"), # usually check in UUID
              data %>% data_validation_outliers
)

issues %>% write.csv("./output/potential_issues.csv")


# percent by governorate:
    
    gov_percent<-data %>% aggregate_percent(split.by = "governorate.name.en",
                               write.to.file = "./output/gov_percent.csv",
                               ignore.missing.data = F)
  
  
# count by governorate to check sample size:
    
    gov_count<-data %>% aggregate_percent(split.by = "governorate.name.en",
                                            write.to.file = "./output/gov_count.csv",
                                            ignore.missing.data = F)
    
    
    
    
# what's up with different supplier types in a single row?
    # vendor_type_combos<-data[,grep("type.supplier",names(data))] %>% lapply(table) %>% melt  %>% head(20)
    #  they don't seem to  match, will ask what's up
    
    
    
# any sensitive information
data %>% names %>% grep("enum|gps|phone|num|name",.,value = T) %>% paste0(collapse="\n") %>% cat

    
# sample size by     
    
