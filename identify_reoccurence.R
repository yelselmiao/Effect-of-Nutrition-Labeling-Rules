# This script is to identify re-occuring items 

# library(tidyverse)
# library(dplyr)
# library(readxl)
# library(parsedate)
# library(openxlsx)
# library(fuzzywuzzyR)


# class <- "PDF 20 or more"
restaurant_name = "Qdoba"

directory_path = paste0("~/Dropbox/Restaurant Menu's shared workspace/Data/Panel Matching/Shuyi's test folder/", 
                        restaurant_name, 
                        "_panel.xlsx")

data <- read_excel(directory_path)
date_list <- list(unique(data$file_date))

while (i <= lengths(date_list)){
  current_file = data %>% filter(file_date == date_list[[1]][[i]])
  current_file_no_match = current_file %>% filter(post_fuzzy == 1) 
  for (item in current_file_no_match){
    for item_to_search in data %>% filter(file_date %notin% (date_list[[1]][[i]], date_list[[1]][[i+1]]){
      similarity =  fuzzywuzzyR::Partial_token_set_ratio(item, item_to_search)
    }
  }
  
}
  current_file = data %>% filter(file_date == date_list[[1]][[i]])
  current_file_no_match = current_file %>% filter(post_fuzzy == 1) 
  for (item in current_file_no_match: 
    for item_to_search in data %>% filter(file_date %notin% (date_list[[1]][[i]], date_list[[1]][[i+1]]): 
                                            
    









