library(tidyverse)
library(dplyr)
library(readxl)
library(parsedate)
library(openxlsx)

# Specify what restaurant
class <- "PDF 20 or more"
restaurant_name = "Cosi"
#directory_path <- paste0("~/Dropbox/Restaurant Menu's shared workspace/Data/Data Validation/Updated Dual_way Validation/PDF less than 20 JING/", restaurant_name)
directory_path = paste0("/Users/shuyitan/Dropbox/Restaurant Menu's shared workspace/Data/Data Validation/Updated Dual_way Validation/", 
                        class, "/", 
                        restaurant_name)

# read all files within the restaurant  
fi<-list.files(directory_path, full.names=T)
dat<-lapply(fi,read_excel)

# Preliminary clean:
# 1: remove useless columns before "Effective Date"
# 2. Since the column  "Effective Date" is empty in many files, I add an extra date based on the file name
# 3. Add a column of unique identifier

pre_clean <- function(file_list){
  for (i in 1:length(file_list)){
    df <- file_list[[i]]
    if (i == 1){
      df <- df %>% rename(pre_fuzzy_types = pre_fuzzy_item_types)
    }
    df <- df %>% 
      select(`Effective Date`:tidyselect::last_col()) %>% 
      add_column(id = NA,.before ="Effective Date") %>% 
      add_column(file_date = parse_date(fi[i]), .after ='id') 
    
    file_list[[i]] <- df
  }
  return(file_list)
}

dat <- pre_clean(dat)

# Initialize the first file, let's call it period t
t_file <- dat[[1]]

# These are items that have a match at period t + 1, we assign them ids
t_file_match <- t_file %>% 
  filter(post_fuzzy == 1) %>% 
  mutate(id = row_number()) %>% 
  mutate(id = paste(restaurant_name,'_', id)) 

# Start building the panel
panel <- t_file_match
i = 2

# Start iteration
while (i <= length(dat)){
# let's call file at period t+1 as t_1_file
  t_1_file <- dat[[i]]
  # For the items that have a match at period t + 1 from t, let's find where they are at t+1 and give them same ids 
  for (j in 1:nrow(t_file_match)){
    t_1_file$id[t_file_match$post_fuzzy_loc[j]] <- t_file_match$id[j]
  }
  
  # items at t+1 that have been matched to t
  t_1_file_match <- t_1_file %>% filter(pre_fuzzy == 1)
  # update the panel with items at t + 1 that were matched at t
  common_cols <- intersect(colnames(panel), colnames(t_1_file_match))
  panel <- rbind(subset(panel, select = common_cols),
                 subset(t_1_file_match, select = common_cols))
  
  # Now the t + 1 file will become the new t file 
  t_file <- t_1_file %>% filter(post_fuzzy == 1)
  # items that have matches in the next period
  t_file_match <- t_file %>% 
    mutate(id = ifelse(is.na(id), paste(restaurant_name, '_',nrow(panel) + row_number()), id))
  i = i + 1
}

panel <- panel %>% 
  group_by(id) %>% 
  filter(n() > 1) 

# writexl::write_xlsx(panel, "~/Dropbox/Restaurant Menu's shared workspace/Data/Panel Matching/Shuyi's test folder/boiling_point_panel.xlsx")