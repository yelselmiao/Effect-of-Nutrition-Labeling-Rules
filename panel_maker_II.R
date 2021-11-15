library(tidyverse)
library(dplyr)
library(readxl)
library(parsedate)
library(openxlsx)
library(stringr)

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

  
row_num = 0
for (i in 1:length(dat)){
  df <- dat[[i]]
    # assign different ids to all items
  if(i == 1){
      df <- df %>% 
        rename(pre_fuzzy_types = pre_fuzzy_item_types)}

  df <- df %>% 
    mutate(id = row_number() + row_num) %>% 
    mutate(id = sprintf("%04d", id)) %>%
    mutate(id = paste0(id, '_', restaurant_name)) %>% 
    select(`Effective Date`:tidyselect::last_col()) %>% 
    relocate(id, .before ="Effective Date") %>% 
    add_column(file_date = parse_date(fi[i]), .after ='id') 
  
  row_num <- row_num + nrow(df)
  
  dat[[i]] <- df
      
  }
  
  
# Initialize the first file, let's call it period t
t_file <- dat[[1]]


# Start building the panel
panel <- t_file  # this is the 1st file
i = 2

t_file_match <- t_file %>% 
     filter(post_fuzzy == 1) 


# Start iteration
while (i <= length(dat)){
  # let's call file at period t+1 as t_1_file
  t_1_file <- dat[[i]]
  # For the items that have a match at period t + 1 from t, let's find where they are at t+1 and give them same ids 
  for (j in 1:nrow(t_file_match)){
    t_1_file$id[t_file_match$post_fuzzy_loc[j]] <- t_file_match$id[j]
  }
  
  # items at t+1 that have been matched to t
  #t_1_file_match <- t_1_file %>% filter(pre_fuzzy == 1)
  
  # update the panel with items at t + 1 that were matched at t
  common_cols <- intersect(colnames(panel), colnames(t_1_file))
  panel <- rbind(subset(panel, select = common_cols),
                 subset(t_1_file, select = common_cols))
  
  # Now the t + 1 file will become the new t file 
  # and items that have matches in the next period
  t_file_match <- t_1_file %>% filter(post_fuzzy == 1)
  i = i + 1
}
  

#write.xlsx(panel, 'D:/panel_jing.xlsx', overwrite = T)


