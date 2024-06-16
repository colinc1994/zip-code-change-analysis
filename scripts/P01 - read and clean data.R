library(tidyverse)
library(readxl)
library(data.table)
rm(list = ls())
## NOTE: 1990-2000
# HAD TO DELETE DUPLICATE COLUMN NAMES ON DELEWARE, 
# THEY WERE NOT NEEDED, NOTHING IS THERE
# DELETED OLD STATE VARIABLE IN TEXAS BECAUSE IT WAS EMPTY

## going to do this one at a time, there is probably a better way but it's three 
# chunks so this shouldn't be bad


######################## 1990 - 2000
# get sheet names
sheets_temp = readxl::excel_sheets(file.path("Data",
                                             list.files("Data/",
                                                               pattern = "1990-2000")[1]))
# read in data and combine rows
zc_change_90_00 = lapply(sheets_temp,function(i){
temp = read_excel(file.path("Data",list.files("Data/",pattern = "1990-2000")[1]),
                  sheet = i,)
#temp = temp %>% na.omit()
colnames(temp) = colnames(temp) %>% make.names()
temp = temp %>% 
  mutate(across(everything(), as.character),
         State = i)

temp %>%  return()
}) %>% 
  do.call(bind_rows,.)

######################### 2000-2010
# get sheet names
sheets_temp = readxl::excel_sheets(file.path("Data",
                                             list.files("Data/",
                                                        pattern = "2000-2010")[1]))
# read data
zc_change_00_10 = lapply(sheets_temp,function(i){
  temp = read_excel(file.path("Data",list.files("Data/",pattern = "2000-2010")[1]),
                    sheet = i,)
  #temp = temp %>% na.omit()
  colnames(temp) = colnames(temp) %>% make.names()
  temp = temp %>% 
    mutate(across(everything(), as.character),
           State = i) 
  
  temp %>%  return()
}) %>% 
  do.call(bind_rows,.)

######################## 2010 - 2020
# get sheet names
sheets_temp = readxl::excel_sheets(file.path("Data",
                                             list.files("Data/",
                                                        pattern = "2010-2020")[1]))
# read data
zc_change_10_20 = lapply(sheets_temp,function(i){
  temp = read_excel(file.path("Data",list.files("Data/",pattern = "2010-2020")[1]),
                    sheet = i,)
  #temp = temp %>% na.omit()
  colnames(temp) = colnames(temp) %>% make.names()
  temp = temp %>% 
    mutate(across(everything(), as.character),
           State = i)
  
  temp %>%  return()
}) %>% 
  do.call(bind_rows,.)

# check the column names, they really should all be the same
zc_change_00_10 %>% colnames()
zc_change_10_20 %>% colnames()
zc_change_90_00 %>% colnames()
# checking the column names, there is a variable that should be remove in the 1990 data
zc_change_90_00 = zc_change_90_00 %>% 
  select(-...10)

# I think it is safe now to smart bind everything
zc_change_df = list(zc_change_00_10,
     zc_change_10_20,
     zc_change_90_00) %>% 
  do.call(bind_rows,.) %>% 
  mutate(New.County = ifelse(is.na(New.County),County,New.County), # the new county variable was just county for some states
         State = State %>% gsub("-","",.)) # need to remove - which was in there for some reason


# make an indicator whether the change was to establish a new zip code for a delivery are
zc_change_df = zc_change_df %>% 
  mutate(est_for_del = ifelse(grepl("Establish a new ZIP Code for a delivery area.",
                                    Comments),"Yes","No")) %>% 
  mutate(new_date = as.Date(Effective.Date %>% as.numeric(), origin = "1899-12-30") %>% as.character()) %>% 
  mutate(new_date = ifelse(grepl("-",Effective.Date),
                           Effective.Date %>% lubridate::ymd() %>% as.character(),
                           new_date)) %>% 
  filter(!(is.na(Old.ZIP.Code)&is.na(New.ZIP.Code))) %>% 
  mutate(check_brand_new = ifelse(is.na(Old.ZIP.Code)&!is.na(New.ZIP.Code),
                                  "Yes","No")) %>% 
  mutate(effect_year = new_date %>% lubridate::year())

# do a freq count of the states
zc_change_df %>% 
  select(State) %>% table() %>% sort()
# FL, CA, KY were the top three in changes

