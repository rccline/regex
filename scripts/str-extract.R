library(tidyverse)
library(janitor)
library(tictoc)
library(stringr)
library(lubridate)
library(arrow)
library(here)
library(tidyr)

##%######################################################%##
#                                                          #
####    Using tidyr::extract()  and str_replace() to    ####
####           split a string into two columns          ####
#                                                          #
##%######################################################%##  



# import data -------------------------------------------------------------



parcels0 <- read_parquet(here("data/cheyenne_parcels.parquet")) %>% 
  clean_names()

 

parcels <- parcels0 %>% 
  rename(pid = parcel_id) %>% 
  mutate(acres = shape_area/43560) %>% 
  select(pid, current_ow, street_add, legal_desc, acres, sales_date) 


# extract with tidyr::extract()   -----------------------------------------



pattern = "(.*) ,(\\d{5}-\\d{4})"

x <- parcels %>%
  tidyr::extract(street_add, into = c("address", "zip"), regex = pattern)  


# str_replace() -----------------------------------------------------------



capture_groups <- "(.+?)\\s*,\\s*(\\d{5}-\\d{4})"

x2 <- parcels %>% 
  mutate(address = str_replace(street_add, capture_groups, "\\1"),
         zip     = str_replace(street_add, capture_groups, "\\2"))

 