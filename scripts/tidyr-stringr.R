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



# split at first whitespace  ----------------------------------------------



parcels %>% 
  mutate(col = str_replace(legal_desc, "\\s", "|")) %>%              #   str_replace()
  separate(col, into = c("SRT", "legal"), sep = "\\|")              #                   separate()


x1 <- parcels %>% 
  mutate(col = str_replace(legal_desc, "\\s", "|")) %>% 
  separate(col, into = c("SRT", "legal"), sep = "\\|")

head(x1) 


# split at first whitespace - then merge ---------------------------------- 
### extra = "merge" gets rid of the column which has been split

x2 <- separate(parcels, legal_desc, into = c("STR", "legal"), sep = "\\s",
         extra = "merge")



# split at first space using tidyr::str_extract()   -----------------------


x3 <- parcels %>% 
  # at the beginning of the string - excluide the space and everthing after the space.
  mutate(SRT = str_extract(legal_desc, "^[^\\s]+"),
  # extract everythinng following the space   
         legal = str_extract(legal_desc, "\\s.+")) %>% 
  
  select(-legal_desc)
x3


# RESORT ORDER OF SRT TO TRS ----------------------------------------------


# resort order of SRT to TRS ----------------------------------------------
 
###     \\d+ means "one or more digits". That lets us not care if it's 1 or 2.
###     If we anchor to the beginning or end, we can grab what we need.

xtrs <- x3 %>% 
  mutate(
    TRS = paste(
      str_extract(SRT, "\\d+-\\d+$"),
      str_extract(SRT, "^\\d+"),
      sep = "-"
    )
  )

xtrs

# Remove whitespace from 'legal' -------------------------------------------------------------

x4 <- x3 %>% 
  mutate(legal = str_squish(legal))

x4 %>% 
  mutate(legal= str_replace("\\~", ""))

x4$legal

x4$legal

xlot <- x3 %>% 
  mutate(LOT = str_extract(SRT, "LOT")) %>% 
  relocate(LOT, .before = legal) %>% 
  unite('Merged', LOT:legal, remove = FALSE)  

xlot2 <- xlot %>% 
  mutate(Merged = str_replace(Merged, "LOT_", ""))

xlot2

xlot2$Merged



         legal = str_extract(legal_desc, "\\s.+")) %>% 
  select(-legal_desc)
x3


# extract Section from SRT ------------------------------------------------



x4 <- parcels %>% 
  mutate(S = str_extract(legal_desc, "^[^\\-]+"),
         # legal = str_extract(legal_desc, "\\-.+")) %>% 
  #       legal = str_extract(legal_desc, "^(\\w+)\\s?(.*)$")) %>% 
            legal = str_extract(legal_desc, "([\-]).*(\1)")) %>% 
  select(-legal_desc)
x4

x4 %>% 
  count()

myvar <- c("2-15-30", "20-15-30", "34-17-35", "7-12-22")
df <- enframe(myvar, name="var") 

df <- tibble(myvar, name = "var1")

df
myvar <- names("var1")

class(df)
class(myvar)
myvar



# remove tilde ------------------------------------------------------------



df <- parcels %>% 
  mutate(col = str_replace(legal_desc, "\\~$", "|")) %>% 
  mutate(col = str_replace(col, "\\|", ""))


df <- parcels %>% 
  mutate(col = str_replace(legal_desc, "\\~$", ""))

df$col



