##%######################################################%##
#                                                          #
####       Clean Kimball data from Kimball County       ####
####      Shape file.show 2022-10-09  Date of Data      ####
#                LAST DATA IMPORT 2022-02-06               #
##%######################################################%##

library(tictoc)

library(tidyverse)
library(janitor)
library(tictoc)
library(stringr)
library(lubridate)
library(arrow)
library(here)
library(tidyr)
library(gt)
library(kableExtra)

# library(clipr)
# library(datapasta)


# Import Data -------------------------------------------------------------



parcels0 <- read_csv("data/Kimball_County_NE.dbf.csv") %>% 
    clean_names()

parcels1 <- parcels0 %>% 
  select(pid, county_nam, owner_name, owner_addr, owner_city, owner_stat, owner_zip, subdivisio, legal_desc, property_t, land_value, sale_date, shape_area) %>% 
  mutate(acres = shape_area/43560) %>% 
  mutate(sale_date = ymd(sale_date)) %>% 
  rename(county = county_nam,
         subdivision = subdivisio,
         address = owner_addr,
         city = owner_city,
         state = owner_stat,
         zip = owner_zip) %>% 
  select(-shape_area)

parcels1$county <- "Kimball"


###########################################################################

# Modify owner_name -------------------------------------------------------

###########################################################################


# using stringr -----------------------------------------------------------

?separate
# Reference from Equitable Equations:  https://www.youtube.com/watch?v=6I1bPT_suuw

parcels2 <- parcels1 %>% 
  separate(owner_name, c("lastname", "firstname0"), "/")  %>% 
  separate(col = firstname0, 
           into = c("firstname", "capacity"),
           ## Regex Expression 
           sep =  "\\(",  
            remove = FALSE) %>% 
  mutate(capacity= str_squish(capacity)) %>% 
  select(-firstname0)

## https://www.youtube.com/watch?v=2J3Te_bug0c&t=3s 
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TEE )", replacement = "TRUSTEE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TSTS )", replacement = "TRUSTEE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TEE'S )", replacement = "TRUSTEE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TTE'S )", replacement = "TRUSTEE ")

parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TTE )", replacement = "TRUSTEE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TTES )", replacement = "TRUSTESE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(CO-TTE )", replacement = "TRUSTEE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(CO TTES )", replacement = "TRUSTESE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TRTS )", replacement = "TRUSTEES ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(TTES )", replacement = "TRUSTEES ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(CO-TTES )", replacement = "TRUSTEES ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(JR CO-TRTS )", replacement = "JR, TRUSTEES ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(JR TTE& )", replacement = "JR, TRUSTEE ")
parcels2$capacity <-   str_replace(parcels2$capacity, pattern = "^(JR TTE & )", replacement = "JR, TRUSTEE ")



# check "capacity" --------------------------------------------------------


x2 <- parcels2 %>% 
  select(capacity) %>% 
  drop_na()



# CREATE OWNERNAME:   combine firstname & lastname -> ownername -----------


parcels3 <- parcels2 %>% 
  unite("ownername", 4:3, na.rm = TRUE, remove = FALSE)

 

write_csv(x2, "data/x_temp.csv")


# Clean Legal Description -------------------------------------------------



parcels3 %>% 
  select(legal_desc) %>% 
  head()

#### Step one - Extract Section-Township-Range 

parcels4 <- parcels3 %>% 
  mutate(legal = str_squish(legal_desc)) %>% 
  # at the beginning of the string - exclude the space and everything after the space.
  mutate(Section = str_extract(legal_desc, "^[^\\s]+"),
         # extract everything following the space   
         legal = str_extract(legal_desc, "\\s.+")) %>% 
  mutate(legal = str_squish(legal)) %>% 
  select(-legal_desc)


parcels4 %>% 
 # mutate(TR = str_extract(legal, "\\s.+")) %>% 
  mutate(legal = str_squish(legal)) %>% 
  mutate(legal2 = str_extract(legal, "^[^\\s]+")) %>% 
  select(legal2)


parcels5 <- parcels4 %>% 
  mutate(
    TR = str_extract(legal, ".{0,5}")) %>% 
  mutate(legal = str_replace(legal, ".{0,5}", "")) %>% 
  mutate(TR = str_replace(TR,  "\\s", "-")) %>% 
  unite("TRS", c("TR", "Section"), sep="-") %>% 
  mutate(legal = str_squish(legal))


# REMOVE CARD  ------------------------------------------------------------
parcels5$legal

parcels6 <- parcels5 %>% 
mutate(legal = gsub("\\([^()]*\\)", "", parcels5$legal)) %>% 
  mutate(legal = str_squish(legal)) %>% 
  
#  REMOVE THE STR at the end of the string  
  mutate(legal = str_replace(legal, "(\\S*)$", "")) %>% 
  mutate(legal = str_squish(legal)) %>% 
  select(pid, county, ownername, capacity, address, city, state, zip, subdivision, TRS, legal, acres, firstname, lastname, property_t, land_value, sale_date)
  
 ## Not working:  https://www.statology.org/dplyr-relocate/ 

parcels7 <- parcels6[c("pid", "county", "ownername", "capacity", "address", "city", "state", "zip", "subdivision", "TRS", "legal", "acres", "firstname", "lastname", "property_t", "land_value", "sale_date")]

sum(str_count(parcels5$legal, " \\s*\\([^\\)]+\\)"))

sum(str_count(parcels5$legal, " ^([(\\)]+\\)"))
