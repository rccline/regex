library(readxl)
library(tidyverse)
library(stringr)


# https://www.youtube.com/watch?v=vf0wDeg2YPI&t=4s 
# Clean tax roll Legals 
# Basic Regex reference  https://www.regular-expressions.info/quickstart.html 



# data
pspace0 <- read_excel("E:/tallgrass-kimball/shape/merged-co2-area.dbf.xlsx")

legal <- pspace0 %>% 
  select(legal)

head(legal)


# remove (CARD #16A-1)  --------------------------------------------------- 



pspace1 <- pspace0 %>% 
  mutate(legal = gsub("\\([^()]*\\)", "", pspace$legal))

# gsub(".*?([0-9]{1,2}", pspace$legal)

# Count the number of (CARD xxx) strings 

sum(str_count(pspace1$legal, " \\s*\\([^\\)]+\\)"))



# remove STR from string "legal" ------------------------------------------

clean_legal <- pspace1 %>% 
  mutate(legal = str_replace_all(legal, "(\\d{1,2}\\-?)(\\d{2}\\-?)(\\d{2})",
                                 ""))

