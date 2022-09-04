library(dplyr)

require(stringi)

dat <- tibble::tribble(
  ~id, ~ssn, ~phone,
  1L, 123803838L, 5555555555,
  2L, 324555423L, 8133055022,
  3L, 525307008L, 1234567777,
  4L, 726058593L, 9999999999,
  5L, 926810178L, 9399399393
) %>%
  mutate(
    ssn_format = stri_replace_all_regex(
      ssn,
      "^(\\d{3})(\\d{2})(\\d{4})$",
      "$1-$2-$3"
    ),
    phone_format = stri_replace_all_regex(
      phone,
      "^(\\d{3})(\\d{3})(\\d{4})$",
      "($1) $2 $3"
    )
  )

##%######################################################%##
#                                                          #
####   In the regex pattern ( ) define a match group.   ####
####            \\d matches a single digit.             ####
####           {3} is the number of preceding           ####
####        matches required. (\\d{3}) creates a        ####
####           match group with three digits.           ####
#### ^ indicates the start of the string, $ the end of  ####
#### the string. You can then access the matches in the ####
####           replacement using $1, $2, $...           ####
####           where the number refers to the           ####
####              match group in the regex              ####
####       pattern. The replacement is a string,        ####
####              so you can add brackets,              ####
####      dashes and spaces as required. (edited)       ####
#                                                          #
##%######################################################%##

dat %>% 
mutate(
  ssn_format = stri_replace_all_regex(
    ssn,
    "^(\\d{3})(\\d{2})(\\d{4})$",
    "$1-$2-$3"
  ),
  phone_format = stri_replace_all_regex(
    phone,
    "^(\\d{1}{2})(\\-?)(d{2})(\\-?)(\\d{2})$",
    ""
  )
)

