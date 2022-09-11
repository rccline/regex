##%######################################################%##
#                                                          #
####    Download files directly to your data folder     ####
#                                                          #
##%######################################################%##


fs::dir_create(here("data"))
download.file(
  url = "https://raw.githubusercontent.com/cosmoduende/r-marvel-vs-dc/main/dataset_shdb/heroesInformation.csv", 
  destfile = here("data/comics.csv")
)