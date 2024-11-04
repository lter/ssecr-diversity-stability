#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Katherine Hulting
#### Last Updated: November 4th, 2024
# Load `librarian` package
library(librarian)
# Install missing packages and load needed libraries
shelf(tidyverse)



#### KBS LTER harmonization ####
# Define URL as an object
kbs_consum_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-kbs.23.30&entityid=8d33fa9169147f266d20bdcd09a07820"
# Read it into R
kbs_consumer_raw <- read.csv(file = kbs_consum_url)

kbs_consumer <- kbs_consumer_raw %>%
  filter(Treatment == "T7") %>% # filtering out non-early successional treatments - only want T7 treatments
  mutate(Sample_Date = as_datetime(Sample_Date)) %>% # converting sampling date to datetime
  mutate(month = month(Sample_Date)) %>% # adding month as a column - already ahve year as a column
  filter(Adults != 0) %>% # removing rows that have "0" for counts of insects
  mutate(lter = "KBS", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat = "grassland",
         biome = "temperate",
         taxa = "insect", 
         unit_product = "count", 
         scale_product = "1 trap") %>%
  mutate(plot = Replicate, subplot = Station, productivity = Adults, species = Species, year = Year, # renaming columns
         Replicate = NULL, Station = NULL, Adults = NULL, Species = NULL, Year = NULL) %>% # removing old columns
  mutate(unique_ID = paste(lter, plot, subplot, year, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("lter", "taxa_type", "ecosystem", "habitat", "biome", "taxa", 
                  "plot", "subplot", "year", "month", "unique_ID", "species", "productivity", 
                  "unit_product", "scale_product"))

# SPECIES NAMES NOTES 
### 1. should we remove "other" from analysis - "other" is other insects on traps that were not IDed to species
### 2. this dataset contains counts of 14 species of Coccinellidae, 1 species of Chrysopidae, and 1 species of Lampyridae - do we want to remove others as they were not the focus? 








