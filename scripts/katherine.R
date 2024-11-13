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
# CONSUMER 
# loading data
kbs_consum_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-kbs.23.30&entityid=8d33fa9169147f266d20bdcd09a07820"
kbs_consumer_raw <- read.csv(file = kbs_consum_url)

kbs_consumer <- kbs_consumer_raw %>%
  filter(Treatment == "T7") %>% # filtering out non-early successional treatments - only want T7 treatments
  mutate(Sample_Date = as_datetime(Sample_Date)) %>% # converting sampling date to datetime
  mutate(month = month(Sample_Date)) %>% # adding month as a column - already ahve year as a column
  filter(Adults != 0) %>% # removing rows that have "0" for counts of insects
  mutate(site = "KBS", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "insect", 
         unit_abundance = "count", 
         scale_abundance = "1 trap") %>%
  mutate(plot = Replicate, subplot = Station, abundance = Adults, species = Species, year = Year, # renaming columns
         Replicate = NULL, Station = NULL, Adults = NULL, Species = NULL, Year = NULL) %>% # removing old columns
  mutate(abundance = as.numeric(abundance)) %>%
  mutate(year = as.double(year)) %>%
  mutate(unique_ID = paste(site, plot, subplot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))


# SPECIES NAMES NOTES 
### 1. should we remove "other" from analysis - "other" is other insects on traps that were not IDed to species
### 2. this dataset contains counts of 14 species of Coccinellidae, 1 species of Chrysopidae, and 1 species of Lampyridae - do we want to remove others as they were not the focus? 


# PRODUCER
kbs_produce_url <- "https://lter.kbs.msu.edu/datatables/291.csv"
kbs_producer_raw <- read.csv(file = kbs_produce_url, skip = 26) # skipping metadata rows at top of file

kbs_producer <- kbs_producer_raw %>%
  filter(Treatment == "T7") %>% # filtering out non-early successional treatments - only want T7 treatments
  mutate(Date = as_datetime(Date)) %>% # converting sampling date to datetime
  mutate(month = month(Date)) %>% # adding month as a column - already ahve year as a column
  mutate(site = "KBS", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "plant", 
         unit_abundance = "g/m2", 
         scale_abundance = "1 m2") %>%
  mutate(Replicate = str_sub(Replicate, -1), # removing "R" in front of rep # to be consistent with insect data
         Station = str_sub(Station, -1)) %>% # removing "S" in front of station # to be consistent with insect data
  mutate(plot = Replicate, subplot = Station, abundance = Biomass_g, species = Species, year = Year, # renaming columns
         Replicate = NULL, Station = NULL, Biomass_g = NULL, Species = NULL, Year = NULL) %>% # removing old columns
  mutate(abundance = as.numeric(abundance)) %>%
  mutate(year = as.double(year)) %>%
  mutate(unique_ID = paste(site, plot, subplot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

# Species data is a mess! need to talk about how to handle. lots of IDs to genus
# may want to compare to past synthesis groups that have worked with this data
kbs_producer <- kbs_producer %>% # removing unknown - still have some IDed to genus or as "dicot" or "monocot"
  filter(!species %in% c("another unknown dicot", "unknown Asteraceae", "unknown Brassicaceae", "unknown Sedge",
                         "Unknown dicot", "Unknown grass", "Unknown monocot", "Unkown Fabaceae", "UnSorted",
                         "Composite Basal", "Composite sp.", "Dicots", "Monocots", "Unknown Orchidaceae", "Composite Basal Leaves")) #%>%
 # mutate(genus = word(species, 1, sep=" ")) %>% # getting rid of authorities for scientific names, subsetting first word of name as genus
  #mutate(species = word(species, 2, sep=" ")) %>% # subsetting second word of species name
 # mutate(species = if_else(is.na(species), "sp.", species)) %>% # converting "NA" to "sp." - only "dicot" and "monocot" were one word and so have "NA" as second word
 # mutate(species = paste(genus, species), genus = NULL) # recombining genus and species, getting rid of genus column


## SUMMARIZING AND JOINING
kbs_consumer_summarized <- kbs_consumer %>%
  filter(year >= 1991) %>%
  group_by(unique_ID, year) %>%
  summarise(abundance_consumer = sum(abundance), diversity_consumer = n_distinct(unique(species)))

kbs_producer_summarized <- kbs_producer %>%
  filter(year <= 2019) %>%
  group_by(unique_ID, year) %>%
  summarise(abundance_producer = sum(abundance), diversity_producer = n_distinct(unique(species)))

kbs_summarized <- kbs_consumer_summarized %>%
  left_join(kbs_producer_summarized, by = c("unique_ID" = "unique_ID",
                                            "year" = "year"))


