#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Pooja Panwar
#### Last Updated: November 5th, 2024

#Load required packages
library(librarian)
librarian::shelf(tidyverse, readxl, dplyr)

#### SEV LTER harmonization ####
# CONSUMER 
# loading data
SEV_consumer_URL <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.106.214970&entityid=60c354ca834ae4359b331638b2a5fe2e"
SEV_consumer_data <- read.csv(SEV_consumer_URL)

SEV_specieslist_URL <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.106.214970&entityid=b627997ec4477026c1b3d6be7614203f"
### species list is an excel file with data on species and metadata in the same sheet. Might need some extra work to extract info from this excel.
#SEV_sppList <- read_excel(SEV_specieslist_URL, sheet = 1, col_names = FALSE)

#defining date format
# attempting to convert SEV_consumer_data$DATE dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"

SEV_consumer <- SEV_consumer_data %>%
  #filter(AGE == "4") %>% # filtering only data on adult grasshopppers May need to filter some sites
  mutate(Sample_Date = as_datetime(DATE)) %>% # converting sampling date to datetime
  mutate(month = month(DATE)) %>% # adding month as a column - already ahve year as a column
  filter(CNT != 0) %>% # removing rows that have "0" for counts of insects
  mutate(lter = "SEV", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat = "grassland",
         biome = "desert",
         taxa = "insect", 
         unit_product = "count", 
         scale_product = "1_transect_50m2") %>%
  mutate(site = SITE, plot = WEB, subplot = TRN, productivity = CNT, species = SPECIES, year = YEAR) %>% # renaming columns
  mutate(unique_ID = paste(lter, site, plot, subplot, year, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("lter", "taxa_type", "ecosystem", "habitat", "biome", "taxa", "site",
                  "plot", "subplot", "year", "month", "unique_ID", "species", "productivity", 
                  "unit_product", "scale_product"))

# PRODUCER
SEV_producer_URL <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.4.202312&entityid=d954c3b638b19e2ccac3b45f2b47229a"
SEV_producer_data <- read.csv(file = SEV_producer_URL)

# Converting transect data to cover data
remove_species <- c("SOIL", "LITT", "ROCK","UKN","UNK1","") #check  what to do with POACE, FORB, SEED general category
SEV_producer_derived <- SEV_producer_data %>%
  filter(!(Species %in% remove_species) & #removing species that are not plants
           !is.na(Species) &  #removing any rows with Species=NA                           
           !is.na(Start) & #removing any rows with Start=NA  
           !is.na(Stop) & #removing any rows with Stop=NA  
           (Stop < 400)) %>% #removing any row with stop point greater than 400
  mutate(cover = abs(abs(Stop) - abs(Start))) %>%               # using absolute values because sometimes start and stop values are in opposite order and one stop value is negative. Data entry issue
  group_by(Year, Location, Transect, Period, Species) %>%
  summarize(total_cover = sum(cover), percent_cover = sum(cover)/400*100, .groups = "drop") 
#BOGR2 (in DW in 2017 in period 3) has cover value more than 100%. I think there is a typo in one of the BOGR2 start stop value.

SEV_producer <- SEV_producer_derived %>%
  #filter(Location == "FP") %>% # May need vegetation data from some sites one location only. There are two locations
  mutate(lter = "SEV", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat = "grassland",
         biome = "desert",
         taxa = "plant", 
         unit_product = "percent_cover", 
         scale_product = "400m_transect") %>%
  mutate(plot = Location, subplot = Transect, productivity = percent_cover, species = Species, year = Year, month = Period) %>% # renaming columns
  mutate(unique_ID = paste(lter, plot, subplot, year, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("lter", "taxa_type", "ecosystem", "habitat", "biome", "taxa", 
                  "plot", "subplot", "year", "month", "unique_ID", "species", "productivity", 
                  "unit_product", "scale_product"))


