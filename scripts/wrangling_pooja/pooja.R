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
SEV_sppList <- read_excel(SEV_specieslist_URL, sheet = 1, col_names = FALSE)

#defining date format
# attempting to convert SEV_consumer_data$DATE dateTime string to R date structure (date or POSIXct)                                
#tmpDateFormat<-"%Y-%m-%d"

SEV_consumer <- SEV_consumer_data %>%
  filter(AGE == "4" & SITE != "PJ") %>% # filtering only data on adult grasshopppers; removing "PJ" site as corrspinding producer data not available
  mutate(Sample_Date = as_datetime(DATE)) %>% # converting sampling date to datetime
  mutate(month = month(DATE), day = day(DATE)) %>% # adding month as a column - already ahve year as a column
  filter(CNT != 0) %>% # removing rows that have "0" for counts of insects
  mutate(site = "SEV", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland", #adding habitat_fine below because 3 types of habitats
         biome = "desert",
         guild = "insect", 
         herbivore = "yes",
         unit_abundance = "count", 
         scale_abundance = "1_transect_100m2",
         genus = "genus") %>%                       #will need to get genus info from species metadata. Hardcoding with genus for now
  mutate(habitat_fine = SITE, plot = WEB, subplot = TRN, abundance = CNT, species = SPECIES, year = YEAR) %>% # renaming columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild","herbivore",
                  "year", "month","day", "plot", "subplot", "unique_ID","unit_abundance", "scale_abundance",
                  "genus", "species", "abundance" 
                  ))
#write csv file for later use
#write.csv(SEV_consumer, "C:/Users/f005c0j/OneDrive - Dartmouth College/Dartmouth/Courses/SSECR/Diversity_Stability/ssecr-diversity-stability/data/SEV/sev_consumer.csv")


# PRODUCER
SEV_producer_URL <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.4.202312&entityid=d954c3b638b19e2ccac3b45f2b47229a"
SEV_producer_data <- read.csv(file = SEV_producer_URL)


# Converting transect data to cover data
remove_species <- c("SOIL", "LITT", "ROCK","UNK","UNK1","", "NA",
                    "POACE", "FORB", "SHRUB", "CACTA", "BSHRUB", "BGRASS") #check  what to do with POACE, FORB, SHRUB, CACTA, BSHRUB, BGRASS; removing for now
SEV_producer_derived <- SEV_producer_data %>%
  filter(!(Species %in% remove_species) & #removing species that are not plants
           !is.na(Species) &  #removing any rows with Species=NA                           
           !is.na(Start) & #removing any rows with Start=NA  
           !is.na(Stop) & #removing any rows with Stop=NA  
           (Stop < 400) & (Start > 0)) %>% #removing any row with stop point greater than 400 and start less than 0  & (Start < 0)
  mutate(cover = abs(abs(Stop) - abs(Start))) %>%               # using absolute values because sometimes start and stop values are in opposite order and one stop value is negative. Data entry issue
  group_by(Year, Location, Transect, Period, Species) %>%
  summarize(total_cover = sum(cover), percent_cover = sum(cover)/400*100, .groups = "drop") 
#BOGR2 (in DW in 2017 in period 3) has cover value more than 100%. I think there is a typo in one of the BOGR2 start stop value.

SEV_producer <- SEV_producer_derived %>%
  #filter(Location == "FP") %>% # May need vegetation data from some sites one location only. There are two locations
  mutate(site = "SEV", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         biome = "desert",
         guild = "plant",
         herbivore = "no",
         unit_abundance = "percent_cover", 
         scale_abundance = "400m_transect",
         day = "NA",
         genus = "genus") %>%  ### will need to extract this from USDA
  mutate(habitat_fine = Location, plot = Location, subplot = Transect, abundance = percent_cover, species = Species, year = Year, month = Period) %>% # renaming columns
  mutate(unique_ID = paste(site, plot, subplot, year, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", "herbivore", 
                  "year", "month", "day", "plot", "subplot", "unique_ID", "unit_abundance", "scale_abundance",
                  "genus", "species", "abundance" ))

#write csv file for later use
#write.csv(SEV_producer, "C:/Users/f005c0j/OneDrive - Dartmouth College/Dartmouth/Courses/SSECR/Diversity_Stability/ssecr-diversity-stability/data/SEV/sev_producer.csv")
