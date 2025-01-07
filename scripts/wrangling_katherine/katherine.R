#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Katherine Hulting
#### Last Updated: November 18th, 2024
# Load `librarian` package
library(librarian)
# Install missing packages and load needed libraries
shelf(tidyverse, summarytools, googlesheets4, googledrive)



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

kbs_consumer <- kbs_consumer %>%
  filter(species != "Other") # removing unidentified individuals n=45

# GROUPING NOTES: traps are sampled weekly throughout the growing season -- it may make the most sense to sum rather than average?
kbs_consumer %>%
  count(species)
  

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

kbs_producer %>%
  count(species)

###### KNZ LTER harmonization ######
# producer
knz_produce_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.69.23&entityid=63768b48f41e790a40e7fa4f9267c3a2"
knz_producer_raw <- read.csv(file = knz_produce_url)


knz_producer <- knz_producer_raw %>%
  filter(SoilType == "f") %>% # only keeping upland (florance) soil types to match consumer
  mutate(site = "KNZ", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "plant", 
         unit_abundance = "cover class", 
         scale_abundance = "10 m2") %>%
  mutate(plot = WaterShed, subplot = Transect, sub_subplot = Plot, 
         abundance = Cover, species = paste(AB_genus, AB_species, sep = " "), year = RecYear,
         month = RecMonth, day = RecDay) %>% # renaming columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "sub_subplot", "year", "month", "day", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

# changing cover classes to midpoint of cover class - according to Konza sampling manual https://lter.konza.ksu.edu/sites/default/files/MM_0.pdf
knz_producer$abundance <- as.factor(knz_producer$abundance)
knz_producer$abundance <- fct_recode(knz_producer$abundance, "0.5" = "1", "3.0" = "2", "15.0" = "3",
                                     "37.5" = "4", "62.5" = "5", "85.0" = "6", "97.5" = "7")
knz_producer$abundance <- as.numeric(as.character(knz_producer$abundance))

# species names
knz_producer <- knz_producer %>% # do we want to filter all of these??
  filter(!species %in% c("annual forb", "carex spp.", "cyperu spp.", "euphor spp.", "symphy spp."))

write.csv(knz_producer,"knz_producer.csv", row.names = FALSE) # exporting csv




# consumer
knz_consume_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.29.22&entityid=3fb352e2478f776517f7e880fe31b808"
knz_consumer_raw <- read.csv(file = knz_consume_url)


knz_consumer <- knz_consumer_raw %>%
  filter(SOILTYPE == "fl") %>% # only keeping upland (florance) soil types - others were not sampled long term
  mutate(site = "KNZ", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "insect", 
         unit_abundance = "count", 
         scale_abundance = "200 sweeps") %>%
  mutate(plot = WATERSHED, subplot = REPSITE, 
       abundance = TOTAL, species = SPCODE, year = RECYEAR,
       month = RECMONTH, day = RECDAY) %>% # renaming columns
  mutate(plot = tolower(plot)) %>% # lowercasing plot to be consistent with producer
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

knz_consumer$abundance <- str_replace(knz_consumer$abundance, "1 01", "101") # fixing abundance typo
knz_consumer <- knz_consumer %>%
  filter(!is.na(species)) %>% # removing NAs for species
  filter(!abundance %in% c("", "0")) %>% # removing 0 and NA for abundance
  mutate(species = as.character(species)) %>% # converting species codes to character
  mutate(abundance = as.numeric(abundance)) # converting abundance to numeric


write.csv(knz_consumer,"knz_consumer.csv", row.names = FALSE) # exporting csv




###### CDR LTER harmonization ######
# producer 
cdr_producer_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-cdr.273.11&entityid=27ddb5d8aebe24db99caa3933e9bc8e2"
cdr_producer_raw <- read.csv(file = cdr_producer_url)


cdr_producer <- cdr_producer_raw %>%
  mutate(site = "CDR", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "plant", 
         unit_abundance = "g/m2", 
         scale_abundance = "m2") %>%
  mutate(plot = Plot, subplot = Strip, 
         abundance = `Biomass..g.m2.`, species = Species, year = Year,
         month = Month) %>% # renaming columns
  separate(Date, c("month_discard", "day", "year_discard"), sep = "/") %>% # separating date components in order to get "day" -- not using month/year from this, discarding those extra columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

cdr_producer$species <- str_replace(cdr_producer$species, "Festuca Sp.", "Festuca sp.") # fixing typo
cdr_producer$species <- str_replace(cdr_producer$species, "cyperus sp", "Cyperus sp.") # fixing typo
cdr_producer$species <- str_replace(cdr_producer$species, "leptoloma sp.", "Leptoloma sp.") # fixing typo

cdr_producer <- cdr_producer %>%
  filter(!species %in% c("32 Species Weeds", "Fungi", "Grasses", "Green matter", "Green matter (alive)", 
                         "Miscellaneous Forb", "Miscellaneous forb", "Miscellaneous grass", "Miscellaneous grasses",
                         "Miscellaneous herbs", "Miscellaneous litter", "Miscellaneous sedges", "Miscellaneous sp.",
                         "Mosses", "Mosses & lichens", "Real Weeds", "Unsorted Biomass", "Unsorted biomass", "Weeds",
                         "era sp", "erg sp", "miscellaneous seedhead", "unknown forb", "unknown grass"))


# consumer
cdr_consumer_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-cdr.418.8&entityid=aae64949e1ef41513062633cfb6da7d5"
cdr_consumer_raw <- read.delim(file = cdr_consumer_url)


cdr_consumer <- cdr_consumer_raw %>%
  mutate(site = "CDR", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "insect", 
         unit_abundance = "count", 
         scale_abundance = "25 sweeps") %>%
  mutate(plot = Plot, subplot = NA, 
         abundance = Count, species = paste(Genus, Specific.epithet, sep = " "), year = Year,
         month = Month, day = NA) %>% # renaming columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

cdr_consumer_raw %>%
  count(Order, Family.subfamily., Genus, Specific.epithet, Further.ID)
cdr_consumer_raw %>%
  filter(Order == "Homoptera") %>%
  count(Genus)

cdr_consumer_raw$Further.ID <- str_trim(cdr_consumer_raw$Further.ID, side = c("both")) 
cdr_consumer_raw$Further.ID <- str_replace_all(cdr_consumer_raw$Further.ID, "^na$", "") 

cdr_consumer_trial_taxonomy <- cdr_consumer_raw %>%
  mutate(ID = paste(Genus, Specific.epithet, Further.ID, sep = " ")) %>%
  count(Order, Family.subfamily., ID)


## Loading info on taxonomy resolutions from google drive
googledrive::drive_auth() # Authenticate Google drive 
cdr_consumer_taxonomy <- read_sheet("https://docs.google.com/spreadsheets/d/14KEoAB88NcpEHVWZ0-VvB8hEXTNMWwCjf0kWB311fJg/edit?gid=45649463#gid=45649463")

cdr_consumer_trial_taxonomy$ID <- str_trim(cdr_consumer_trial_taxonomy$ID, side = c("both")) 
cdr_cleaned_species$PreferredName <- str_trim(cdr_cleaned_species$PreferredName, side = c("both")) 

cdr_consumer_trial_taxonomy %>%
  full_join(cdr_cleaned_species, by = c("ID" = "PreferredName")) %>%
  filter(!is.na(n.x)) %>%
  View()



cdr_cleaned_species <- cdr_consumer_taxonomy %>%
  count(Order, Family, PreferredName, PreferredCode)









