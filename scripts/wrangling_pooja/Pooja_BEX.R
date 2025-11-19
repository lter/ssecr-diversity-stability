#### Cleaning Biodiversity Exploratory Data for producers data ----

## Data downloaded from: Arthropod: https://www.bexis.uni-jena.de/ddm/data/Showdata/21969
## and Plants: https://www.bexis.uni-jena.de/ddm/data/Showdata/24247
## on Arthropods: 21969 and plants: 24247
## dataset published/last modified on: Arthropod: 12/27/2024 and Producer: 2020-01-15

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Pooja Panwar
#### Last Updated: Jul 24th, 2025

# Purpose:
## Clean the Biodiversity Exploratories Datatset from Germany (plants and arthropods)

## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##

# Clear environment for maximum reproducibility
rm(list=ls())

# load librarian (package for installing/loading packages)
if (!require("librarian")) install.packages("librarian")

# Load other necessary libraries
librarian::shelf(here, # relative file paths
                 tidyverse # data wrangling
)

## -------------------------------------------- ##
#             Site cleanup cleaning ----
## including sites within 3 areas where no fertilization was used through the study period
## using land use intensity information from Data:31661 https://www.bexis.uni-jena.de/lui/LUICalculation/index
## this will reduce nsites in 3 areas 
## -------------------------------------------- ##

LUI_info <- read.csv(here("../BEX/BEX_raw_data/LUI_31661.csv"))

# Assuming your data frame is called df
plot_summary <- LUI_info %>%
  group_by(EP_PlotID) %>%
  summarise(Total_Fertilization_All_Years = sum(TotalFertilization, na.rm = TRUE)) %>% 
  filter(Total_Fertilization_All_Years == 0)

#Add padding to plot ID so that they are similar to ones in consumer and producer data
sites_noFert <- sub("^([A-Za-z]{3})(\\d)$", "\\10\\2", plot_summary$EP_PlotID)

## -------------------------------------------- ##
#             Load plant community data
# data on plant community survey using percent cover estimates in 4x4 m subplot
# More information: https://www.bexis.uni-jena.de/ddm/data/Showdata/21969
## -------------------------------------------- ##

plant_comm <- read_csv(here("../BEX/BEX_raw_data/Producer/24247_2_data.csv"))
plant_taxonomy <- read_csv(here("../BEX/taxa_tables/BEX_producer_taxa_table.csv"))

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##
# taxa information from: https://www.bexis.uni-jena.de/ddm/data/Showdata/31122. Last modified 6/3/2025
plant_comm2 <- plant_comm %>% 
  left_join(plant_taxonomy %>% select(SpeciesID, Type, Taxa_Resolution, id_confidence), by = c("Species" = "SpeciesID"))

## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##


## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##
# now format in the style we need
BEX_AE_producer <- plant_comm2 %>% 
  filter(id_confidence == 1 & Useful_EP_PlotID%in%sites_noFert) %>% 
  filter(substr(Useful_EP_PlotID, 1, 3) == "AEG") %>%
  #mutate(Date = as.Date(SAMPLE_DATE, format = "%d-%b-%Y %H:%M:%S")) %>% 
  mutate(site = "BEX_AE",
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         biome = "temperate",
         guild = "plant",
         herbivore = "no", 
         habitat_fine = NA,
         year = Year,
         month = NA, 
         day = NA,
         plot = Useful_EP_PlotID,
         subplot = NA,
         unique_ID = paste0(site, "_", plot),
         unit_abundance = "percent",
         scale_abundance = "16m2",
         taxon_name = Species,
         abundance = Cover,
         id_confidence = 1) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)

BEX_HE_producer <- plant_comm2 %>% 
  filter(id_confidence == 1 & Useful_EP_PlotID%in%sites_noFert) %>% 
  filter(substr(Useful_EP_PlotID, 1, 3) == "HEG") %>%
  #mutate(Date = as.Date(SAMPLE_DATE, format = "%d-%b-%Y %H:%M:%S")) %>% 
  mutate(site = "BEX_HE",
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         biome = "temperate",
         guild = "plant",
         herbivore = "no", 
         habitat_fine = NA,
         year = Year,
         month = NA, 
         day = NA,
         plot = Useful_EP_PlotID,
         subplot = NA,
         unique_ID = paste0(site, "_", plot),
         unit_abundance = "percent",
         scale_abundance = "16m2",
         taxon_name = Species,
         abundance = Cover,
         id_confidence = 1) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)

BEX_SE_producer <- plant_comm2 %>% 
  filter(id_confidence == 1 & Useful_EP_PlotID%in%sites_noFert) %>% 
  filter(substr(Useful_EP_PlotID, 1, 3) == "SEG") %>%
  #mutate(Date = as.Date(SAMPLE_DATE, format = "%d-%b-%Y %H:%M:%S")) %>% 
  mutate(site = "BEX_SE",
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         biome = "temperate",
         guild = "plant",
         herbivore = "no", 
         habitat_fine = NA,
         year = Year,
         month = NA, 
         day = NA,
         plot = Useful_EP_PlotID,
         subplot = NA,
         unique_ID = paste0(site, "_", plot),
         unit_abundance = "percent",
         scale_abundance = "16m2",
         taxon_name = Species,
         abundance = Cover,
         id_confidence = 1) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)
## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
BEX_AE_producer %>% 
  select(year) %>% 
  range()

# number of producer taxa
BEX_AE_producer %>% 
  filter(taxa_type == "producer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

# year range
BEX_HE_producer %>% 
  select(year) %>% 
  range()

# number of producer taxa
BEX_HE_producer %>% 
  filter(taxa_type == "producer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

# year range
BEX_SE_producer %>% 
  select(year) %>% 
  range()

# number of producer taxa
BEX_SE_producer %>% 
  filter(taxa_type == "producer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(BEX_AE_producer, here("../BEX/BEX_clean_data/BEX_AE_producer.csv"))
write_csv(BEX_HE_producer, here("../BEX/BEX_clean_data/BEX_HE_producer.csv"))
write_csv(BEX_SE_producer, here("../BEX/BEX_clean_data/BEX_SE_producer.csv"))

## -------------------------------------------- ##
#             Load Arthropod community data
# data on arthropod survey in grassland plots community using sweepnetting surveys 
# More information: Taxonomy: https://www.bexis.uni-jena.de/ddm/data/Showdata/31122 and
#                   Community: https://www.bexis.uni-jena.de/ddm/data/Showdata/21969
## -------------------------------------------- ##

arthropod_comm <- read_csv(here("../BEX/BEX_raw_data/Arthropod/21969_13_data.csv"))
arthropod_taxonomy <- read_csv(here("../BEX/taxa_tables/BEX_consumer_taxa_table_new.csv"))

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##
# update taxonomy, if needed
arthropod_comm2 <- arthropod_comm %>% 
  left_join(arthropod_taxonomy %>% select(Species, Taxa_Resolution, Mean_BodySize, Feeding_guild, id_confidence), by = "Species")
#feeding_guilds: c-carnivore, d - detritivore, f - fungivore, h-herbivore, m- mycetophagous, o-omnivore

## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##
# now format in the style we need
BEX_AE_consumer <- arthropod_comm2 %>%  #Add to filter if we want only herbivores <& Feeding_guild == "h">
  filter(id_confidence == 1 & substr(PlotID, 1, 5) %in% sites_noFert) %>% 
  filter(substr(PlotID, 1, 3) == "AEG") %>% ##filtering out taxa observations that are not to required taxonomic resolution.
  mutate(site = "BEX_AE",
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         biome = "temperate",
         guild = "arthropod",
         herbivore = case_when(
           Feeding_guild == "h" ~ "herbivore",
           Feeding_guild == "c" ~ "carnivore",
           Feeding_guild == "d" ~ "detritivore",
           Feeding_guild == "f" ~ "fungivore",
           Feeding_guild == "m" ~ "mycetophagous",
           Feeding_guild == "o" ~ "omnivore",
           TRUE ~ "NA" # Assign a default value for all other feeding guilds
         ), 
         habitat_fine = NA,
         year = CollectionYear,
         month = CollectionMonth, # Not in Data month(Date)
         day = "NA", # Not in Data day(Date)
         plot = substr(TrapID, 1, 5),
         subplot = NA,
         unique_ID = paste0(site, "_", plot),
         unit_abundance = "count",
         scale_abundance = "150m2",
         taxon_name = Species,
         abundance = NumberAdults,
         id_confidence = id_confidence) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)

BEX_HE_consumer <- arthropod_comm2 %>% 
  filter(id_confidence == 1 & substr(PlotID, 1, 5) %in% sites_noFert) %>% 
  filter(substr(PlotID, 1, 3) == "HEG") %>% ##filtering out taxa observations that are not to required taxonomic resolution.
  mutate(site = "BEX_HE",
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         biome = "temperate",
         guild = "arthropod",
         herbivore = case_when(
           Feeding_guild == "h" ~ "herbivore",
           Feeding_guild == "c" ~ "carnivore",
           Feeding_guild == "d" ~ "detritivore",
           Feeding_guild == "f" ~ "fungivore",
           Feeding_guild == "m" ~ "mycetophagous",
           Feeding_guild == "o" ~ "omnivore",
           TRUE ~ "NA" # Assign a default value for all other feeding guilds
         ), 
         habitat_fine = NA,
         year = CollectionYear,
         month = CollectionMonth, # Not in Data month(Date)
         day = "NA", # Not in Data day(Date)
         plot = substr(TrapID, 1, 5),
         subplot = NA,
         unique_ID = paste0(site, "_", plot),
         unit_abundance = "count",
         scale_abundance = "150m2",
         taxon_name = Species,
         abundance = NumberAdults,
         id_confidence = id_confidence) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)

BEX_SE_consumer <- arthropod_comm2 %>% 
  filter(id_confidence == 1 & substr(PlotID, 1, 5) %in% sites_noFert) %>% 
  filter(substr(PlotID, 1, 3) == "SEG") %>% ##filtering out taxa observations that are not to required taxonomic resolution.
  mutate(site = "BEX_SE",
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         biome = "temperate",
         guild = "arthropod",
         herbivore = case_when(
           Feeding_guild == "h" ~ "herbivore",
           Feeding_guild == "c" ~ "carnivore",
           Feeding_guild == "d" ~ "detritivore",
           Feeding_guild == "f" ~ "fungivore",
           Feeding_guild == "m" ~ "mycetophagous",
           Feeding_guild == "o" ~ "omnivore",
           TRUE ~ "NA" # Assign a default value for all other feeding guilds
         ),  
         habitat_fine = NA,
         year = CollectionYear,
         month = CollectionMonth, # Not in Data month(Date)
         day = "NA", # Not in Data day(Date)
         plot = substr(TrapID, 1, 5),
         subplot = NA,
         unique_ID = paste0(site, "_", plot),
         unit_abundance = "count",
         scale_abundance = "150m2",
         taxon_name = Species,
         abundance = NumberAdults,
         id_confidence = id_confidence) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)

## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
BEX_AE_consumer %>% 
  select(year) %>% 
  range()

# number of producer taxa
BEX_AE_consumer %>% 
  filter(taxa_type == "consumer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

# year range
BEX_HE_consumer %>% 
  select(year) %>% 
  range()

# number of producer taxa
BEX_HE_consumer %>% 
  filter(taxa_type == "consumer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

# year range
BEX_SE_consumer %>% 
  select(year) %>% 
  range()

# number of producer taxa
BEX_SE_consumer %>% 
  filter(taxa_type == "consumer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(BEX_AE_consumer, here("../BEX/BEX_clean_data/BEX_AE_consumer.csv"))
write_csv(BEX_HE_consumer, here("../BEX/BEX_clean_data/BEX_HE_consumer.csv"))
write_csv(BEX_SE_consumer, here("../BEX/BEX_clean_data/BEX_SE_consumer.csv"))

