#### Cleaning AIMS Benthic Community algae/coral data ----

## Data downloaded from: xxx (get from Noam) 
## on xxx
## dataset published on xxx

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Pooja Panwar
#### Last Updated: May 4th, 2025

# Purpose:
## Clean the Australian Institue of marine Science (AIMS) dataset (benthic and Fish)

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
#             Load benthic community data
# data on Surveys of benthic reef community using underwater digital photography
# More information: https://www.aims.gov.au/sites/default/files/2020-11/AIMS_LTMP_SOP10v2_Benthic-surveys-photography_2020_DOI.pdf
## -------------------------------------------- ##

benthicComm <- read_csv(here("../AIMS/AIMS_raw_data/AIMS_comp2021.csv"))
benthic_taxonomy <- read_csv(here("../AIMS/taxa_tables/AIMS_benthic_taxa_table.csv"))

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##
# taxa information from: "Coral Reef Research. (2011). Corals of the World online. https://apps.aims.gov.au/metadata/view/c4444880-4227-448a-9f47-999bf2ffebe7, accessed 04-May-2025".
benthicComm2 <- benthicComm %>% 
  left_join(benthic_taxonomy %>% select(COMP_2021, guild, taxon_resolution, id_confidence), by = "COMP_2021")

## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##
# look for quadrats with less than 100% cover (know this would be incorrect)
incorrect_quads <- benthicComm %>% 
  group_by(REPORT_YEAR, FULLREEF_ID, SITE_NO, TRANSECT_NO) %>% 
  summarize(Total_cover = as.numeric(sum(COVER))) %>% 
  filter(Total_cover != 100) %>% 
  # create a quadrat ID, so we can exclude these
  mutate(Quad_ID = paste0(REPORT_YEAR, "_", FULLREEF_ID, "_", SITE_NO, "_", TRANSECT_NO)) 

dim(incorrect_quads) # R reports that 6256 quadrats do not meet the criteria but upon investigation,
#they all add upto 100. 
# ACTION: Commenting the code to exclude incorrect quads

# Exclude incorrect quads
# benthicComm2 <- benthicComm %>% 
#   # create a quadrat ID, so we can exclude these
#   mutate(Quad_ID = paste0(REPORT_YEAR, "_", FULLREEF_ID, "_", SITE_NO, "_", TRANSECT_NO)) %>% 
#   # remove incorrect quadrats
#   filter(!(Quad_ID %in% incorrect_quads$Quad_ID))

## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##
# now format in the style we need
AIMS_producer <- benthicComm2 %>% 
  filter(guild %in% c("macroalgae", "turf", "seagrass", "algae") & id_confidence == 1) %>%
  mutate(Date = as.Date(SAMPLE_DATE, format = "%d-%b-%Y %H:%M:%S")) %>% 
  mutate(site = "aims",
        taxa_type = "producer",
        ecosystem = "aquatic",
        habitat_broad = "coral_reef",
        biome = "tropical",
        guild = "macroalgae",
        herbivore = "no", 
        habitat_fine = SHELF,
        year = REPORT_YEAR,
        month = month(Date), 
        day = day(Date),
        plot = paste0(FULLREEF_ID, "_", SHELF, "_", SITE_NO),
        subplot = TRANSECT_NO,
        unique_ID = paste0(site, "_", SHELF, "_", plot),
        unit_abundance = "percent",
        scale_abundance = "50m2",
        taxon_name = COMP_2021,
        abundance = COVER,
        id_confidence = 1) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)

## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
AIMS_producer %>% 
  select(year) %>% 
  range()

# number of producer taxa
AIMS_producer %>% 
  filter(taxa_type == "producer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(AIMS_producer, here("../AIMS/AIMS_clean_data/aims_algae.csv"))

## -------------------------------------------- ##
#             Load fish community data
# data on Surveys of reef fish community using transect surveys
# More information:https://www.aims.gov.au/sites/default/files/AIMS%20LTMP%20Fish%20Census%20SOP_3_Ed2_Oct%202018.pdf
## -------------------------------------------- ##

FishComm <- read_csv(here("../AIMS/AIMS_raw_data/AIMS_fish.csv"))
Fish_taxonomy <- read_csv(here("../AIMS/taxa_tables/AIMS_FISH_TAXA_TABLE.csv"))

### From AIMS IM (Mike): please be aware that until 2021, we surveyed a restricted set of ~240 spp. 
### After 2021 the list was expanded to include all non-cryptic reef associated fishes. 
### Any temporal analyses will need to be restricted to the original spp list (attached) – 
### obviously particularly relevant to examinations of diversity through time
### Pooja: No such difference exist when I looked at data. Have asked Mike.

#get taxa table from data. Commeneting because it only needs to be run once to get the table.
# FishComm2 <- FishComm %>%
#   group_by(unique(FISH_CODE)) %>%
#   mutate(FISH_CODE, FAMILY, GENUS, SPECIES)
# 
# fc <- FishComm %>%
#   group_by(FISH_CODE,FAMILY, GENUS, SPECIES, TAXA) %>%
#   summarise(unique_values = n_distinct(ABUNDANCE))
# 
# write.csv(fc, "FISH_TAXA_TABLE3.csv")
## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##
# update taxonomy, if needed
FishComm2 <- FishComm %>% 
  left_join(Fish_taxonomy %>% select(FISH_CODE, id_confidence, transect_type), by = "FISH_CODE") 

## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##
# now format in the style we need
AIMS_FISH <- FishComm2 %>% 
  filter(id_confidence == 1) %>% ##filtering out taxa observations that are not to required taxonomic resolution
  mutate(site = "aims",
         taxa_type = "consumer",
         ecosystem = "aquatic",
         habitat_broad = "coral_reef",
         biome = "tropical",
         guild = "fish",
         herbivore = "maybe", 
         habitat_fine = SHELF,
         year = REPORT_YEAR,
         month = "NA", # Not in Data month(Date)
         day = "NA", # Not in Data day(Date)
         plot = paste0(FULLREEF_ID, "_", SHELF, "_", SITE_NO),
         subplot = TRANSECT_NO,
         unique_ID = paste0(site, "_", SHELF, "_", plot),
         unit_abundance = "count",
         scale_abundance = case_when(
           transect_type == "50m_by_5m" ~ "250m2",
           transect_type == "50m_by_1m" ~ "50m2",
           transect_type == "" ~ "TBD"),
         taxon_name = TAXA,
         abundance = ABUNDANCE,
         id_confidence = id_confidence) %>%   # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, abundance, id_confidence)

## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
AIMS_FISH %>% 
  select(year) %>% 
  range()

# number of consumer taxa
AIMS_FISH %>% 
  filter(taxa_type == "consumer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(AIMS_FISH, here("../AIMS/AIMS_clean_data/aims_fish.csv"))
