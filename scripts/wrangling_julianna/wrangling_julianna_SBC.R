#### Cleaning SBC biomass data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.50.17 
## on December 3, 2024
## dataset published on 2024-09-13

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: January 5th, 2024

# Purpose:
## Clean the SBC data and get it into the correct format

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
#             Load data ----
## -------------------------------------------- ##

sbc_1 <- read_csv(here("../data/SBC_all_spp_biomass_transect_20240823.csv"))
updated_taxonomy <- read_csv(here("../taxa_tables/SBC_taxa_common_annotated.csv"))

# Notes: these are from permanent transects at 11 sites
# BULL, CARP, NAPL started in 2000, others in 2001
# Two Santa Cruz sites began in summer 2004
# 2-8 transects/site; some transects added later
# transect = 40 m x 2 m fixed plot
# transects 3, 5, 6, 7, 8 at IVEE were added in 2011
# Surveyed annually in the summer
# -99999 means the value was not recorded or not available 
# IMPORTANT: understory species whose abundance is measured as density 
# (i.e., the kelps Pterygophora californica and Laminaria farlowii, and the 
# fucoid, Stephanocystis osmundaceae) were not collected prior to 2008
# The values for them prior to 2008 are estimates
# Also: Size data for invertebrates were not collected prior to 2008
# Values before are estimates
# Reef fish are those 2 m from the benthos
# Note: the accuracy of sampling fish may vary with water clarity and data 
# collected during sampling events when horizontal visibility was < 2 m should be used with caution.

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

# Make taxa table
# sbc_1 %>% 
#   group_by(SCIENTIFIC_NAME, COMMON_NAME, TAXON_KINGDOM, TAXON_PHYLUM, 
#            TAXON_CLASS, TAXON_ORDER, TAXON_FAMILY, TAXON_GENUS, GROUP,
#            MOBILITY) %>% 
#   summarize(N = n()) %>% 
#   select(-N) %>% 
#   mutate(SCIENTIFIC_NAME = as.character(SCIENTIFIC_NAME)) %>% 
#   arrange(SCIENTIFIC_NAME) %>% 
#   write_csv(here("../taxa_tables/SBC_taxa_common.csv"))

## CHANGES TO TAXONOMY
# "Agalophenia spp." to "Aglaophenia spp."
# Crisia occidentalis to Filicrisia franciscana
# Dodecaceria fewkesi to Dodecaceria pacifica
# Haemulon californiensis to Brachygenys californiensis
# See taxa table for other changes

## -------------------------------------------- ##
#             Data wrangling ----
## -------------------------------------------- ##
sbc_1 %>% 
  # just want post 2008 data
  filter(YEAR > 2007) %>% 
  # get biomass data - want shell-free dry mass if values are not NA
  # otherwise want dry mass
  mutate(biomass = case_when(SFDM >= 0 ~ SFDM,
                             TRUE ~ DRY_GM2),
         biomass_metric = case_when(SFDM >= 0 ~ "SFDM",
                                    TRUE ~ "DRY_GM2"
         )) %>% 
  # sum biomass in case it isn't
  group_by(YEAR, MONTH, DATE, SITE, TRANSECT, SP_CODE, SCIENTIFIC_NAME, COMMON_NAME) %>% 
  summarize(biomass = sum(biomass)) %>% # FYI I checked it is already summarized this is overkill
  ungroup() %>% 
  # join with taxa table
  full_join(updated_taxonomy) -> sbc_2


## -------------------------------------------- ##
#             Format data ----
## -------------------------------------------- ##

sbc_2 %>% 
  mutate(plot = SITE) %>% 
  mutate(site = "sbc") %>% 
  mutate(guild = str_to_lower(taxa_type)) %>% 
  mutate(taxa_type = feeding_type) %>% 
  mutate(ecosystem = "aquatic") %>% 
  mutate(habitat_broad = "kelp_forest") %>% 
  mutate(biome = "temperate") %>% 
  mutate(herbivore = NA) %>% 
  mutate(habitat_fine = NA) %>% 
  mutate(date = as.Date(DATE)) %>% 
  rename(year = YEAR) %>% 
  mutate(month = MONTH, 
         day = day(date)) %>% 
  mutate(subplot = TRANSECT) %>% 
  mutate(unique_ID = paste0(site, "_", plot)) %>% 
  mutate(unit_abundance = "g") %>% 
  mutate(scale_abundance = "2x40m") %>% 
  mutate(taxon_name = SCIENTIFIC_NAME_updated) %>% 
  mutate(abundance = biomass) %>% 
  # add a column saying we're confident in all the spp taxonomies
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, taxon_resolution, abundance, id_confidence) -> sbc_3

## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
range(as.numeric(sbc_3$year))

# number of taxa
sbc_3 %>% 
  group_by(guild) %>% 
  summarize(unique(taxon_name)) %>% 
  group_by(guild) %>% 
  summarize(count = n()) 

# And for sites/transect:
sbc_3 %>% 
  group_by(plot, subplot, year) %>% 
  # arbitrary summary thing here to get 1 value per entry
  summarize(total = sum(abundance)) %>% 
  group_by(plot, year) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = plot,
              values_from = count
              ) -> transect_reps
  

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(sbc_3, here("../cleaned_data/sbc_all.csv"))
write_csv(transect_reps, here("../cleaned_data/sbc_transect_reps.csv"))

  



