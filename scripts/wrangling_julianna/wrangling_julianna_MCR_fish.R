#### Cleaning MCR fish data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcr&identifier=6&revision=63 
## on November 20, 2024
## dataset published on 2024-09-27

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: December 4th, 2024

# Purpose:
## Clean the Moorea fish dataset

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

fish_1 <- read_csv(here("../data/MCR_fish_abundance_20241204_Hillary.csv")) %>% 
# ^ these are cleaned fish data with updated taxonomy from Hillary FRESH
# off the press (12/4/24) that I think she made for a different working group
  filter(Swath != 2) # I think this is a typo (just one case of this)

updated_taxonomy <- read_csv(here("../taxa_tables/MCR_fish_taxa_annotated.csv"))

# NOTES ON FISH DATA
# 2005 used a different method--start with 2006
# need to deal with the 1m and 5m swaths differently (both x 50 m)
# Need to make decisions about spp not identified to the spp level
# "no fish observed" is an option
# Secondary consumer = "Feed primarily on non-planktonic animals including 
# corals and other benthic invertebrates, but not fishes (may feed on fishes to some extent)"


## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

# Make taxa table
# fish_1 %>% 
#   select(Taxonomy) %>% 
#   unique() %>% 
#   arrange(Taxonomy) %>% 
#   write_csv(here("../taxa_tables/MCR_fish_taxa.csv"))

# how many observations are biomass not available?
fish_1 %>% 
  filter(Biomass <= 0) %>% 
  summarize(sum(Count),
            Num_rows = n()) # 1642 individuals across 232 rows

# list of fishes not identified to species
low_res_fishes <- updated_taxonomy %>% 
  filter(id_confidence == 0) %>%
  select(Taxonomy)

# count up the instances of the fishes not identified to species
fish_1 %>% 
  filter(Taxonomy %in% low_res_fishes$Taxonomy) %>% 
  group_by(Taxonomy) %>% 
  summarize(Sum_count = sum(Count)) # %>% view() # most there ever is is 47--maybe OK to exclude?
# Andy agrees it is fine to exclude these

# total instances of these fish:
fish_1 %>% 
  filter(Taxonomy %in% low_res_fishes$Taxonomy) %>%
  select(Count) %>% 
  sum() # 219

# compared to how many total fish observations:
fish_1 %>% 
  select(Count) %>% 
  sum() # 435,087 -- that's less than a tenth of a percent (0.07%)


# Need to deal with "CF" records
fish_1 %>%
  full_join(updated_taxonomy) %>% 
  select(-Taxonomy) %>% 
  # manually changed "cf" entries in Taxonomy updated to be without cf
  rename(Taxonomy = Taxonomy_updated) -> fish_2

# Andy says - description for every Blennie spp 6, etc. is unique
# Unknown Scaridae does not have a unique description
# They're such small numbers, probably fine to just exclude them
# most conservative thing is to just chuck them > also no way to do biomass
# also FYI biomass is way more stable than abundance
# also thinks it's fine to count the "cf" species as the species they are recorded as
# sometimes these are for spp that are not even described (but reeeeally similar - 
# length/weight probably the same; trophic is also probably the same );
# these differences are more regional 

## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##

# get a list of sites
fish_2 %>% 
  group_by(Year, Date, Location, Site, Habitat, Transect, Swath) %>% 
  summarize(n = n()) %>% # can see n() varies a ton
  select(-n) -> transect_list

# Explore na & unknown trophic designations:
fish_2 %>% 
  filter(Fine_Trophic == "na" |
           Fine_Trophic == "Unknown") %>% 
  group_by(Taxonomy) %>% 
  summarize(Sum_count = sum(Count)) # %>% view() # this is extremely rare

# join with quad list
fish_2 %>% 
  filter(Count > 0) %>% # two instances where no fish was observed
  # get rid of negative biomass observations
  filter(Biomass >= 0) %>% 
  full_join(transect_list) %>% 
  # fill in for missing
  mutate(Taxonomy = case_when(is.na(Taxonomy) ~ NA, 
                              TRUE ~ Taxonomy),
         Fine_Trophic = case_when(is.na(Fine_Trophic) ~ NA, 
                                  TRUE ~ Fine_Trophic),
         Count = case_when(is.na(Count) ~ 0, 
                           TRUE ~ Count),
         Biomass = case_when(is.na(Biomass) ~ 0, 
                           TRUE ~ Biomass)
  ) -> fish_3


## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##

fish_3 %>% 
  mutate(site = "mcr",
         taxa_type = str_to_lower(Fine_Trophic),
         ecosystem = "aquatic",
         habitat_broad = "coral_reef",
         habitat_fine = str_to_lower(Habitat),
         biome = "tropical",
         guild = "fish",
         herbivore = NA,
         year = Year,
         month = month(as.Date(Date)),
         day = day(as.Date(Date)),
         plot = paste0("lter_", Site), 
         subplot = paste0(Transect, "_", Swath, "m"),
         unique_ID = paste0(site, "_", habitat_fine, "_", plot),
         unit_abundance = "g",
         scale_abundance = case_when(Swath == 5 ~ "5x50m",
                                     Swath == 1 ~ "1x50m",
                                     TRUE ~ "ERROR"),
         taxon_name = Taxonomy,
         abundance = Biomass
  ) %>%
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore,
         year, month, day, plot, subplot, unique_ID, unit_abundance,
         scale_abundance, taxon_name, taxon_resolution, abundance, id_confidence) -> fish_4


## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
fish_4 %>% 
  select(year) %>% 
  range()

# number of taxa
fish_4 %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(fish_4, here("../cleaned_data/mcr_fish.csv"))



