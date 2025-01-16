#### Cleaning MCR invertebrate data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcr&identifier=7&revision=35
## on November 20, 2024
## dataset published on 2023-12-20

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: November 20th, 2024

# Purpose:
## Clean the Moorea herbivore and corallivore dataset

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

# note: might also want to consider the COTS time series
# 20 fixed 1m^2 quadrats per site/habitat/year, divided into 5 "transects" of 50 m
# Beginning in 2009, Acanthaster are noted when one meter away, by including the note 
# '(1 m away)' in the species field. Beginning in 2013, Culcita are similarly noted 
# when one meter away. And beginning in 2018, Linckia are so noted.
# In 2005-2006, where none of this set of invertebrates were observed within a 
# quadrat, the words 'No invertebrate observed' appear in the taxonomy field with 
# a count of zero. In 2007 and later, where none of this set of invertebrates were 
# observed within a quadrat, all species observed elsewhere on the transect are listed with a count of zero.
# The “year one” survey spread between 2005-2006 to cover all locations. Each subsequent year contains a complete survey
# In 2021 fore reef 10 m and 17 m surveys were not conducted due to COVID border closures in French Polynesia

inverts_1 <- read_csv(here("../data/MCR_invert_abundance_20231220.csv"))
updated_taxonomy <- read_csv(here("../taxa_tables/MCR_invert_taxa_annotated.csv"))

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

# Make taxa table
#inverts %>% 
#  select(Taxonomy) %>% 
#  unique() %>% 
#  arrange(Taxonomy) %>% 
#  write_csv(here("../taxa_tables/MCR_invert_taxa.csv"))

# Changed Tectus niloticus to Rochia nilotica

# update taxonomy and trophic designations (done manually)
inverts_1 %>% 
  full_join(updated_taxonomy) %>% 
  select(-Taxonomy) %>% 
  rename(Taxonomy = 
           Taxonomy_updated) %>% 
  # use dates after 2006 because the first year was 2005-2006 (not consistent with others)
  filter(Year > 2006)  %>% 
  # exclude organisms off the quadrat (i.e., those that have a "1 m" in their taxa name)
  mutate(Taxonomy_on_quad = case_when(str_detect(Taxonomy, pattern = "1") ~ "off_quad",
                                 TRUE ~ "on_quad")) %>% 
  # filter these out
  filter(Taxonomy_on_quad == "on_quad") -> inverts_2

## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##

# check to see if there's any NA's
is.na(inverts_2) %>% unique() # looks good
# check for any weird placeholder values (e.g., -9999)
inverts_2$Count %>% min() # looks good

# get a list of sites
inverts_2 %>% 
  group_by(Year, Date, Location, Site, Habitat, Transect, Quadrat) %>% 
  summarize(n = n()) %>% # can see n() varies a ton
  select(-n) -> quad_list

# filter all the relevant trophic groups and then join with quad list
inverts_2 %>% 
  filter(Count > 0) %>% 
  filter(Trophic_role == "primary_consumer" | 
           Trophic_role == "omnivore" | 
           Trophic_role == "suspension_feeder") %>% 
  full_join(quad_list) %>% 
  mutate(Taxonomy = case_when(is.na(Taxonomy) ~ NA, 
                              TRUE ~ Taxonomy),
         Trophic_role = case_when(is.na(Trophic_role) ~ "herbivore_any_type", 
                                  TRUE ~ Trophic_role),
         Count = case_when(is.na(Count) ~ 0, 
                                  TRUE ~ Count)
         ) -> invert_3

## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##

invert_3 %>% 
  mutate(site = "mcr",
         taxa_type = Trophic_role,
         ecosystem = "aquatic",
         habitat_broad = "coral_reef",
         habitat_fine = str_to_lower(Habitat),
         biome = "tropical",
         guild = "invertebrate",
         herbivore = NA,
         year = Year,
         month = month(as.Date(Date)),
         day = day(as.Date(Date)),
         plot = str_to_lower(paste0(str_split(Site, pattern = " ")[[1]][1], "_", 
                                    str_split(Site, pattern = " ")[[1]][2])), 
         subplot = paste0(Transect, "_", Quadrat),
         unique_ID = paste0(site, "_", habitat_fine, "_", plot),
         unit_abundance = "count",
         scale_abundance = "1m2",
         taxon_name = Taxonomy,
         abundance = Count
         ) %>% 
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore,
         year, month, day, plot, subplot, unique_ID, unit_abundance,
         scale_abundance, taxon_name, taxon_resolution, abundance) -> invert_4

## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
invert_4 %>% 
  select(year) %>% 
  range()

# number of taxa
invert_4 %>% 
  select(species) %>% 
  unique() %>% 
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(invert_4, here("../cleaned_data/mcr_invertebrate.csv"))


