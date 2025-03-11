#### Cleaning MCR invertebrate data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcr&identifier=7&revision=37 
## on March 7th, 2025
## dataset published on 2024-12-20

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: March 10th, 2025

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

inverts_1 <- read_csv(here("../data/MCR_invert_abundance_20241219.csv"))
updated_taxonomy <- read_csv(here("../taxa_tables/MCR_invert_taxa_annotated.csv"))

# get a weird version of this for a small purpose (don't want to fully join until filled in 0's but do want updated nomenclature)
updated_taxonomy_1 <- updated_taxonomy %>% select(Taxonomy, Taxonomy_updated)

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
  full_join(updated_taxonomy_1) %>% 
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

# See there are some odd missing ones (know this from the metadata):
inverts_2 %>% 
  filter(Taxonomy == "No invertebrate observed") 
  
# check for any NA's 
inverts_2 %>% 
  filter(Taxonomy != "No invertebrate observed") %>% 
  is.na() %>% 
  unique()

# check for any weird placeholder values (e.g., -9999)
inverts_2$Count %>% min() # looks good
inverts_2$Count %>% max()

## -------------------------------------------- ##
#             Add zeros ----
## -------------------------------------------- ##

# need to add zeros for the plots where there was no invertebrate observed
inverts_2 %>% 
  # replace with first taxa
  mutate(Taxonomy = case_when(Taxonomy == "No invertebrate observed" ~ "Acanthaster planci",
                              TRUE ~ Taxonomy)) -> inverts_3

# can see for most entries they just put zeros for ones where no inverts were observed:
inverts_1 %>% 
  filter(Year == 2007 & 
           Location == "LTER 1 Outer 17 m Invertebrate Herbivores Transect 2 Quad 4")  
# see some sites are all zeros when no invert is observed (supposed normal entry format for most years)

# get a list of sites so we can be sure we also aren't missing any (from first filtering step)
inverts_1 %>% 
  filter(Year > 2006)  %>% 
  group_by(Year, Date, Location, Site, Habitat, Transect, Quadrat) %>% 
  summarize(n = n()) %>% # can see n() varies a ton
  select(-n) -> quad_list

# join with quad list to make sure we're not missing anything
inverts_3 %>% 
  full_join(quad_list) %>% # filter(is.na(Count)) %>% view()
  # still 63 missing that were in the original dataset--I think this is because there were none "off quad"
  mutate(Count = case_when(is.na(Taxonomy) ~ 0, 
                           TRUE ~ Count),
         Taxonomy = case_when(is.na(Taxonomy) ~ "Acanthaster planci", 
                              TRUE ~ Taxonomy)
         ) %>% 
  # join with metadata
  left_join(updated_taxonomy) %>% 
  # fix taxonomy again
  select(-Taxonomy) %>% 
  rename(Taxonomy = 
           Taxonomy_updated) -> invert_4
  
# check what the missing 63 are that we replaced in the last step:
inverts_1 %>% 
  filter(Year == 2019 & Location == "LTER 4 Outer 17 m Invertebrate Herbivores Transect 1 Quad 1") 
# yep, these are from a couple weird instances

# check for NA's in new dataset
invert_4 %>% 
  is.na() %>% unique()


## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##

invert_4 %>% 
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
  # add a column saying we're confident in all the spp taxonomies
  mutate(id_confidence = 1) %>% 
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore,
         year, month, day, plot, subplot, unique_ID, unit_abundance,
         scale_abundance, taxon_name, taxon_resolution, abundance, id_confidence) -> invert_5

## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
invert_5 %>% 
  select(year) %>% 
  range()

# number of taxa
invert_5 %>% 
  select(taxon_name) %>% 
  unique() %>%
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(invert_5, here("../cleaned_data/mcr_invertebrate.csv"))


