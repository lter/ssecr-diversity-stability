#### Cleaning MCR algae data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcr&identifier=8&revision=36 
## on November 19, 2024
## dataset published on 2023-12-12

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: November 19th, 2024

# Purpose:
## Clean the Moorea benthic algal dataset, using code from the Moorea algal 
## working group--specifically Lauren Enright and Noam Altman-Kurosaki

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

algae <- read_csv(here("../data/MCR_algal_cover_20231211.csv"))
updated_taxonomy <- read_csv(here("../taxa_tables/MCR_algae_taxa_annotated.csv"))

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

# make taxa list to check with WORMS (if haven't done already)
# also use this to manually add which taxa are producers/algae
#algae %>% 
#  select(Taxonomy_Substrate_Functional_Group) %>% 
#  unique() %>%
#  write_csv(here("../taxa_tables/MCR_algae_taxa.csv"))

# cross referenced with WORMS and made the following changes:
# Changed Caulerpa peltata to Caulerpa chemnitzia 
# Changed Chnoospora implexa to Pseudochnoospora implexa
# Changed Neogoniolithon frutescens to Neogoniolithon brassica-florida
# Changed Caulerpa pickeringii to Caulerpa webbiana
# Changed Boodlea kaeneana to Cladophoropsis membranacea
# Changed Dictyota divaricata to Dictyota cervicornis

# Used algal classifications based on Noam's classifications
# Classified mat-forming cyanos as macroalgae even though they're not macroalgae in the traditional sense
# and we excluded cyanophyta generally - may want to revisit this if we look at just macroalgae
# "Coelothrix irregularis", "Symploca hydnoides" 

# update taxonomy, if needed
algae %>% 
  full_join(updated_taxonomy) %>% 
  select(-Taxonomy_Substrate_Functional_Group) %>% 
  rename(Taxonomy_Substrate_Functional_Group = 
           Taxonomy_Substrate_Functional_Group_updated)  -> algae_taxa


## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##

# look for quadrats with less than 100% cover (know this would be incorrect)
algae_taxa %>% 
  # Dropping 2020 2020 fringe and back reef data, because it's not real 
  # (is an average of 2019 and 2021 survey data due to errors in classifying benthic components during that year)
  filter(!((Habitat == "Backreef" | Habitat == "Fringing") & Year == 2020)) %>% 
  group_by(Year, Location, Site, Habitat, Transect, Quadrat) %>% 
  summarize(Total_cover = sum(Percent_Cover)) %>% 
  filter(Total_cover != 100) -> incorrect_quads

dim(incorrect_quads) # see there are 76 quadrats of 206,121 (pretty good!)

# remove bad data  
algae_taxa %>% 
  # Dropping 2020 2020 fringe and back reef data, because it's not real 
  # (is an average of 2019 and 2021 survey data due to errors in classifying benthic components during that year)
  filter(!((Habitat == "Backreef" | Habitat == "Fringing") & Year == 2020)) %>% dim()
