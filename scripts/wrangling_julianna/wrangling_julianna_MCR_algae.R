#### Cleaning MCR algae data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcr&identifier=8&revision=36 
## on November 19, 2024
## dataset published on 2023-12-12

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: November 20th, 2024

# Purpose:
## Clean the Moorea benthic algal dataset, inspired by code from the Moorea algal 
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

algae_1 <- read_csv(here("../data/MCR_algal_cover_20231211.csv"))
updated_taxonomy <- read_csv(here("../taxa_tables/MCR_algae_taxa_annotated.csv"))

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

# make taxa list to check with WORMS (if haven't done already)
# also use this to manually add which taxa are producers/algae
#algae %>% 
#  select(Taxonomy_Substrate_Functional_Group) %>% 
#  unique() %>%
#  arrange(Taxonomy_Substrate_Functional_Group) %>% 
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
algae_1 %>% 
  full_join(updated_taxonomy) %>% 
  select(-Taxonomy_Substrate_Functional_Group) %>% 
  rename(Taxonomy_Substrate_Functional_Group = 
           Taxonomy_Substrate_Functional_Group_updated) -> algae_2


## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##

# look for "no data" rows, which have a -1 code and mean no data were collected for the quadrat

# LE talked to Hillary and we found that of the 85 rows that have sums that do not = 100, 
# 49 of them are because a "-1" was accidentally added to the no data column, even though data was 
# taken. (100 percent cover - 1 = 99). This happened only in 2017. Hillary will fix this eventually on EDI, 
# but for now I am going to manually remove these -1 from the data set so we can continue with our analyses 


algae_2 %>% 
  # Dropping 2020 fringe and back reef data, because it's not real 
  # (is an average of 2019 and 2021 survey data due to errors in classifying benthic components during that year)
  filter(!((Habitat == "Backreef" | Habitat == "Fringing") & Year == 2020)) %>% 
  filter(Taxonomy_Substrate_Functional_Group == "No data") -> nodata

dim(nodata) # see there are 63 rows with no data

# drop no data rows from analysis, as well as 2020 data
algae_2 %>% 
  # Dropping 2020 fringe and back reef data, because it's not real 
  # (is an average of 2019 and 2021 survey data due to errors in classifying benthic components during that year)
  filter(!((Habitat == "Backreef" | Habitat == "Fringing") & Year == 2020)) %>% 
  filter(Taxonomy_Substrate_Functional_Group != "No data") -> algae_3

# look for quadrats with less than 100% cover (know this would be incorrect)
algae_3 %>% 
  group_by(Year, Location, Site, Habitat, Transect, Quadrat) %>% 
  summarize(Total_cover = sum(Percent_Cover)) %>% 
  filter(Total_cover != 100) %>% 
  # create a quadrat ID, so we can exclude these
  mutate(Quad_ID = paste0(Year, "_", Location)) -> incorrect_quads

dim(incorrect_quads) # see there are 13 quadrats of 206,058, which is pretty good!

# Exclude incorrect quads
algae_3 %>% 
  mutate(Quad_ID = paste0(Year, "_", Location)) %>% 
  # remove incorrect quadrats
  filter(!(Quad_ID %in% incorrect_quads$Quad_ID)) -> algae_4

## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##

## Get JUST algae
# first get a key of all quadrats 
algae_4 %>% 
  # list of all quadrats/information
  group_by(Year, Date, Location, Site, Habitat, Transect, Quadrat) %>% 
  summarize(n = n()) %>% 
  # get rid of meaningless column
  select(-n) -> quad_key

# get just algae, but re-join with quad_key to make sure we also have zeros
algae_4 %>% 
  filter(Is_algae == "y") %>% 
  full_join(quad_key) %>% 
  filter(is.na(Taxonomy_Substrate_Functional_Group)) %>%
  head() # okay great there are no instances of this

# now format in the style we need
algae_4 %>% 
  mutate(site = "mcr") %>% 
  mutate(taxa_type = "producer") %>% 
  mutate(ecosystem = "aquatic") %>% 
  mutate(habitat_broad = "coral_reef") %>% 
  mutate(biome = "tropical") %>% 
  mutate(guild = "algae") %>% 
  mutate(herbivore = "no") %>% 
  mutate(habitat_fine = str_to_lower(Habitat)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  rename(year = Year) %>% 
  mutate(month = month(Date), 
         day = day(Date)) %>% 
  mutate(plot = str_to_lower(paste0(str_split(Site, pattern = " ")[[1]][1], "_", str_split(Site, pattern = " ")[[1]][2]))) %>% 
  mutate(subplot = paste0(Transect, "_", Quadrat)) %>% 
  mutate(unique_ID = paste0(site, "_", habitat_fine, "_", plot)) %>% 
  mutate(unit_abundance = "percent") %>% 
  mutate(scale_abundance = "0.25m2") %>% 
  mutate(species = Taxonomy_Substrate_Functional_Group) %>% 
  mutate(abundance = Percent_Cover) %>% 
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         species, abundance) -> algae_5

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(algae_5, here("../cleaned_data/mcr_algae_cleaned.csv"))

          