#### Cleaning MCR algae data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-mcr&identifier=8&revision=37 
## on March 7, 2025
## dataset published on 2024-12-19

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: March 7th, 2025

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

algae_1 <- read_csv(here("../data/MCR_algal_cover_20241219.csv"))
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

dim(nodata) # see there are 14 rows with no data

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

dim(incorrect_quads) # see there are 8 quadrats of 216,808, which is pretty good!

# Exclude incorrect quads
algae_3 %>% 
  # create a quadrat ID, so we can exclude these
  mutate(Quad_ID = paste0(Year, "_", Location)) %>% 
  # remove incorrect quadrats
  filter(!(Quad_ID %in% incorrect_quads$Quad_ID)) -> algae_4

## -------------------------------------------- ##
#             SSECR format ----
## -------------------------------------------- ##

## Going to keep everything (not JUST algae), but don't want sand/etc.
# first get a key of all quadrats 
algae_4 %>% 
  # list of all quadrats/information
  group_by(Year, Date, Location, Site, Habitat, Transect, Quadrat) %>% 
  summarize(n = n()) %>% 
  # get rid of meaningless column
  select(-n) -> quad_key

# get just organisms we care about, but re-join with quad_key to make sure we also have zeros
algae_4 %>% 
  filter(!is.na(guild)) %>% 
  full_join(quad_key) %>% 
  filter(is.na(Taxonomy_Substrate_Functional_Group)) %>%
  head() # okay great there are no instances of this

# This is because the dataset puts 0's when 0's were observed (e.g., full sand quadrats, like the one below)
algae_4 %>% 
  filter(Quad_ID == "2006_LTER 2 Backreef Algae Transect 1 Quad 10") 

# now format in the style we need
algae_4 %>% 
  # get just the spp we care about:
  filter(!is.na(guild)) %>% 
  mutate(site = "mcr") %>% 
  mutate(taxa_type = feeding_type) %>% 
  mutate(ecosystem = "aquatic") %>% 
  mutate(habitat_broad = "coral_reef") %>% 
  mutate(biome = "tropical") %>% 
  mutate(guild = guild) %>% 
  mutate(herbivore = "no") %>% 
  mutate(habitat_fine = str_to_lower(Habitat)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  rename(year = Year) %>% 
  mutate(month = month(Date), 
         day = day(Date)) %>% 
  mutate(plot = str_to_lower(paste0(str_split(Site, pattern = "_")[[1]][1], "_", 
                                    str_split(Site, pattern = "_")[[1]][2]))) %>% 
  mutate(subplot = paste0(Transect, "_", Quadrat)) %>% 
  mutate(unique_ID = paste0(site, "_", habitat_fine, "_", plot)) %>% 
  mutate(unit_abundance = "percent") %>% 
  mutate(scale_abundance = "0.25m2") %>% 
  mutate(taxon_name = Taxonomy_Substrate_Functional_Group) %>% 
  mutate(abundance = Percent_Cover) %>% 
  # filter 2005/2006
  filter(year > 2006) %>% 
  # add a column saying we're confident in all the spp taxonomies
  mutate(id_confidence = 1) %>% 
  select(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, 
         year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, 
         taxon_name, taxon_resolution, abundance, id_confidence) -> algae_5

## -------------------------------------------- ##
#             Summary stats ----
## -------------------------------------------- ##

# year range
algae_5 %>% 
  select(year) %>% 
  range()

# number of producer taxa
algae_5 %>% 
  filter(taxa_type == "producer") %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()
  
# number of total taxa
algae_5 %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()


# number of suspension feeding taxa
algae_5 %>% 
  select(taxon_name) %>% 
  unique() %>% 
  dim()

# number of macroalgal taxa  
algae_5 %>% 
  filter(taxa_type == "suspension_feeder") %>%
  select(taxon_name) %>% 
  unique() %>%  
  dim()

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(algae_5, here("../cleaned_data/mcr_algae.csv"))
          
