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

fish_1 <- read_csv(here("../data/MCR_fish_abundance_20240927.csv"))
updated_taxonomy <- read_csv(here("../taxa_tables/MCR_fish_taxa_annotated.csv"))
# HILLARY SPP LIST
# ^ sent to me via Hillary on Dec 3, 2024

# NOTES ON FISH DATA
# 2005 used a different method--start with 2006
# need to deal with the 1m and 5m swaths differently (both x 50 m)
# Need to make decisions about spp not identified to the spp level
# "no fish observed" is an option

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

# Make taxa table
# fish_1 %>% 
#   select(Taxonomy) %>% 
#   unique() %>% 
#   arrange(Taxonomy) %>% 
#   write_csv(here("../taxa_tables/MCR_fish_taxa.csv"))

# list of fishes not identified to species
low_res_fishes <- updated_taxonomy %>% 
  filter(`Low taxonomic resolution` == "Y") %>%
  select(Taxonomy)

# count up the instances of the fishes not identified to species
fish_1 %>% 
  filter(Taxonomy %in% low_res_fishes$Taxonomy) %>% 
  group_by(Taxonomy) %>% 
  summarize(Sum_count = sum(Count)) %>% view() # most there ever is is 47--maybe OK to exclude?
# should ask Tom


