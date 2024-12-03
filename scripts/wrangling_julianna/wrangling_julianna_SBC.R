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

algae_1 <- read_csv(here("../data/MCR_algal_cover_20231211.csv"))
updated_taxonomy <- read_csv(here("../taxa_tables/MCR_algae_taxa_annotated.csv"))

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##
