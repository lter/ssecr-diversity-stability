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
