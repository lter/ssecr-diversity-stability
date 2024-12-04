#### Cleaning SBC biomass data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.50.17 
## on December 3, 2024
## dataset published on 2024-09-13

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: December 3rd, 2024

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
updated_taxonomy <- read_csv(here("../taxa_tables/SBC_taxa_annotated.csv"))

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
#   select(SCIENTIFIC_NAME) %>% 
#   unique() %>% 
#   mutate(SCIENTIFIC_NAME = as.character(SCIENTIFIC_NAME)) %>% 
#   arrange(SCIENTIFIC_NAME) %>% 
#   write_csv(here("../taxa_tables/SBC_taxa.csv"))

## CHANGES TO TAXONOMY
# "Agalophenia spp." to "Aglaophenia spp."
# Crisia occidentalis to Filicrisia franciscana
# Dodecaceria fewkesi to Dodecaceria pacifica



# Start at 2008
