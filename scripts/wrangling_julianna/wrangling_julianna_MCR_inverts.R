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

inverts <- read_csv(here("../data/MCR_invert_abundance_20231220.csv"))

# Make taxa table
#inverts %>% 
#  select(Taxonomy) %>% 
#  unique() %>% 
#  arrange() %>% 
#  write_csv(here("../taxa_tables/MCR_invert_taxa.csv"))

# Changed Tectus niloticus to Rochia nilotica



