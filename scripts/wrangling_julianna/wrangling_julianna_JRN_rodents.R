#### Cleaning JRN rodent data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-jrn.210262008.141 
## on July 23, 2025
## dataset published on 2022-04-19

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: July 24th, 2025

# Purpose:
## Clean the Jornada rodent dataset

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

rodent_1 <- read_csv(here("../JORN/Ecotone_Rodent_Capture.csv")) 

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

rodent_1 %>% 
  

## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##

# First, make sure there are no duplicate individuals
rodent_1 %>% 
  group_by(Year, Site, Habitat, Tag, Species_binomial) %>% 
  summarize(n = n()) %>% 
  view()


# Make sure all site/year/habitats are accounted for
rodent_1 %>% 
  group_by(Year, Site, Habitat) %>% 
  summarize(n = n()) %>% 
  group_by(Year) %>% 
  summarize(n = n())
  view()






