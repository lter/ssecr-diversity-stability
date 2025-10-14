#### Cleaning JRN plant data ----

## Data downloaded from: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-jrn.210262001.11
## on July 23, 2025
## dataset published on 2024-03-08

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: July 23rd, 2025

# Purpose:
## Clean the Jornada plant dataset

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

plant_1 <- read_csv(here("../JORN/Ecotone_quadrat_plant_cover.csv")) 

## -------------------------------------------- ##
#             Taxa cleaning ----
## -------------------------------------------- ##

# Deal with no vegetation quadrats:
plant_1 %>% 
  # replace no vegetation with zeros
  mutate(cover = case_when(Species_binomial == "no vegetation" ~ "0",
                           TRUE ~ cover)) %>% 
  mutate(Species_binomial = case_when(Species_binomial == "no vegetation" ~ "Acanthochiton wrightii",
                                      TRUE ~ Species_binomial)) -> plant_2

# Make taxa table
plant_2 %>% 
  select(Species_binomial) %>% 
  unique() %>% 
  arrange(Species_binomial) %>% 
  write_csv(here("../taxa_tables/JRN_plant_taxa.csv"))

# Examine strange taxa/instances
plant_2 %>% 
  filter(Species_binomial == ".") %>% view()


plant_2 %>% 
  filter(Species_binomial == "missing") %>% view()

plant_2 %>% 
  filter(str_detect(Species_binomial, pattern = "unidentified")) %>% view()

## -------------------------------------------- ##
#             QA QC ----
## -------------------------------------------- ##

# Make sure all site/year/habitats are accounted for
plant_2 %>% 
  group_by(year, site, season, zone) %>% # zone = habitat
  summarize(n = n()) %>% 
  group_by(year, season) %>% 
  summarize(n = n()) # see that in 2005

# Check max cover in a quad
plant_2 %>% 
  group_by(year, site, season, zone, row, quad) %>% # zone = habitat
  summarize(cover_sum = sum(cover)) %>% view()

