#### Cleaning SBC biomass data ----

## Data downloaded from: XX
## on XX
## dataset published on XX

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: March 13, 2025

# Purpose:
## Clean the PISCO kelp forest data and get it into the correct format

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

fish <- read_csv(here("..", "data", "PISCO", "PISCO_kelpforest_fish.1.4.csv"))
swath <- read_csv(here("..", "data", "PISCO", "PISCO_kelpforest_swath.1.3.csv"))
upc <- read_csv(here("..", "data", "PISCO", "PISCO_kelpforest_upc.1.3.csv"))

## -------------------------------------------- ##
#           Temporal scope of data ----
## -------------------------------------------- ##

# are survey_year and year the same?
unique(fish$survey_year == fish$year) # yes!

# get temporal distribution of sites/sampling
fish %>% 
  group_by(year, site) %>% 
  summarize(total_fish = sum(count)) %>% 
  pivot_wider(names_from = year,
              values_from = total_fish) %>% write_csv(here("..", "data", "pisco_site_survey_distribution.csv"))

# manually annotated then bring it back in:
temporal_span <- read_csv(here("..", "data", "pisco_site_survey_distribution_manual.csv"))

# how many sites have at least 10 years continuous:
temporal_span %>% 
  filter(CONSECUTIVE_10YEARS == "y") %>% dim() # 89

temporal_span %>% 
  filter(CONSECUTIVE_10YEARS == "9") %>% dim() # 7

## -------------------------------------------- ##
#                   QA/QC ----
## -------------------------------------------- ##

# seems like there can be different methods in the same year/site?
# with method:
fish %>% 
  group_by(year, method, site, zone, transect) %>% 
  summarize(total_fish = sum(count)) %>% dim() # more than the next

# without:
fish %>% 
  group_by(year, site, zone, transect) %>% 
  summarize(total_fish = sum(count)) %>% dim() # 1,180 fewer

# there are missing years within the time series
