#### Author(s): Julianna Renzi
#### Last Updated: May 8, 2025

# Purpose:
## Explore the SBC/MCR datasets using our cool functions

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

# Load functions
source(here("scripts/00_functions.R"))

## -------------------------------------------- ##
#             Load data ----
## -------------------------------------------- ##

sbc <- read_csv(here("../cleaned_data/sbc_all.csv")) %>% 
  # just want taxa we're confident on
  filter(id_confidence == 1) %>% 
  filter(taxa_type != "suspension_feeder") %>% 
  mutate(guild = case_when(guild == "macroalgae" ~ "algae",
                           TRUE ~ guild))

mcr_invert <- read_csv(here("../cleaned_data/mcr_invertebrate.csv")) %>% 
  filter(id_confidence == 1)

mcr_fish <- read_csv(here("../cleaned_data/mcr_fish.csv")) %>% 
  filter(id_confidence == 1)

mcr_benthic <- read_csv(here("../cleaned_data/mcr_algae.csv")) %>% 
  filter(id_confidence == 1) %>% 
  filter(taxa_type != "suspension_feeder") %>% 
  mutate(guild = case_when(guild == "macroalgae" ~ "algae",
                           TRUE ~ guild))

## -------------------------------------------- ##
#             Make mega df ----
## -------------------------------------------- ##

j_data <- as_tibble(rbind(sbc,
                 mcr_invert,
                  mcr_fish,
                  mcr_benthic))


## -------------------------------------------- ##
#            Calculate statistics ----
## -------------------------------------------- ##

j_data %>% 
  dplyr::group_by(plot) %>% 
  dplyr::summarize(CoVar = mean(abundance)) %>% view()


filter_data()

