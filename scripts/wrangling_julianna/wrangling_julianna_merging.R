#### Merging synthesis data files ----

## Using cleaned files generated from my wrangling scripts

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: December 4th, 2024

# Purpose:
## Join files

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

mcr_algae <- read_csv(here("../cleaned_data/mcr_algae_cleaned.csv"))
mcr_fish <- read_csv(here("../cleaned_data/mcr_fish_cleaned.csv"))
mcr_invert <- read_csv(here("../cleaned_data/mcr_invert_cleaned.csv"))

## -------------------------------------------- ##
#             Join data ----
## -------------------------------------------- ##
mcr_algae %>% 
  rbind(mcr_fish) %>% 
  rbind(mcr_invert) -> mcr_merged

# check
is.na(mcr_merged) %>% unique() # looks good!

## -------------------------------------------- ##
#             Write CSV ----
## -------------------------------------------- ##

write_csv(mcr_merged, here("../cleaned_data/mcr_merged.csv"))


