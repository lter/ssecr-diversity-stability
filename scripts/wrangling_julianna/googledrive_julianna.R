#### Setting up directories and pulling data from Drive ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Julianna Renzi
#### Last Updated: January 16th, 2025

# Purpose:
## Create the directory structure and download files from Google Drive
## for Julianna's code

## ------------------------------------------ ##
#            Housekeeping -----
## ------------------------------------------ ##


# Clear environment for maximum reproducibility
rm(list=ls())

# load librarian (package for installing/loading packages)
if (!require("librarian")) install.packages("librarian")

# Load other necessary libraries
librarian::shelf(here, # relative file paths
                 tidyverse, # data wrangling
                 googledrive, # interfacing with Google Drive
                 httpuv # helps Google Drive package
)

## -------------------------------------------- ##
#             Create file structure ----
## -------------------------------------------- ##

# create MCR folder structure
dir.create(path = here("..", "MCR"), showWarnings = F)
dir.create(path = here("..", "MCR", "l2_clean_data"))
dir.create(path = here("..", "MCR", "l1_raw_data"))
dir.create(path = here("..", "MCR", "taxa_tables"))

# create SBC folder structure
dir.create(path = here("..", "SBC"), showWarnings = F)
dir.create(path = here("..", "SBC", "l2_clean_data"))
dir.create(path = here("..", "SBC", "l1_raw_data"))
dir.create(path = here("..", "SBC", "taxa_tables"))
           
# create PISCO folder structure
dir.create(path = here("..", "PISCO"), showWarnings = F)
dir.create(path = here("..", "PISCO", "l2_clean_data"))
dir.create(path = here("..", "PISCO", "l1_raw_data"))
dir.create(path = here("..", "PISCO", "taxa_tables"))

## -------------------------------------------- ##
#             Load data from Drive ----
## -------------------------------------------- ##

# more information on this process here: https://lter.github.io/scicomp/tutorial_googledrive-pkg.html 
googledrive::drive_auth(email = "jrenzi@ucsb.edu") # authenticate Google Drive

drive_url <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/19wQilfKU8BYMxidLMoMfE8sq_P-nyr12")
drive_folder <- googledrive::drive_ls(path = drive_url)






