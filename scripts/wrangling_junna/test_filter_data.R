
# test filter_data function to see how many data we shall have. 
# Junna Wang, 4/7/2025

library(librarian)
shelf('tidyverse', 'vegan', 'ggpubr', "googledrive")

rm(list=ls())

source('scripts/wrangling_junna/00_functions_Junna.R')

# use USVI data
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1d4uwv5Eq5u5hFw22po_7sK-_Uwisc65w"), type='csv')

tmp <- tempfile(fileext = ".csv")

# producer
drive_download(drive_folder[3,], path = tmp, overwrite = TRUE)
producer_data <- read_csv(tmp)

# consumer fish
drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
consumer_data <- read_csv(tmp)

result <- filter_data(site_name = 'USVI', # site name as string
            producer_data = producer_data, # producer df
            consumer_data = consumer_data, # consumer df
            mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
            minimize = FALSE, # subset for shortest possible time series based on spatio-temporally co-located plots
            output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
            write_csv = TRUE)

# potential problems of this function
# 1 what if we have multiple consumer datasets? use optional input variables, the maximum number of consumer datasets we have?
# 2 we need a folder to store filtered data on google drive
# 3 how to use the column unique ID?
# 6 filter out unconfident rows before aggregating? and filter out the data of specific site??
# 7 rm(list = ls()) should be removed.
# 5 the input variable minimize is confusing. 
# 8 we only used site, plot, year, taxon_name and abundance information in this function
# 


