library(librarian)
# Install missing packages and load needed libraries
shelf(tidyverse, googlesheets4, googledrive, readxl)
rm(list=ls())

source('scripts/00_functions_minimize.R')


# References related to this study: 
# Changes in plant-herbivore network structure and robustness along land-use intensity gradients in grasslands and forests

tmp <- tempfile(fileext = ".csv")
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1aNuNslzIM4g03cZ85rjgsG-bbE1fuGec"), type='csv')

drive_download(drive_folder[grepl('BEX_SE_producer', drive_folder$name),], path = tmp, overwrite = TRUE)
BEX_SE_producer <- read.csv(tmp)

drive_download(drive_folder[grepl('BEX_SE_consumer', drive_folder$name),], path = tmp, overwrite = TRUE)
BEX_SE_consumer <- read.csv(tmp)

drive_download(drive_folder[grepl('BEX_HE_producer', drive_folder$name),], path = tmp, overwrite = TRUE)
BEX_HE_producer <- read.csv(tmp)

drive_download(drive_folder[grepl('BEX_HE_consumer', drive_folder$name),], path = tmp, overwrite = TRUE)
BEX_HE_consumer <- read.csv(tmp)

drive_download(drive_folder[grepl('BEX_AE_producer', drive_folder$name),], path = tmp, overwrite = TRUE)
BEX_AE_producer <- read.csv(tmp)

drive_download(drive_folder[grepl('BEX_AE_consumer', drive_folder$name),], path = tmp, overwrite = TRUE)
BEX_AE_consumer <- read.csv(tmp)

##
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/14ng9TmoBsTR0PHhbU_HNtbpZ6XcU5_fE"), type='csv')
drive_download(drive_folder, path = tmp, overwrite = TRUE)
LUI <- read.csv(tmp)
# land use starts between 2006 and 2024. 
# producer: 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018
# consumer: 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019

# TotalFertilization; we have 95 plots with fertilization effects
plotID_fertilized <- unique(LUI$EP_PlotID[LUI$TotalFertilization > 0])

LUI_fertilization <- LUI %>% filter(TotalMowing > 0.0 | TotalGrazing > 0.0) %>% mutate(year = year(as.Date(Year, format = "%m/%d/%Y %H:%M")))

test <- BEX_SE_consumer %>% left_join(LUI_fertilization[, c("year", "EP_PlotID", "TotalFertilization")], by = c("year" = "year", "plot" = "EP_PlotID"))
sum(!is.na(test$TotalFertilization))

# only 200 plots without any treatments. 

# all the fertilization plots are removed. 
filter_data("BEX_SE", # site name as string
            BEX_SE_producer, # producer df
            BEX_SE_consumer, # consumer df
            mean_sum = "mean", 
            minimize = TRUE)

# Number of effective plots:  20 ; plot names:  SEG14, SEG15, SEG16, SEG18, SEG19, SEG22, SEG24, SEG27, SEG28, SEG29, SEG30, SEG31, SEG32, SEG37, SEG38, SEG40, SEG41, SEG46, SEG49, SEG50 
# Number of sampling years:  11 ; sampling years:  2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018 

filter_data("BEX_HE", # site name as string
            BEX_HE_producer, # producer df
            BEX_HE_consumer, # consumer df
            mean_sum = "mean", 
            minimize = TRUE)

# Number of effective plots:  15 ; plot names:  HEG16, HEG17, HEG18, HEG19, HEG20, HEG21, HEG24, HEG39, HEG40, HEG41, HEG42, HEG43, HEG44, HEG45, HEG46 
# Number of sampling years:  11 ; sampling years:  2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018 

filter_data("BEX_AE", # site name as string
            BEX_AE_producer, # producer df
            BEX_AE_consumer, # consumer df
            mean_sum = "mean", 
            minimize = TRUE)

# Number of effective plots:  12 ; plot names:  AEG25, AEG26, AEG27, AEG28, AEG32, AEG33, AEG34, AEG44, AEG46, AEG47, AEG48, AEG49 
# Number of sampling years:  10 ; sampling years:  2008, 2009, 2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018 

# we can see the results
