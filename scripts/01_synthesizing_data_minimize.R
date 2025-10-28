# use filter_data function to produce another set of data 
# Junna Wang, 4/7/2025

library(librarian)
shelf('tidyverse', 'vegan', 'ggpubr', "googledrive")

rm(list=ls())

source('scripts/00_functions.R')

tmp <- tempfile(fileext = ".csv")

#----------------------------------------------------use USVI data------------------------------------------
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1d4uwv5Eq5u5hFw22po_7sK-_Uwisc65w"), type='csv')

# producer
drive_download(drive_folder[3,], path = tmp, overwrite = TRUE)
producer_data <- read_csv(tmp)

# consumer fish
drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
fish_data <- read_csv(tmp)

drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
invert_data <- read_csv(tmp)

result <- filter_data(site_name = 'usvi_fish', # site name as string
                      producer_data = producer_data, # producer df
                      consumer_data = fish_data, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)

# write data to google drive
googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_fish_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_fish_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

# removed invert data for usvi because they are senssile
# result <- filter_data(site_name = 'usvi_invert', # site name as string
#                       producer_data = producer_data, # producer df
#                       consumer_data = invert_data,   # consumer df
#                       mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
#                       minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
#                       output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
#                       write_csv = TRUE)
# 
# # write data to google drive
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_invert_producers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))
# 
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_invert_consumers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

# #------------------------------------------------------------more USVI updates------------------------------------------------------
# # update the non-minimize result for USVI
# result <- filter_data(site_name = 'usvi_fish', # site name as string
#                       producer_data = producer_data, # producer df
#                       consumer_data = fish_data, # consumer df
#                       mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
#                       minimize = FALSE, # subset for shortest possible time series based on spatio-temporally co-located plots
#                       output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
#                       write_csv = TRUE)
# 
# # write data to google drive
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_fish_producers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1AY6SAOQkBr3V4foxPtgi-Q3hlyqYXqRG"))
# 
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_fish_consumers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1AY6SAOQkBr3V4foxPtgi-Q3hlyqYXqRG"))
# 
# #----------------------------------------------------------------------
# result <- filter_data(site_name = 'usvi_invert', # site name as string
#                       producer_data = producer_data, # producer df
#                       consumer_data = invert_data, # consumer df
#                       mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
#                       minimize = FALSE, # subset for shortest possible time series based on spatio-temporally co-located plots
#                       output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
#                       write_csv = TRUE)
# 
# # write data to google drive
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_invert_producers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1AY6SAOQkBr3V4foxPtgi-Q3hlyqYXqRG"))
# 
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/usvi_invert_consumers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1AY6SAOQkBr3V4foxPtgi-Q3hlyqYXqRG"))

#————————————————————————————————————————————————————————————————SBC—————————————————————————————————————————————————
# SBC: Santa Barbara Coastal
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xy4aChSNzkMCYCiU3uhzo4KHhf_W20_U"), type='csv')
# only one dataset
drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
sbc_all <- read_csv(tmp)

# subset producers
sbc_prod <- subset(sbc_all, taxa_type == "producer")

# coerce macroalge = algae, as we do not make that distinction in our protocol anymore
sbc_prod <- sbc_prod %>%
  mutate(guild = ifelse(guild == "algae", "macroalgae", guild))

sbc_prod <- subset(sbc_prod, id_confidence == 1)

# subset out invert and fish as unique consumer guilds
sbc_invert <- subset(sbc_all, guild == "invert")
sbc_fish <- subset(sbc_all, guild == "fish")
sbc_fish <- subset(sbc_fish, id_confidence == 1)

# remove suspension feeders from inverts --> don't interact with the benthos/primary producers at all as consumers
sbc_invert <- sbc_invert[!(sbc_invert$taxa_type %in% c("suspension_feeder_detritivore", "suspension_feeder")),]
sbc_invert <- subset(sbc_invert, id_confidence == 1)

#
result <- filter_data(site_name = 'sbc_fish', # site name as string
                      producer_data = sbc_prod, # producer df
                      consumer_data = sbc_fish, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/sbc_fish_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/sbc_fish_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

#
result <- filter_data(site_name = 'sbc_invert', # site name as string
                      producer_data = sbc_prod, # producer df
                      consumer_data = sbc_invert, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/sbc_invert_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/sbc_invert_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

#—————————————————————————————————————————————————————————————————MCR—————————————————————————————————————————————————————
# MCR: Moorea Coral Reef LTER
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1Cw47XY2FZwR9BkzeeGClLE4t8qMCaAUC"), type='csv')
# only one dataset
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
mcr_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
mcr_invert <- read_csv(tmp)

drive_download(drive_folder[3,], path = tmp, overwrite = TRUE)
mcr_fish <- read_csv(tmp)

# process these data
# coerce macroalge = algae, as we do not make that distinction in our protocol anymore
# remove filter feeders
mcr_prod <- subset(mcr_prod, taxa_type == "producer") %>%
  mutate(guild = ifelse(guild == "algae", "macroalgae", guild))
mcr_prod <- subset(mcr_prod, id_confidence == 1)

# fish
mcr_fish <- subset(mcr_fish, id_confidence == 1)

# fish transects have two different sizes which makes the merge break
# multiple 5 to scale_abundance == 1x50m; then merge the datasets
mcr_fish$abundance[mcr_fish$scale_abundance == "1x50m"] <- mcr_fish$abundance[mcr_fish$scale_abundance == "1x50m"] * 5
mcr_fish$scale_abundance <- "5x50m"

# group by the data using sum
mcr_fish <- mcr_fish %>% group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, taxon_name, taxon_resolution, id_confidence) %>%
  summarise(abundance = sum(abundance), .groups = "drop")

# inverts - remove suspension feeders for reasons stated above
mcr_invert <- mcr_invert[!(mcr_invert$taxa_type %in% c("suspension_feeder")),]
mcr_invert <- subset(mcr_invert, id_confidence == 1)

# call function for fish
result <- filter_data(site_name = 'mcr_fish', # site name as string
                      producer_data = mcr_prod, # producer df
                      consumer_data = mcr_fish, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/mcr_fish_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/mcr_fish_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

# invertebrates
result <- filter_data(site_name = 'mcr_invert', # site name as string
                      producer_data = mcr_prod, # producer df
                      consumer_data = mcr_invert, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/mcr_invert_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/mcr_invert_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

#------update non-filtering wide data
# result <- filter_data(site_name = 'mcr_fish', # site name as string
#                       producer_data = mcr_prod, # producer df
#                       consumer_data = mcr_fish, # consumer df
#                       mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
#                       minimize = FALSE, # subset for shortest possible time series based on spatio-temporally co-located plots
#                       output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
#                       write_csv = TRUE)
# 
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/mcr_fish_producers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1AY6SAOQkBr3V4foxPtgi-Q3hlyqYXqRG"))
# 
# googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/mcr_fish_consumers_wide_sub.csv"), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/folders/1AY6SAOQkBr3V4foxPtgi-Q3hlyqYXqRG"))
#————————————————————————————————————————————————AIMS—————————————————————————————————————————————————————————————————
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1dk1hOI6pAudVoBXftKiFkx4oPEZkakJk"), type='csv')
# only one dataset
drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
aims_prod <- read_csv(tmp)
# only 7 taxa; all producers are macro-algae, no algae
# how to deal with different scale_abundance in fish data: 50 m2; 250 m2

drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
aims_fish <- read_csv(tmp)
# merge the fish data of scale abundance 250m2 and 50m2

aims_fish$abundance[aims_fish$scale_abundance == "50m2"] <- aims_fish$abundance[aims_fish$scale_abundance == "50m2"] * 5
aims_fish$scale_abundance <- "250m2"

# group by the data using sum
aims_fish <- aims_fish %>% group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, herbivore, year, month, day, plot, subplot, unique_ID, unit_abundance, scale_abundance, taxon_name, id_confidence) %>%
  summarise(abundance = sum(abundance), .groups = "drop")

aims_prod <- subset(aims_prod, id_confidence == 1)
aims_fish <- subset(aims_fish, id_confidence == 1)

result <- filter_data(site_name = 'aims', # site name as string
                      producer_data = aims_prod, # producer df
                      consumer_data = aims_fish, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)
# we have 104 plots and 16 years of data
googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/aims_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/aims_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# # PIE
# drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/16oEmL7Vf4WgjQCw4QacSa5Bl_wfVRCgH"), type='csv')
# # only one dataset
# drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
# producer_data <- read_csv(tmp)
# 
# drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
# consumer_data <- read_csv(tmp)
# 
# result <- filter_data(site_name = 'PIE', # site name as string
#                       producer_data = producer_data, # producer df
#                       consumer_data = consumer_data, # consumer df
#                       mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
#                       minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
#                       output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
#                       write_csv = FALSE)
# NO good data left.
# this sites need to be excluded because only 6 plots, even for producers. 

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# GCE: Georgia Coastal Ecosystems
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1Qyt03OzxgAW2LVgCuuWWaB6-UN9uUEgP"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
gce_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
gce_con <- read_csv(tmp)

# correct column name
colnames(gce_prod)[colnames(gce_prod) == "scale_abundnace"] <- "scale_abundance"
colnames(gce_con)[colnames(gce_con) == "scale_abundnace"] <- "scale_abundance"

# subset confident ids
gce_prod <- subset(gce_prod, id_confidence == 1)
gce_con <- subset(gce_con, id_confidence == 1)

# gce_prod has a -1 instead of a 1 in one of the habitat_fine --> correct to 1?
gce_prod <- gce_prod %>%
  mutate(habitat_fine = ifelse(habitat_fine == -1, 1, habitat_fine))

# NOTE: each plot can belong to 1 of 3 zones - should we re-factor plot to paste0(gce_prod$plot, "-", gce_prod$habitat_fine)?
# moving ahead with this approach - delete these lines if this is incorrect
# filtering seems to not work properly if we don't do this because of spatial co-location issues
gce_prod_mutated <- gce_prod %>%
  mutate(plot = paste0(gce_prod$plot, "-", gce_prod$habitat_fine))

gce_con_mutated <- gce_con %>%
  mutate(plot = paste0(gce_con$plot, "-", gce_con$habitat_fine))

#
result <- filter_data(site_name = 'gce', # site name as string
                      producer_data = gce_prod_mutated, # producer df
                      consumer_data = gce_con_mutated, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)
# it has 21 plots * 12 years

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/gce_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/gce_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

# Terrestrial ecosystems
#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# KNZ: Konza Prairie Long-Term Ecological Research
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1UUXzJUKvjcRZW-yzI78O_GLFtSPUCclg"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
knz_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
knz_con <- read_csv(tmp)

# consumers are read in as integers due to ID codes. convert all spp to factors
knz_con$taxon_name <- as.factor(knz_con$taxon_name)
knz_prod$taxon_name <- as.factor(knz_prod$taxon_name)

result <- filter_data(site_name = 'knz', # site name as string
                      producer_data = knz_prod, # producer df
                      consumer_data = knz_con, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)
# 13 plots * 19 years

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/knz_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/knz_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# KBS: Kellogg Biological Station Long-Term Ecological Research
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1O2n89tOIMNZGXTzCZNb0Qsj_C8dRI09l"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
kbs_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
kbs_con <- read_csv(tmp)

result <- filter_data(site_name = 'kbs', # site name as string
                      producer_data = kbs_prod, # producer df
                      consumer_data = kbs_con, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)
# 6 plots * 26 years

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/kbs_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/kbs_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# JRN: Jordana
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1NPNdNCa2RJgTyiQmSZRURHCqVmFLrCR1"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
jrn_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
jrn_con <- read_csv(tmp)

# convert all spp to factors
jrn_con$taxon_name <- as.factor(jrn_con$taxon_name)
jrn_prod$taxon_name <- as.factor(jrn_prod$taxon_name)

# convert names to lower case
jrn_con$site <- as.factor(tolower(jrn_con$site))
jrn_prod$site <- as.factor(tolower(jrn_prod$site))

# convert producer plotnames to match consumer plotnames
jrn_prod <- jrn_prod %>%
  mutate(plot = str_replace(plot, "_E$", "_ecotone"),
         plot = str_replace(plot, "_G$", "_grassland"),
         plot = str_replace(plot, "_S$", "_shrubland"))


result <- filter_data(site_name = 'jrn', # site name as string
                      producer_data = jrn_prod, # producer df
                      consumer_data = jrn_con, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'data/JRN', # string for output folder if writing csv 
                      write_csv = TRUE)

# NOAM NOTE: MANUALLY UPLOADED WIDE DATAFRAMES!

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# NTL_Trout: Northern Temperate Lakes LTER Trout Lake
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1_YcfL5sht-B0LOJH3gjdYozilxMLHmi6"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
ntl_trout_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
ntl_trout_con <- read_csv(tmp)

# convert all spp to factors
ntl_trout_con$taxon_name <- as.factor(ntl_trout_con$taxon_name)
ntl_trout_prod$taxon_name <- as.factor(ntl_trout_prod$taxon_name)

# convert names to lower case
ntl_trout_con$site <- as.factor(tolower(ntl_trout_con$site))
ntl_trout_prod$site <- as.factor(tolower(ntl_trout_prod$site))

# remove 2005 and 2006 as they are not consecutive with the rest of the data (ends in 1993 otherwise)
ntl_trout_con <- ntl_trout_con[!ntl_trout_con$year %in% c(2005, 2006),]
ntl_trout_prod <- ntl_trout_prod[!ntl_trout_prod$year %in% c(2005, 2006),]

result <- filter_data(site_name = 'ntl_trout', # site name as string
                      producer_data = ntl_trout_prod, # producer df
                      consumer_data = ntl_trout_con, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'data/ntl', # string for output folder if writing csv 
                      write_csv = TRUE)

# NOAM NOTE: MANUALLY UPLOADED WIDE DATAFRAMES!

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# ntl_madison: Northern Temperate Lakes LTER Trout Lake
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1l9tGFwQOyak7bCSn1K5wYqC-xJhodnZt"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
ntl_madison_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
ntl_madison_con <- read_csv(tmp)

# convert all spp to factors
ntl_madison_con$taxon_name <- as.factor(ntl_madison_con$taxon_name)
ntl_madison_prod$taxon_name <- as.factor(ntl_madison_prod$taxon_name)

# convert names to lower case
ntl_madison_con$site <- as.factor(tolower(ntl_madison_con$site))
ntl_madison_prod$site <- as.factor(tolower(ntl_madison_prod$site))

result <- filter_data(site_name = 'ntl_madison', # site name as string
                      producer_data = ntl_madison_prod, # producer df
                      consumer_data = ntl_madison_con, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'data/ntl', # string for output folder if writing csv 
                      write_csv = TRUE)

# NOAM NOTE: MANUALLY UPLOADED WIDE DATAFRAMES!


#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# CDR-Old field: cedar creek
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1yeKgZ8LMfDUS_N2iUJoBLb-u-SNctnhp"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
cdr_of_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
cdr_of_con <- read_csv(tmp)

# only one family sampled in years 1992 and 1993 --> remove
cdr_of_con <- cdr_of_con[!cdr_of_con$year %in% c(1992, 1993),]

result <- filter_data(site_name = 'cdr_of', # site name as string
                      producer_data = cdr_of_prod, # producer df
                      consumer_data = cdr_of_con, # consumer df
                      mean_sum = 'mean', # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                      minimize = TRUE, # subset for shortest possible time series based on spatio-temporally co-located plots
                      output_folder = 'scripts/wrangling_junna/filtered_data', # string for output folder if writing csv (e.g. "data/CDR")
                      write_csv = TRUE)
# 13 plots * 10 years
googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_of_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_of_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

#——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
# CDR diversity experiment: cedar creek
# How to deal with this?
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1UPaIm6Tp8aQH0gUrOCEz8tV_mkArQkhw"), type='csv')
drive_download(drive_folder[2,], path = tmp, overwrite = TRUE)
cdr_biodiv_prod <- read_csv(tmp)

drive_download(drive_folder[1,], path = tmp, overwrite = TRUE)
cdr_biodiv_con <- read_csv(tmp)
cdr_biodiv_con$abundance <- as.numeric(cdr_biodiv_con$abundance)

# cdr_biodiv_con abundances being read in as characters Convert to numeric
cdr_biodiv_con$abundance <- as.numeric(cdr_biodiv_con$abundance)

# set treatments as factor
cdr_biodiv_prod$treatment_seeding <- as.factor(cdr_biodiv_prod$treatment_seeding)
cdr_biodiv_con$treatment_seeding <- as.factor(cdr_biodiv_con$treatment_seeding)

# Split dfs by treatment
cdr_split_prod <- split(cdr_biodiv_prod, cdr_biodiv_prod$treatment_seeding)
cdr_split_con <- split(cdr_biodiv_con, cdr_biodiv_con$treatment_seeding)

# Assign each subset to a variable

# Modify each split to update 'site' and assign to global environment
list2env(
  setNames(
    lapply(names(cdr_split_prod), function(treatment) {
      df_split <- cdr_split_prod[[treatment]]
      df_split$site <- paste0(df_split$site, "_", treatment)
      return(df_split)
    }),
    paste0("cdr_biodiv_prod_", names(cdr_split_prod))
  ),
  envir = .GlobalEnv
)

list2env(
  setNames(
    lapply(names(cdr_split_con), function(treatment) {
      df_split <- cdr_split_con[[treatment]]
      df_split$site <- paste0(df_split$site, "_", treatment)
      return(df_split)
    }),
    paste0("cdr_biodiv_con_", names(cdr_split_con))
  ),
  envir = .GlobalEnv
)

# filter data for each treatment - there's probably a better way to do this
filter_data(site_name = "cdr_biodiv_1", producer_data = cdr_biodiv_prod_1, consumer_data = cdr_biodiv_con_1,
            mean_sum = "mean", output_folder = "scripts/wrangling_junna/filtered_data", write_csv = TRUE, minimize = TRUE)
# only in 2002-2006

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_1_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_1_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))
#----

filter_data(site_name = "cdr_biodiv_2", producer_data = cdr_biodiv_prod_2, consumer_data = cdr_biodiv_con_2,
            mean_sum = "mean", output_folder = "scripts/wrangling_junna/filtered_data", write_csv = TRUE, minimize = TRUE)
# only in 2002-2006

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_2_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_2_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))
#----

filter_data(site_name = "cdr_biodiv_4", producer_data = cdr_biodiv_prod_4, consumer_data = cdr_biodiv_con_4,
            mean_sum = "mean", output_folder = "scripts/wrangling_junna/filtered_data", write_csv = TRUE, minimize = TRUE)
# only in 2002-2006

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_4_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_4_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))
#----

filter_data(site_name = "cdr_biodiv_8", producer_data = cdr_biodiv_prod_8, consumer_data = cdr_biodiv_con_8,
            mean_sum = "mean", output_folder = "scripts/wrangling_junna/filtered_data", write_csv = TRUE, minimize = TRUE)
# only in 2002-2006

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_8_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_8_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))
#----

filter_data(site_name = "cdr_biodiv_16", producer_data = cdr_biodiv_prod_16, consumer_data = cdr_biodiv_con_16,
            mean_sum = "mean", output_folder = "scripts/wrangling_junna/filtered_data", write_csv = TRUE, minimize = TRUE)
# only in 2002-2006

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_16_producers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))

googledrive::drive_upload(media = file.path("scripts/wrangling_junna/filtered_data/cdr_biodiv_16_consumers_wide_sub.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"))
# producer data in before 2001 are excluded because of id_confidence = 0
