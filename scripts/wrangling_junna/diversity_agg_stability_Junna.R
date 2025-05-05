rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)

# retrieve functions from 00_functions.r
source('scripts/00_functions_minimize.R')
tmp <- tempfile(fileext = ".csv")

drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1yT4XdK6V-6GtXYcXzW_V3HllVSumDBnx"), type='csv')

#### TERRESTRIAL DATA ####

##### Read in data and create agg stability dfs #####
# NOTE: PROBABLY A FASTER WAY TO DO THIS

###### KNZ ######
drive_download(drive_folder[grepl('knz_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
knz_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('knz_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
knz_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = knz_prod_wide, consumer_data = knz_con_wide, "knz")

###### KBS ######
drive_download(drive_folder[grepl('kbs_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
kbs_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('kbs_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
kbs_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = kbs_prod_wide, consumer_data = kbs_con_wide, "kbs")

###### CDR OLD FIELD ######
drive_download(drive_folder[grepl('cdr_of_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
cdr_of_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('cdr_of_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
cdr_of_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = cdr_of_prod_wide, consumer_data = cdr_of_con_wide, "cdr_of")

###### COMBINE TERRESTRIAL AGGREGATE DFS ######
terrestrial_agg_stability <- rbind(
  knz_aggregate_dss,
  kbs_aggregate_dss,
  cdr_of_aggregate_dss
)

write.csv(row.names = F, terrestrial_agg_stability, 'scripts/wrangling_junna/synthesized_data/terrestrial_agg_dss.csv')
googledrive::drive_upload(media = file.path("scripts/wrangling_junna/synthesized_data/terrestrial_agg_dss.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"))

#### MARINE DATA ####
##### read in and create agg dss dataframes #####

###### GCE ######
drive_download(drive_folder[grepl('gce_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
gce_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('gce_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
gce_con_wide <- read_csv(tmp)
#
calculate_agg_stability(producer_data = gce_prod_wide, consumer_data = gce_con_wide, "gce")

###### USVI ######
drive_download(drive_folder[grepl('usvi_invert_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
usvi_invert_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('usvi_invert_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
usvi_invert_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = usvi_invert_prod_wide, consumer_data = usvi_invert_con_wide, "usvi_invert")
####------

drive_download(drive_folder[grepl('usvi_fish_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
usvi_fish_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('usvi_fish_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
usvi_fish_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = usvi_fish_prod_wide, consumer_data = usvi_fish_con_wide, "usvi_fish")

###### SBC ######
drive_download(drive_folder[grepl('sbc_invert_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
sbc_invert_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('sbc_invert_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
sbc_invert_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = sbc_invert_prod_wide, consumer_data = sbc_invert_con_wide, "sbc_invert")
####------

drive_download(drive_folder[grepl('sbc_fish_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
sbc_fish_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('sbc_fish_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
sbc_fish_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = sbc_fish_prod_wide, consumer_data = sbc_fish_con_wide, "sbc_fish")

###### MCR ######
drive_download(drive_folder[grepl('mcr_invert_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
mcr_invert_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('mcr_invert_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
mcr_invert_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = mcr_invert_prod_wide, consumer_data = mcr_invert_con_wide, "mcr_invert")
####------

drive_download(drive_folder[grepl('mcr_fish_producers', drive_folder$name),], path = tmp, overwrite = TRUE)
mcr_fish_prod_wide <- read_csv(tmp)
#
drive_download(drive_folder[grepl('mcr_fish_consumers', drive_folder$name),], path = tmp, overwrite = TRUE)
mcr_fish_con_wide <- read_csv(tmp)

calculate_agg_stability(producer_data = mcr_fish_prod_wide, consumer_data = mcr_fish_con_wide, "mcr_fish")

###### COMBINE MARINE AGGREGATE DFS ######
# add one more column, because some marine sites have two sets of producer-consumer data
gce_aggregate_dss <- gce_aggregate_dss %>% mutate(site_new = 'gce_invert')
usvi_invert_aggregate_dss <- usvi_invert_aggregate_dss %>% mutate(site_new = 'usvi_invert')
usvi_fish_aggregate_dss <- usvi_fish_aggregate_dss %>% mutate(site_new = 'usvi_fish')
sbc_invert_aggregate_dss <- sbc_invert_aggregate_dss %>% mutate(site_new = 'sbc_invert')
sbc_fish_aggregate_dss <- sbc_fish_aggregate_dss %>% mutate(site_new = 'sbc_fish')
mcr_invert_aggregate_dss <- mcr_invert_aggregate_dss %>% mutate(site_new = 'mcr_invert')
mcr_fish_aggregate_dss <- mcr_fish_aggregate_dss %>% mutate(site_new = 'mcr_fish')

marine_agg_stability <- rbind(
  gce_aggregate_dss,
  usvi_invert_aggregate_dss,
  usvi_fish_aggregate_dss,
  sbc_invert_aggregate_dss,
  sbc_fish_aggregate_dss,
  mcr_invert_aggregate_dss,
  mcr_fish_aggregate_dss
)

write.csv(row.names = F, marine_agg_stability, 'scripts/wrangling_junna/synthesized_data/marine_agg_dss.csv')
googledrive::drive_upload(media = file.path("scripts/wrangling_junna/synthesized_data/marine_agg_dss.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"))



