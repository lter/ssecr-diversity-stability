# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: May 13th, 2025

rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan, lmerTest, 
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)

# read combined dss
# combined_agg_stability <- read.csv(here::here("data/synthesized_data", "combined_agg_stability.csv")) |> 
#   filter(plot != "AHND")

# Read length CSVs ----
tmp <- tempfile(fileext = ".csv")
drive_folder_producer <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/183ogFcn_kPRpQBp4UUexrzWrfSXGbX6r"), type='csv')
#
df_prod_comp_stability <- data.frame()
for (i in 1:nrow(drive_folder_producer)) {
  # skip usvi_invert
  if (substr(drive_folder_producer$name[i], 1, 11) == "usvi_invert") {next}
  #
  drive_download(drive_folder_producer[i,], path = tmp, overwrite = TRUE)
  df <- read.csv(tmp) %>%
    mutate(prod_comp_stability = 1.0 / mean_length,
           prod_total_length = Trajectory) %>%
    select(site, plot, prod_comp_stability, prod_total_length)
  if (sub("_producer.*", "", drive_folder_producer$name[i]) == 'aims') {
    df$site_new <- "aims_fish"
  } else if (sub("_producer.*", "", drive_folder_producer$name[i]) == 'gce') {
    df$site_new <- "gce_invert"
  } else if (sub("_producer.*", "", drive_folder_producer$name[i]) == 'cdr_of') {
    df$site_new <- "cdr-of"
  } else {
    df$site_new <- sub("_producer.*", "", drive_folder_producer$name[i])
  }
  df_prod_comp_stability <- rbind(df_prod_comp_stability, df)
}
# sbc producer is missing
df_prod_comp_stability$site[df_prod_comp_stability$site == 'CDR']  <- 'cdr_of'

drive_folder_consumer <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1BLsAtTwRdBURwcKk3QnkPV1u_9OYtx7r"), type='csv')
#
df_con_comp_stability <- data.frame()
for (i in 1:nrow(drive_folder_consumer)) {
  # skip usvi_invert
  if (substr(drive_folder_consumer$name[i], 1, 11) == "usvi_invert") {next}
  #
  drive_download(drive_folder_consumer[i,], path = tmp, overwrite = TRUE)
  df <- read.csv(tmp) %>%
    mutate(con_comp_stability = 1.0 / mean_length,
           con_total_length = Trajectory) %>%
    select(site, plot, con_comp_stability, con_total_length)
  # adding the type of consumers
  if (sub("_consumer.*", "", drive_folder_consumer$name[i]) == "aims") {
    df$site_new <- "aims_fish"
  } else if (sub("_consumer.*", "", drive_folder_consumer$name[i]) == "gce") {
    df$site_new <- "gce_invert"
  } else if (sub("_consumer.*", "", drive_folder_consumer$name[i]) == "cdr_of") {
    df$site_new <- "cdr-of"
  } else {
    df$site_new <- sub("_consumer.*", "", drive_folder_consumer$name[i])
  }
  df_con_comp_stability <- rbind(df_con_comp_stability, df)
}
 
df_comp_stability <- merge(df_prod_comp_stability, df_con_comp_stability, all = TRUE)

# we are missing mcr_invert, sbc_producer, gce_consumer
write.csv(df_comp_stability, 'scripts/wrangling_junna/synthesized_data/combined_comp_stability.csv', row.names = F)
#
googledrive::drive_upload(media = file.path("scripts/wrangling_junna/synthesized_data/combined_comp_stability.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"))

