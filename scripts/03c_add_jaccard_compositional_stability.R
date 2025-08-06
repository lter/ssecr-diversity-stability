
rm(list = ls())
librarian::shelf(tidyverse, googledrive, readr, tools)

# read master stability
drive_auth()
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"), type='csv')
tmp <- tempfile(fileext = ".csv")
download <- drive_download(drive_folder[drive_folder$name=="agg_bray_stability.csv",], path = tmp, overwrite = TRUE)
agg_bray <- read.csv(tmp)

# Read jaccard producer csvs ----

producer_folder_url <- "https://drive.google.com/drive/u/0/folders/1is8cOu-WZzFEYdVeKiwNneQfjQeyu0Wm"
producer_folder_id <- gsub(".*/folders/([^/?]+).*", "\\1", producer_folder_url)

# List CSV files in that folder
producer_csv_files <- drive_ls(as_id(producer_folder_id))
producer_csv_files <- producer_csv_files[grepl("\\.csv$", producer_csv_files$name), ]  # ensure only CSVs

# skip usvi inverts because those are all benthic
producer_skip_file <- "usvi_invert_producer_lengths.csv"

# Loop through and assign each CSV to a dataframe
for (i in seq_len(nrow(producer_csv_files))) {
  original_name <- producer_csv_files$name[i]
  # Skip USVI INVERTS
  if (original_name == producer_skip_file) {
    message("Skipping: ", original_name)
    next
  }
  file_name <- file_path_sans_ext(original_name)
  var_name <- make.names(file_name)
  temp_path <- tempfile(fileext = ".csv")
  
  drive_download(producer_csv_files[i, ], path = temp_path, overwrite = TRUE)
  df <- read_csv(temp_path)
  
  # Apply mutation to all files
  df <- df %>%
    mutate(
      prod_jaccard_stability = 1 / mean_segment_length,
      prod_jaccard_total_length = total_trajectory
    ) %>%
    select(site, plot, prod_jaccard_stability, prod_jaccard_total_length)
  
  # Assign to environment
  assign(var_name, df, envir = .GlobalEnv)
}



# Read jaccard consumer csvs ----
consumer_folder_url <- "https://drive.google.com/drive/u/0/folders/1S8kvBx2lvpF6PNs4VFKXmtjnWbdtsDAK"
consumer_folder_id <- gsub(".*/folders/([^/?]+).*", "\\1", consumer_folder_url)

# List CSV files in that folder
consumer_csv_files <- drive_ls(as_id(consumer_folder_id))
consumer_csv_files <- consumer_csv_files[grepl("\\.csv$", consumer_csv_files$name), ]  # ensure only CSVs

# skip usvi inverts because those are all benthic
consumer_skip_file <- "usvi_invert_consumer_lengths.csv"

# Loop through and assign each CSV to a dataframe
for (i in seq_len(nrow(consumer_csv_files))) {
  original_name <- consumer_csv_files$name[i]
  # Skip USVI INVERTS
  if (original_name == consumer_skip_file) {
    message("Skipping: ", original_name)
    next
  }
  file_name <- file_path_sans_ext(original_name)
  var_name <- make.names(file_name)
  temp_path <- tempfile(fileext = ".csv")
  
  drive_download(consumer_csv_files[i, ], path = temp_path, overwrite = TRUE)
  df <- read_csv(temp_path)
  
  # Apply mutation to all files
  df <- df %>%
    mutate(
      con_jaccard_stability = 1 / mean_segment_length,
      con_jaccard_total_length = total_trajectory
    ) %>%
    select(site, plot, con_jaccard_stability, con_jaccard_total_length)
  
  # Assign to environment
  assign(var_name, df, envir = .GlobalEnv)
}




#### MERGE WITH MASTER DF ####

## REFACTOR CDR, SBC, MCR - needs to be done in the file - talk to james. Brute force here ##
cdr_of_producer_lengths$site <- as.factor( rep("cdr_of", nrow(cdr_of_producer_lengths)) )
sbc_fish_producer_lengths$site <- as.factor( rep("sbc_fish", nrow(sbc_fish_producer_lengths)) )
sbc_fish_consumer_lengths$site <- as.factor( rep("sbc_fish", nrow(sbc_fish_consumer_lengths)) )
sbc_invert_producer_lengths$site <- as.factor( rep("sbc_invert", nrow(sbc_invert_producer_lengths)) )
sbc_invert_consumer_lengths$site <- as.factor( rep("sbc_invert", nrow(sbc_invert_consumer_lengths)) )
mcr_fish_producer_lengths$site <- as.factor( rep("mcr_fish", nrow(mcr_fish_producer_lengths)) )
mcr_fish_consumer_lengths$site <- as.factor( rep("mcr_fish", nrow(mcr_fish_consumer_lengths)) )
mcr_invert_producer_lengths$site <- as.factor( rep("mcr_invert", nrow(mcr_invert_producer_lengths)) )
mcr_invert_consumer_lengths$site <- as.factor( rep("mcr_invert", nrow(mcr_invert_consumer_lengths)) )


## Combine consumer lengths only
all_consumer_lengths <- rbind(
  knz_consumer_lengths,
  aims_consumer_lengths,
  kbs_consumer_lengths,
  cdr_of_consumer_lengths,
  mcr_fish_consumer_lengths,
  mcr_invert_consumer_lengths,
  sbc_invert_consumer_lengths,
  sbc_fish_consumer_lengths,
  usvi_fish_consumer_lengths
)

# Combine producer lengths only
all_producer_lengths <- rbind(
  knz_producer_lengths,
  aims_producer_lengths,
  kbs_producer_lengths,
  cdr_of_producer_lengths,
  mcr_fish_producer_lengths,
  mcr_invert_producer_lengths,
  sbc_invert_producer_lengths,
  sbc_fish_producer_lengths,
  usvi_fish_producer_lengths
)

# Join them separately
master <- agg_bray %>%
  left_join(all_consumer_lengths, by = c("site", "plot")) %>%
  left_join(all_producer_lengths, by = c("site", "plot"))

# write.csv(row.names = F, master, here::here("data/synthesized_data", "master_stability.csv"))
