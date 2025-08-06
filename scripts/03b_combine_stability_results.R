# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: May 13th, 2025

rm(list = ls())
librarian::shelf(tidyverse, googledrive, readr, tools)

# read combined dss
combined_agg_stability <- read.csv(here::here("data/synthesized_data", "combined_agg_stability.csv")) |> 
  filter(plot != "AHND")

# Read bray curtis producer csvs ----
drive_auth()
producer_folder_url <- "https://drive.google.com/drive/u/0/folders/1GHSn0Ffgn4PubbnzEn_vqcNEOejpEoXR"
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
      prod_comp_stability = 1 / mean_segment_length,
      prod_total_length = total_trajectory
    ) %>%
    select(site, plot, prod_comp_stability, prod_total_length)
  
  # Assign to environment
  assign(var_name, df, envir = .GlobalEnv)
}



# Read bray curtis consumer csvs ----
consumer_folder_url <- "https://drive.google.com/drive/u/0/folders/1_85P6tUSAB0MVGDYBmNJ7XUJnO6ZaPEM"
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
      con_comp_stability = 1 / mean_segment_length,
      con_total_length = total_trajectory
    ) %>%
    select(site, plot, con_comp_stability, con_total_length)
  
  # Assign to environment
  assign(var_name, df, envir = .GlobalEnv)
}




#### MERGE WITH COMB STABILITY ####

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
master_stability <- combined_agg_stability %>%
  left_join(all_consumer_lengths, by = c("site", "plot")) %>%
  left_join(all_producer_lengths, by = c("site", "plot"))

# write.csv(row.names = F, master_stability, here::here("data/synthesized_data", "master_stability.csv"))
