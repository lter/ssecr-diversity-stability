# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: March 6th, 2025

# Raw Data Availability ----

# Load necessary packages
# install.packages("librarian") # Uncomment if not already installed
librarian::shelf(tidyverse, googledrive, data.table, ecotraj, vegan, lterdatasampler, supportR, cowplot, summarytools, datacleanr)

# Read in wide-format community matrices
cdr_consumers_1 <- read.csv("data/comm_matrix/cdr_biodiv_1_consumers_wide_sub.csv")
cdr_producers_1 <- read.csv("data/comm_matrix/cdr_biodiv_1_producers_wide_sub.csv")
cdr_consumers_2 <- read.csv("data/comm_matrix/cdr_biodiv_2_consumers_wide_sub.csv")
cdr_producers_2 <- read.csv("data/comm_matrix/cdr_biodiv_2_producers_wide_sub.csv")
cdr_consumers_4 <- read.csv("data/comm_matrix/cdr_biodiv_4_consumers_wide_sub.csv")
cdr_producers_4 <- read.csv("data/comm_matrix/cdr_biodiv_4_producers_wide_sub.csv")
cdr_consumers_8 <- read.csv("data/comm_matrix/cdr_biodiv_8_consumers_wide_sub.csv")
cdr_producers_8 <- read.csv("data/comm_matrix/cdr_biodiv_8_producers_wide_sub.csv")
cdr_consumers_16 <- read.csv("data/comm_matrix/cdr_biodiv_16_consumers_wide_sub.csv")
cdr_producers_16 <- read.csv("data/comm_matrix/cdr_biodiv_16_producers_wide_sub.csv")

cdr_consumers <- read.csv("data/comm_matrix/cdr_of_consumers_wide_sub.csv")
cdr_producers <- read.csv("data/comm_matrix/cdr_of_producers_wide_sub.csv")
gce_consumers <- read.csv("data/comm_matrix/gce_consumers_wide_sub.csv")
gce_producers <- read.csv("data/comm_matrix/gce_producers_wide_sub.csv")
kbs_consumers <- read.csv("data/comm_matrix/kbs_consumers_wide_sub.csv")
kbs_producers <- read.csv("data/comm_matrix/kbs_producers_wide_sub.csv")
knz_consumers <- read.csv("data/comm_matrix/knz_consumers_wide_sub.csv")
knz_producers <- read.csv("data/comm_matrix/knz_producers_wide_sub.csv")

mcr_fish_consumers <- read.csv("data/comm_matrix/mcr_fish_consumers_wide_sub.csv")
mcr_fish_producers <- read.csv("data/comm_matrix/mcr_fish_producers_wide_sub.csv")
mcr_invert_consumers <- read.csv("data/comm_matrix/mcr_invert_consumers_wide_sub.csv")
mcr_invert_producers <- read.csv("data/comm_matrix/mcr_invert_producers_wide_sub.csv")

sbc_fish_consumers <- read.csv("data/comm_matrix/sbc_fish_consumers_wide_sub.csv")
sbc_fish_producers <- read.csv("data/comm_matrix/sbc_fish_producers_wide_sub.csv")
sbc_invert_consumers <- read.csv("data/comm_matrix/sbc_invert_consumers_wide_sub.csv")
sbc_invert_producers <- read.csv("data/comm_matrix/sbc_invert_producers_wide_sub.csv")

usvi_fish_consumers <- read.csv("data/comm_matrix/usvi_fish_consumers_wide_sub.csv")
usvi_fish_producers <- read.csv("data/comm_matrix/usvi_fish_producers_wide_sub.csv")
usvi_invert_consumers <- read.csv("data/comm_matrix/usvi_invert_consumers_wide_sub.csv")
usvi_invert_producers <- read.csv("data/comm_matrix/usvi_invert_producers_wide_sub.csv")

# Function to extract species matrix (everything after "scale_abundance")
extract_species_matrix <- function(df) {
  species_start <- which(names(df) == "scale_abundance") + 1
  df_species <- df[, species_start:ncol(df)]
  return(df_species)
}

# Function to generate and save trajectory plot
generate_cta_plot <- function(full_df, matrix_name) {
  species_matrix <- extract_species_matrix(full_df)
  D <- vegan::vegdist(species_matrix, method = "bray")
  
  metadata <- full_df %>%
    select(year, plot) %>%
    mutate(vector_nums = year - min(year) + 1)
  
  site_id <- sub("_.*", "", matrix_name)
  output_dir <- file.path("figures", "CTA", toupper(site_id))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(output_dir, paste0(matrix_name, "_trajectory_plot.png"))
  
  png(filename = output_file, width = 12, height = 8, res = 300, units = "in")
  trajectoryPCoA(D,
                 sites = metadata$plot,
                 surveys = metadata$vector_nums,
                 survey.labels = TRUE,
                 lwd = 1)
  dev.off()
}

# All matrices in one list
full_matrices <- list(
  cdr_consumers_1 = cdr_consumers_1,
  cdr_producers_1 = cdr_producers_1,
  cdr_consumers_2 = cdr_consumers_2,
  cdr_producers_2 = cdr_producers_2,
  cdr_consumers_4 = cdr_consumers_4,
  cdr_producers_4 = cdr_producers_4,
  cdr_consumers_8 = cdr_consumers_8,
  cdr_producers_8 = cdr_producers_8,
  cdr_consumers_16 = cdr_consumers_16,
  cdr_producers_16 = cdr_producers_16,
  cdr_consumers = cdr_consumers,
  cdr_producers = cdr_producers,
  # gce_consumers = gce_consumers,
  # gce_producers = gce_producers,
  kbs_consumers = kbs_consumers,
  kbs_producers = kbs_producers,
  knz_consumers = knz_consumers,
  knz_producers = knz_producers,
  mcr_fish_consumers = mcr_fish_consumers,
  mcr_fish_producers = mcr_fish_producers,
  # mcr_invert_consumers = mcr_invert_consumers,
  # mcr_invert_producers = mcr_invert_producers,
  sbc_fish_consumers = sbc_fish_consumers,
  # sbc_fish_producers = sbc_fish_producers,
  sbc_invert_consumers = sbc_invert_consumers,
  # sbc_invert_producers = sbc_invert_producers,
  usvi_fish_consumers = usvi_fish_consumers,
  usvi_fish_producers = usvi_fish_producers)
  # usvi_invert_consumers = usvi_invert_consumers,
  # usvi_invert_producers = usvi_invert_producers


# Loop through all matrices and generate plots
for (matrix_name in names(full_matrices)) {
  cat("Processing:", matrix_name, "\n")
  generate_cta_plot(full_df = full_matrices[[matrix_name]],
                    matrix_name = matrix_name)
}




# All matrices in one list
full_matrices <- list(
  cdr_consumers_1 = cdr_consumers_1,
  cdr_producers_1 = cdr_producers_1,
  cdr_consumers_2 = cdr_consumers_2,
  cdr_producers_2 = cdr_producers_2,
  cdr_consumers_4 = cdr_consumers_4,
  cdr_producers_4 = cdr_producers_4,
  cdr_consumers_8 = cdr_consumers_8,
  cdr_producers_8 = cdr_producers_8,
  cdr_consumers_16 = cdr_consumers_16,
  cdr_producers_16 = cdr_producers_16,
  cdr_consumers = cdr_consumers,
  cdr_producers = cdr_producers,
  # gce_consumers = gce_consumers,
  # gce_producers = gce_producers,
  kbs_consumers = kbs_consumers,
  kbs_producers = kbs_producers,
  knz_consumers = knz_consumers,
  knz_producers = knz_producers,
  mcr_fish_consumers = mcr_fish_consumers,
  mcr_fish_producers = mcr_fish_producers,
  # mcr_invert_consumers = mcr_invert_consumers,
  # mcr_invert_producers = mcr_invert_producers,
  sbc_fish_consumers = sbc_fish_consumers,
  # sbc_fish_producers = sbc_fish_producers,
  sbc_invert_consumers = sbc_invert_consumers,
  # sbc_invert_producers = sbc_invert_producers,
  usvi_fish_consumers = usvi_fish_consumers,
  usvi_fish_producers = usvi_fish_producers)
# usvi_invert_consumers = usvi_invert_consumers,
# usvi_invert_producers = usvi_invert_producers