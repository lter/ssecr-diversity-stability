# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: May 13th, 2025

# Load Libraries ----
library(tidyverse)
library(vegan)
library(ecotraj)
library(RColorBrewer)
library(here)

# CTA Functions ----

# Prep function: creates community matrix + metadata
prepare_cta_matrix <- function(df) {
  df <- df %>%
    mutate(site.year = paste(plot, year, sep = "-"))
  
  # Define metadata columns to drop if they exist
  metadata_cols <- c("site", "ecosystem", "habitat_broad", "habitat_fine",
                     "guild", "scale_abundance", "biome", "plot", "year", "unit_abundance")
  
  # Select only species abundance data
  comm_matrix <- df %>%
    column_to_rownames(var = "site.year") %>%
    select(where(is.numeric)) %>%
    select(-any_of(metadata_cols))  # Safely drop only those that exist
  
  nmds <- metaMDS(comm_matrix)
  nmds_coords <- data.frame(nmds$points, site.year = rownames(comm_matrix))
  
  df_joined <- df %>%
    left_join(nmds_coords, by = "site.year") %>%
    mutate(vector_nums = year - min(year) + 1)
  
  list(community_matrix = comm_matrix, metadata = df_joined)
}

# Color palette generator for unique plots
generate_custom_palette <- function(sites) {
  num_sites <- length(unique(sites))
  colors <- colorRampPalette(brewer.pal(min(12, num_sites), "Set3"))(num_sites)
  names(colors) <- unique(sites)
  colors
}

# Run and save CTA plot
run_cta_plot <- function(D, metadata, palette, output_path) {
  png(filename = output_path, width = 12, height = 8, res = 300, units = "in")
  trajectoryPCoA(
    D,
    sites = metadata$plot,
    surveys = metadata$vector_nums,
    traj.colors = palette,  
    lwd = 1,
    survey.labels = TRUE
  )
  legend("topright", legend = names(palette), col = palette, lwd = 2, bty = "n", cex = 0.8)
  dev.off()
}

# Extract and save trajectory length metrics
extract_cta_metrics <- function(D, metadata, site_id, output_path) {
  traj_lengths <- trajectoryLengths(D, sites = metadata$plot, surveys = metadata$vector_nums)
  traj_lengths <- as.data.frame(traj_lengths)
  traj_lengths$mean_length <- rowMeans(traj_lengths, na.rm = TRUE)
  traj_lengths$plot <- rownames(traj_lengths)
  traj_lengths$site <- site_id
  write.csv(traj_lengths, output_path, row.names = FALSE)
}

# Complete pipeline runner
run_cta_pipeline <- function(df, site_id, fig_path, table_path) {
  prep <- prepare_cta_matrix(df)
  D <- vegan::vegdist(prep$community_matrix, "bray")
  palette <- generate_custom_palette(prep$metadata$plot)
  run_cta_plot(D, prep$metadata, palette, fig_path)
  extract_cta_metrics(D, prep$metadata, site_id, table_path)
}

# Load Harmonized Data ----

# KNZ #
knz_prod_wide <- read.csv(here::here("data/KNZ", "knz_producers_wide_sub.csv"))
knz_con_wide <- read.csv(here::here("data/KNZ", "knz_consumers_wide_sub.csv"))

# KBS #
kbs_prod_wide <- read.csv(here::here("data/KBS", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/KBS", "kbs_consumers_wide_sub.csv"))

# CDR OLD FIELD #
cdr_of_prod_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_consumers_wide_sub.csv"))

# CDR BIODIVERSITY #
# numbers describe the initial diversity of producers in each plot
cdr_biodiv_1_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_1_producers_wide_sub.csv"))
cdr_biodiv_1_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_1_consumers_wide_sub.csv"))
cdr_biodiv_2_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_2_producers_wide_sub.csv"))
cdr_biodiv_2_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_2_consumers_wide_sub.csv"))
cdr_biodiv_4_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_4_producers_wide_sub.csv"))
cdr_biodiv_4_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_4_consumers_wide_sub.csv"))
cdr_biodiv_8_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_8_producers_wide_sub.csv"))
cdr_biodiv_8_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_8_consumers_wide_sub.csv"))
cdr_biodiv_16_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_16_producers_wide_sub.csv"))
cdr_biodiv_16_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_16_consumers_wide_sub.csv"))

# GCE #
gce_prod_wide <- read.csv(here::here("data/GCE", "gce_producers_wide_sub.csv"))
gce_con_wide <- read.csv(here::here("data/GCE", "gce_consumers_wide_sub.csv"))

#USVI #
usvi_fish_prod_wide <- read.csv(here::here("data/USVI", "usvi_fish_producers_wide_sub.csv"))
usvi_fish_con_wide <- read.csv(here::here("data/USVI", "usvi_fish_consumers_wide_sub.csv"))
usvi_invert_prod_wide <- read.csv(here::here("data/USVI", "usvi_invert_producers_wide_sub.csv"))
usvi_invert_con_wide <- read.csv(here::here("data/USVI", "usvi_invert_consumers_wide_sub.csv"))

#SBC #
sbc_invert_prod_wide <- read.csv(here::here("data/SBC", "sbc_invert_producers_wide_sub.csv"))
sbc_invert_wide <- read.csv(here::here("data/SBC", "sbc_invert_consumers_wide_sub.csv"))
sbc_fish_prod_wide <- read.csv(here::here("data/SBC", "sbc_fish_producers_wide_sub.csv"))
sbc_fish_wide <- read.csv(here::here("data/SBC", "sbc_fish_consumers_wide_sub.csv"))

# MCR #
mcr_invert_prod_wide <- read.csv(here::here("data/MCR", "mcr_invert_producers_wide_sub.csv"))
mcr_invert_wide <- read.csv(here::here("data/MCR", "mcr_invert_consumers_wide_sub.csv"))
mcr_fish_prod_wide <- read.csv(here::here("data/MCR", "mcr_fish_producers_wide_sub.csv"))
mcr_fish_wide <- read.csv(here::here("data/MCR", "mcr_fish_consumers_wide_sub.csv"))


# Run CTA Pipeline ----

#KNZ Producers
run_cta_pipeline(
  df = knz_prod_wide,
  site_id = "knz",
  fig_path = here("figures/CTA/KNZ", "knz_producer_trajectory_plot.png"),
  table_path = here("tables/KNZ/producer", "knz_producer_lengths.csv"))

#KNZ Consumers
run_cta_pipeline(
  df = knz_con_wide,
  site_id = "knz",
  fig_path = here("figures/CTA/KNZ", "knz_consumer_trajectory_plot.png"),
  table_path = here("tables/KNZ/consumer", "knz_consumer_lengths.csv"))

#KBS Producers
run_cta_pipeline(
  df = kbs_prod_wide,
  site_id = "kbs",
  fig_path = here("figures/CTA/KBS", "kbs_producer_trajectory_plot.png"),
  table_path = here("tables/KBS/producer", "kbs_producer_lengths.csv"))

#KBS Consumers
run_cta_pipeline(
  df = kbs_con_wide,
  site_id = "kbs",
  fig_path = here("figures/CTA/KBS", "kbs_consumer_trajectory_plot.png"),
  table_path = here("tables/KBS/consumer", "kbs_consumer_lengths.csv"))

#CDR Oldfield Producers
run_cta_pipeline(
  df = cdr_of_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_of_producer_trajectory_plot.png"),
  table_path = here("tables/CDR/producer", "cdr_of_producer_lengths.csv"))

#CDR Oldfield Consumer
run_cta_pipeline(
  df = cdr_of_con_wide,
  site_id = "cdr_of",
  fig_path = here("figures/CTA/CDR", "cdr_of_consumer_trajectory_plot.png"),
  table_path = here("tables/CDR/consumer", "cdr_of_consumer_lengths.csv"))

#CDR Biodiv 1 Producers
run_cta_pipeline(
  df = cdr_biodiv_1_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_1_producer_trajectory_plot.png"),
  table_path = here("tables/CDR/producer", "cdr_biodiv_1_producer_lengths.csv"))

#CDR Biodiv 1 Consumers
run_cta_pipeline(
  df = cdr_biodiv_1_con_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_1_consumer_trajectory_plot.png"),
  table_path = here("tables/CDR/consumer", "cdr_biodiv_1_consumer_lengths.csv"))

#CDR Biodiv 2 Producers
run_cta_pipeline(
  df = cdr_biodiv_2_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_2_producer_trajectory_plot.png"),
  table_path = here("tables/CDR/producer", "cdr_biodiv_2_producer_lengths.csv"))

#CDR Biodiv 2 Consumers
run_cta_pipeline(
  df = cdr_biodiv_2_con_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_2_consumer_trajectory_plot.png"),
  table_path = here("tables/CDR/consumer", "cdr_biodiv_2_consumer_lengths.csv"))

#CDR Biodiv 4 Producers
run_cta_pipeline(
  df = cdr_biodiv_4_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_4_producer_trajectory_plot.png"),
  table_path = here("tables/CDR/producer", "cdr_biodiv_4_producer_lengths.csv"))

#CDR Biodiv 4 Consumers
run_cta_pipeline(
  df = cdr_biodiv_4_con_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_4_consumer_trajectory_plot.png"),
  table_path = here("tables/CDR/consumer", "cdr_biodiv_4_consumer_lengths.csv"))

#CDR Biodiv 8 Producers
run_cta_pipeline(
  df = cdr_biodiv_8_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_8_producer_trajectory_plot.png"),
  table_path = here("tables/CDR/producer", "cdr_biodiv_8_producer_lengths.csv"))

#CDR Biodiv 8 Consumers
run_cta_pipeline(
  df = cdr_biodiv_8_con_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_8_consumer_trajectory_plot.png"),
  table_path = here("tables/CDR/consumer", "cdr_biodiv_8_consumer_lengths.csv"))

#CDR Biodiv 16 Producers
run_cta_pipeline(
  df = cdr_biodiv_16_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_16_producer_trajectory_plot.png"),
  table_path = here("tables/CDR/producer", "cdr_biodiv_16_producer_lengths.csv"))

#CDR Biodiv 16 Consumers
run_cta_pipeline(
  df = cdr_biodiv_16_con_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/CDR", "cdr_biodiv_16_consumer_trajectory_plot.png"),
  table_path = here("tables/CDR/consumer", "cdr_biodiv_16_consumer_lengths.csv"))

# #GCE Producers
# run_cta_pipeline(
#   df = gce_prod_wide,
#   site_id = "gce",
#   fig_path = here("figures/CTA/GCE", "gce_producer_trajectory_plot.png"),
#   table_path = here("tables/GCE/producer", "gce_producer_lengths.csv"))
# 
# #GCE Consumers
# run_cta_pipeline(
#   df = gce_con_wide,
#   site_id = "gce",
#   fig_path = here("figures/CTA/GCE", "gce_consumer_trajectory_plot.png"),
#   table_path = here("tables/GCE/consumer", "gce_consumer_lengths.csv"))


#USVI Fish Producers
run_cta_pipeline(
  df = usvi_fish_prod_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/USVI", "usvi_fish_producer_trajectory_plot.png"),
  table_path = here("tables/USVI/producer", "usvi_fish_producer_lengths.csv"))

#USVI Fish Consumers
run_cta_pipeline(
  df = usvi_fish_con_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/USVI", "usvi_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/USVI/consumer", "usvi_fish_consumer_lengths.csv"))

#USVI Invert Producers
run_cta_pipeline(
  df = usvi_invert_prod_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/USVI", "usvi_fish_producer_trajectory_plot.png"),
  table_path = here("tables/USVI/producer", "usvi_fish_producer_lengths.csv"))

#USVI Invert Consumers
run_cta_pipeline(
  df = usvi_invert_con_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/USVI", "usvi_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/USVI/consumer", "usvi_fish_consumer_lengths.csv"))


#SBC Fish Producers
# run_cta_pipeline(
#   df = sbc_fish_prod_wide,
#   site_id = "sbc",
#   fig_path = here("figures/CTA/SBC", "sbc_fish_producer_trajectory_plot.png"),
#   table_path = here("tables/SBC/producer", "sbc_fish_producer_lengths.csv"))

#SBC Fish Consumers
run_cta_pipeline(
  df = sbc_fish_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/SBC", "sbc_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/SBC/consumer", "sbc_fish_consumer_lengths.csv"))

#SBC Invert Producers
# run_cta_pipeline(
#   df = sbc_invert_prod_wide,
#   site_id = "sbc",
#   fig_path = here("figures/CTA/SBC", "sbc_fish_producer_trajectory_plot.png"),
#   table_path = here("tables/SBC/producer", "sbc_fish_producer_lengths.csv"))

#SBC Invert Consumers
run_cta_pipeline(
  df = sbc_invert_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/SBC", "sbc_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/SBC/consumer", "sbc_fish_consumer_lengths.csv"))


#MCR Fish Producers
run_cta_pipeline(
  df = mcr_fish_prod_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/MCR", "mcr_fish_producer_trajectory_plot.png"),
  table_path = here("tables/MCR/producer", "mcr_fish_producer_lengths.csv"))

#MCR Fish Consumers
run_cta_pipeline(
  df = mcr_fish_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/MCR", "mcr_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/MCR/consumer", "mcr_fish_consumer_lengths.csv"))

#MCR Invert Producers
run_cta_pipeline(
  df = mcr_invert_prod_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/MCR", "mcr_fish_producer_trajectory_plot.png"),
  table_path = here("tables/MCR/producer", "mcr_fish_producer_lengths.csv"))

# #MCR Invert Consumers
# run_cta_pipeline(
#   df = mcr_invert_wide,
#   site_id = "mcr",
#   fig_path = here("figures/CTA/MCR", "mcr_invert_consumer_trajectory_plot.png"),
#   table_path = here("tables/MCR/consumer", "mcr_invert_consumer_lengths.csv"))
