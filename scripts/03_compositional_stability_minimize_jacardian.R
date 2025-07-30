# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: July 28th, 2025

# ──────────────────────────────────────────────────────────────────────────────
# Presence–absence CTA pipeline (Jaccardian distance)
# ──────────────────────────────────────────────────────────────────────────────

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
    select(-any_of(metadata_cols))
  
  # Convert to presence-absence for Jaccard (important!)
  comm_matrix[comm_matrix > 0] <- 1
  
  # Run NMDS with Jaccard
  nmds <- metaMDS(comm_matrix, distance = "jaccard", binary = TRUE, trymax = 100)
  nmds_coords <- data.frame(nmds$points, site.year = rownames(comm_matrix))
  
  # Join coordinates back with metadata
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
  traj_lengths_df <- as.data.frame(traj_lengths)
  
  traj_lengths_df$total_trajectory <- traj_lengths_df$Trajectory
  segment_cols <- grep("^S\\d+", names(traj_lengths_df), value = TRUE)
  traj_lengths_df$mean_segment_length <- rowMeans(traj_lengths_df[, segment_cols], na.rm = TRUE)
  
  traj_lengths_df$plot <- rownames(traj_lengths_df)
  traj_lengths_df$site <- site_id
  
  traj_lengths_df <- traj_lengths_df[, c("plot", "site", segment_cols, "Trajectory", "total_trajectory", "mean_segment_length")]
  
  write.csv(traj_lengths_df, output_path, row.names = FALSE)
}

# Complete pipeline runner (uses jaccard instead of bray)
run_cta_pipeline <- function(df, site_id, fig_path, table_path) {
  prep <- prepare_cta_matrix(df)
  D <- vegan::vegdist(prep$community_matrix, method = "jaccard", binary = TRUE)
  palette <- generate_custom_palette(prep$metadata$plot)
  run_cta_plot(D, prep$metadata, palette, fig_path)
  extract_cta_metrics(D, prep$metadata, site_id, table_path)
}
# Load Harmonized Data ----
# KNZ #
knz_prod_wide <- read.csv(here::here("data/wide_output_minimize", "knz_producers_wide_sub.csv"))
knz_con_wide <- read.csv(here::here("data/wide_output_minimize", "knz_consumers_wide_sub.csv"))

# AIMS #
aims_con_wide <- read.csv(here::here("data/wide_output_minimize", "aims_consumers_wide_sub.csv"))
aims_prod_wide <- read.csv(here::here("data/wide_output_minimize", "aims_producers_wide_sub.csv"))

# KBS #
kbs_prod_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/wide_output_minimize", "kbs_consumers_wide_sub.csv"))

# CDR OLD FIELD #
cdr_of_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_of_consumers_wide_sub.csv"))

# # CDR BIODIVERSITY #
# # numbers describe the initial diversity of producers in each plot
# cdr_biodiv_1_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_1_producers_wide_sub.csv"))
# cdr_biodiv_1_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_1_consumers_wide_sub.csv"))
# cdr_biodiv_2_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_2_producers_wide_sub.csv"))
# cdr_biodiv_2_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_2_consumers_wide_sub.csv"))
# cdr_biodiv_4_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_4_producers_wide_sub.csv"))
# cdr_biodiv_4_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_4_consumers_wide_sub.csv"))
# cdr_biodiv_8_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_8_producers_wide_sub.csv"))
# cdr_biodiv_8_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_8_consumers_wide_sub.csv"))
# cdr_biodiv_16_prod_wide <- read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_16_producers_wide_sub.csv"))
# cdr_biodiv_16_con_wide <-  read.csv(here::here("data/wide_output_minimize", "cdr_biodiv_16_consumers_wide_sub.csv"))

# # GCE #
# gce_prod_wide <- read.csv(here::here("data/wide_output_minimize", "gce_producers_wide_sub.csv"))
# gce_con_wide <- read.csv(here::here("data/wide_output_minimize", "gce_consumers_wide_sub.csv"))

#USVI #
usvi_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_fish_producers_wide_sub.csv"))
usvi_fish_con_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_fish_consumers_wide_sub.csv"))
usvi_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_invert_producers_wide_sub.csv"))
usvi_invert_con_wide <- read.csv(here::here("data/wide_output_minimize", "usvi_invert_consumers_wide_sub.csv"))

#SBC #
sbc_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_producers_wide_sub.csv"))
sbc_invert_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_invert_consumers_wide_sub.csv"))
sbc_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_producers_wide_sub.csv"))
sbc_fish_wide <- read.csv(here::here("data/wide_output_minimize", "sbc_fish_consumers_wide_sub.csv"))

# MCR #
mcr_invert_prod_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_invert_producers_wide_sub.csv"))
mcr_invert_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_invert_consumers_wide_sub.csv"))
mcr_fish_prod_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_fish_producers_wide_sub.csv"))
mcr_fish_wide <- read.csv(here::here("data/wide_output_minimize", "mcr_fish_consumers_wide_sub.csv"))


# Filtering Complete 0's ----

mcr_invert_wide = mcr_invert_wide |>
  filter(year != 2024)

mcr_invert_prod_wide = mcr_invert_prod_wide |>
  filter(year != 2024)

mcr_fish_wide = mcr_fish_wide |>
  filter(year != 2024)

mcr_fish_prod_wide = mcr_fish_prod_wide |>
  filter(year != 2024)

sbc_fish_wide = sbc_fish_wide |> 
  filter(plot != "AHND")

sbc_fish_prod_wide = sbc_fish_prod_wide |> 
  filter(plot != "AHND")

sbc_invert_wide = sbc_invert_wide |> 
  filter(plot != "AHND")

sbc_invert_prod_wide = sbc_invert_prod_wide |> 
  filter(plot != "AHND")

# Run CTA Pipeline ----

#KNZ Producers
run_cta_pipeline(
  df = knz_prod_wide,
  site_id = "knz",
  fig_path = here("figures/CTA/wide_output_jaccard", "knz_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "knz_producer_lengths_jaccard.csv"))

#KNZ Consumers
run_cta_pipeline(
  df = knz_con_wide,
  site_id = "knz",
  fig_path = here("figures/CTA/wide_output_jaccard", "knz_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "knz_consumer_lengths_jaccard.csv"))


#AIMS Producers
run_cta_pipeline(
  df = aims_prod_wide,
  site_id = "aims",
  fig_path = here("figures/CTA/wide_output_jaccard", "aims_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "aims_producer_lengths_jaccard.csv"))

#AIMS Consumers
run_cta_pipeline(
  df = aims_con_wide,
  site_id = "aims",
  fig_path = here("figures/CTA/wide_output_jaccard", "aims_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "aims_consumer_lengths_jaccard.csv"))


#KBS Producers
run_cta_pipeline(
  df = kbs_prod_wide,
  site_id = "kbs",
  fig_path = here("figures/CTA/wide_output_jaccard", "kbs_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "kbs_producer_lengths_jaccard.csv"))

#KBS Consumers
run_cta_pipeline(
  df = kbs_con_wide,
  site_id = "kbs",
  fig_path = here("figures/CTA/wide_output_jaccard", "kbs_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "kbs_consumer_lengths_jaccard.csv"))

#CDR Oldfield Producers
run_cta_pipeline(
  df = cdr_of_prod_wide,
  site_id = "CDR",
  fig_path = here("figures/CTA/wide_output_jaccard", "cdr_of_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "cdr_of_producer_lengths_jaccard.csv"))

#CDR Oldfield Consumer
run_cta_pipeline(
  df = cdr_of_con_wide,
  site_id = "cdr_of",
  fig_path = here("figures/CTA/wide_output_jaccard", "cdr_of_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "cdr_of_consumer_lengths_jaccard.csv"))

# #CDR Biodiv 1 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_1_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_1_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_1_producer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 1 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_1_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_1_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_1_consumer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 2 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_2_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_2_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_2_producer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 2 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_2_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_2_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_2_consumer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 4 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_4_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_4_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_4_producer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 4 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_4_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_4_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_4_consumer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 8 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_8_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_8_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_8_producer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 8 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_8_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_8_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_8_consumer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 16 Producers
# run_cta_pipeline(
#   df = cdr_biodiv_16_prod_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_16_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "cdr_biodiv_16_producer_lengths_jaccard.csv"))
# 
# #CDR Biodiv 16 Consumers
# run_cta_pipeline(
#   df = cdr_biodiv_16_con_wide,
#   site_id = "CDR",
#   fig_path = here("figures/CTA/wide_output_minimize", "cdr_biodiv_16_consumer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/consumer", "cdr_biodiv_16_consumer_lengths_jaccard.csv"))

#GCE Producers
# run_cta_pipeline(
#   df = gce_prod_wide,
#   site_id = "gce",
#   fig_path = here("figures/CTA/wide_output_minimize", "gce_producer_trajectory_plot.png"),
#   table_path = here("tables/wide_output_minimize/producer", "gce_producer_lengths_jaccard.csv"))
# # 
# #GCE Consumers
# run_cta_pipeline(
#   df = gce_con_wide,
#   site_id = "gce",
#   fig_path = here("figures/CTA/GCE", "gce_consumer_trajectory_plot.png"),
#   table_path = here("tables/GCE/consumer", "gce_consumer_lengths_jaccard.csv"))


#USVI Fish Producers
run_cta_pipeline(
  df = usvi_fish_prod_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_jaccard", "usvi_fish_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "usvi_fish_producer_lengths_jaccard.csv"))

#USVI Fish Consumers
run_cta_pipeline(
  df = usvi_fish_con_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_jaccard", "usvi_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "usvi_fish_consumer_lengths_jaccard.csv"))

#USVI Invert Producers
run_cta_pipeline(
  df = usvi_invert_prod_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_jaccard", "usvi_invert_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "usvi_invert_producer_lengths_jaccard.csv"))

#USVI Invert Consumers
run_cta_pipeline(
  df = usvi_invert_con_wide,
  site_id = "usvi",
  fig_path = here("figures/CTA/wide_output_jaccard", "usvi_invert_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "usvi_invert_consumer_lengths_jaccard.csv"))


# SBC Fish Producers
run_cta_pipeline(
  df = sbc_fish_prod_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_jaccard", "sbc_fish_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "sbc_fish_producer_lengths_jaccard.csv"))

#SBC Fish Consumers
run_cta_pipeline(
  df = sbc_fish_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_jaccard", "sbc_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "sbc_fish_consumer_lengths_jaccard.csv"))

#SBC Invert Producers
run_cta_pipeline(
  df = sbc_invert_prod_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_jaccard", "sbc_invert_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "sbc_invert_producer_lengths_jaccard.csv"))

#SBC Invert Consumers
run_cta_pipeline(
  df = sbc_invert_wide,
  site_id = "sbc",
  fig_path = here("figures/CTA/wide_output_jaccard", "sbc_invert_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "sbc_invert_consumer_lengths_jaccard.csv"))


#MCR Fish Producers
run_cta_pipeline(
  df = mcr_fish_prod_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_jaccard", "mcr_fish_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "mcr_fish_producer_lengths_jaccard.csv"))

#MCR Fish Consumers
run_cta_pipeline(
  df = mcr_fish_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_jaccard", "mcr_fish_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "mcr_fish_consumer_lengths_jaccard.csv"))

#MCR Invert Producers
run_cta_pipeline(
  df = mcr_invert_prod_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_jaccard", "mcr_invert_producer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/producer", "mcr_invert_producer_lengths_jaccard.csv"))

# #MCR Invert Consumers
run_cta_pipeline(
  df = mcr_invert_wide,
  site_id = "mcr",
  fig_path = here("figures/CTA/wide_output_jaccard", "mcr_invert_consumer_trajectory_plot.png"),
  table_path = here("tables/wide_output_jaccard/consumer", "mcr_invert_consumer_lengths_jaccard.csv"))
