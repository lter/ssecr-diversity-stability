# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: March 6th, 2025

# Raw Data Availability ----

# install.packages(librarian)
librarian::shelf(tidyverse, googledrive, data.table, ecotraj, vegan, lterdatasampler, supportR, cowplot, summarytools, datacleanr)

# read data directly from EDI repo
# data_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.437.2&entityid=d9c5d73d11fc6a16ec41139b63b27751"
# data_long <- read.csv(file = data_url) # should be 217512 obs, 11 var
sbc_all = read.csv(file = "data/SBC/sbc_all.csv")

unique(sbc_all$guild)

sbc_producer = sbc_all %>% 
  filter(guild == "algae")

sbc_fish = sbc_all %>% 
  filter(guild == "fish")

sbc_invert = sbc_all %>% 
  filter(guild == "invert")

sbc_producer = sbc_producer %>% 
  filter(!taxon_name %in% "Agardhiella subulata")
# Richness Summary Tables ----

# Summary data for consumer species richness across sites
species_n_producer_site <- sbc_producer %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_producer_site
write.csv(species_n_producer_site, "tables/SBC/summary/species_n_producer_site.csv",row.names = F)

species_n_invert_site <- sbc_invert %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_invert_site
write.csv(species_n_invert_site, "tables/SBC/summary/species_n_invert_site.csv",row.names = F)

# Summary data for fish species richness across sites
species_n_fish_site <- sbc_fish %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_fish_site
write.csv(species_n_fish_site, "tables/SBC/summary/species_n_fish_site.csv",row.names = F)

# Site Selection & Order ----
# remove 4 sites with less than 10 years of data
site_order = c("GOLB", "IVEE", "AQUE", "CARP", "NAPL", "AHND", "BULL", "SCDI", "SCTW", "ABUR", "MOHK")


# #only keep 15 producer sites that match consumer data
# sbc_producers <- sbc_producers %>%
#   filter(plot == site_order)
# 
# sbc_producers <- sbc_producers %>%
#   mutate(plot = factor(plot, levels = site_order)) %>%  # Ensure plot matches site_order
#   arrange(match(plot, site_order))

# Survey History Summary Tables ----

# Summary data years of consumer assemblage data per site
site_years_count_producer <- sbc_producer %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_producer
write.csv(site_years_count_producer, "tables/SBC/summary/site_years_count_producer.csv",row.names = F)

# Summary data years of producer assemblage data per site
site_years_count_invert <- sbc_invert %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_invert
write.csv(site_years_count_invert, "tables/SBC/summary/site_years_count_invert.csv",row.names = F)


site_years_count_fish <- sbc_fish %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_fish
write.csv(site_years_count_fish, "tables/SBC/summary/site_years_count_fish.csv",row.names = F)

# Summary data total number of observations of each consumer species
# and the number of plots that species has been observed in at least once
site_fish_spp_summary <- sbc_fish %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_fish_spp_summary
write.csv(site_fish_spp_summary, "tables/SBC/summary/site_fish_spp_summary.csv",row.names = F)


site_invert_spp_summary <- sbc_invert %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_invert_spp_summary
write.csv(site_invert_spp_summary, "tables/SBC/summary/site_invert_spp_summary.csv",row.names = F)

site_producer_spp_summary <- sbc_producer %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_producer_spp_summary
write.csv(site_producer_spp_summary, "tables/SBC/summary/site_producer_spp_summary.csv",row.names = F)



sbc_fish_means <- sbc_fish %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

sbc_invert_means <- sbc_invert %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

sbc_producer_means <- sbc_producer %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))





sbc_fish_means <- sbc_fish_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

sbc_invert_means <- sbc_invert_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

sbc_producer_means <- sbc_producer_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))


sbc_fish_means = sbc_fish_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

sbc_invert_means = sbc_invert_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

sbc_producer_means = sbc_producer_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

sbc_fish_matrix = sbc_fish_means %>% 
  column_to_rownames(var = "site.year")

sbc_invert_matrix = sbc_invert_means %>% 
  column_to_rownames(var = "site.year")

sbc_producer_matrix = sbc_producer_means %>% 
  column_to_rownames(var = "site.year")

sbc_fish_matrix = sbc_fish_matrix %>%
  dplyr::select(-plot, -year)

sbc_invert_matrix = sbc_invert_matrix %>%
  dplyr::select(-plot, -year)

sbc_producer_matrix = sbc_producer_matrix %>%
  dplyr::select(-plot, -year)

# SBC NMDS ----

#doing this as a combined coral/algae community and as just coral or alage cover
nmds_fish <- metaMDS(sbc_fish_matrix)
nmds_invert <- metaMDS(sbc_invert_matrix)

# Identify rows (sites) where all values are 0
zero_sites <- rowSums(sbc_producer_matrix) == 0

# Print the site names with all-zero rows
print(rownames(sbc_producer_matrix)[zero_sites])

# Remove rows with all zeros
sbc_producer_matrix2 <- sbc_producer_matrix[!zero_sites, ]

nmds_producer <- metaMDS(sbc_producer_matrix2)

# Extract NMDS coordinates
nmds_fish_coords <- nmds_fish$points
nmds_invert_coords <- nmds_invert$points
nmds_producer_coords <- nmds_producer$points

# Add site and year information to NMDS coordinates
nmds_fish_data <- data.frame(nmds_fish_coords, site.year = rownames(sbc_fish_matrix))
nmds_invert_data <- data.frame(nmds_invert_coords, site.year = rownames(sbc_invert_matrix))

nmds_prooducer_data <- data.frame(nmds_producer_coords, site.year = rownames(sbc_producer_matrix2))

sbc_fish_means = sbc_fish_means %>% 
  left_join(nmds_fish_data)

sbc_fish_means = sbc_fish_means %>% 
  mutate(site.year = paste0(plot,year))


sbc_invert_means = sbc_invert_means %>% 
  left_join(nmds_invert_data)

sbc_invert_means = sbc_invert_means %>% 
  mutate(site.year = paste0(plot,year))

sbc_producer_means = sbc_producer_means %>% 
  left_join(nmds_prooducer_data)

sbc_producer_means = sbc_producer_means %>% 
  mutate(site.year = paste0(plot,year))

# Create NMDS plots for... 

# consumers
plot_nmds_fish <- ggplot(sbc_fish_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = plot,
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "SBC Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_fish)

ggsave(filename = paste0("figures/NMDS/SBC", "/", "plot_nmds_fish", ".png"),
       plot = plot_nmds_fish, width = 8, height = 6, units = "in", dpi = 600)


# inverts
plot_nmds_invert <- ggplot(sbc_invert_means, aes(x = MDS1,
                                             y = MDS2,
                                             color = plot,
                                             label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "SBC Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_invert)

ggsave(filename = paste0("figures/NMDS/SBC", "/", "plot_nmds_invert", ".png"),
       plot = plot_nmds_invert, width = 8, height = 6, units = "in", dpi = 600)
# producers
plot_nmds_producer <- ggplot(sbc_producer_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = plot,
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "SBC Producer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_producer)

ggsave(filename = paste0("figures/NMDS/SBC", "/", "plot_nmds_producer", ".png"),
       plot = plot_nmds_producer, width = 8, height = 6, units = "in", dpi = 600)


# SBC CTA ----

#consumers
D <- vegan::vegdist(sbc_fish_matrix,"bray")

custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8"
) 

# Generate the plot using trajectoryPCoA

sbc_fish_means = sbc_fish_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("GOLB", "IVEE", "AQUE", "CARP", "NAPL", "AHND", "BULL", "SCDI", "SCTW", "ABUR", "MOHK"),  col = c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                                                                               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                                                                                                                                                                                               "#aec7e8"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/SBC/sbc_fish_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()


#inverts
Di <- vegan::vegdist(sbc_invert_matrix,"bray")

custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8"
) 

# Generate the plot using trajectoryPCoA

sbc_invert_means = sbc_invert_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(Di, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("GOLB", "IVEE", "AQUE", "CARP", "NAPL", "AHND", "BULL", "SCDI", "SCTW", "ABUR", "MOHK"),  col = c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                                                       "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                                                                                                                                                                       "#aec7e8"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/SBC/sbc_invert_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()



# producers
sbc_producer_means <- sbc_producer_means[!zero_sites, ]

Dd <- vegan::vegdist(sbc_producer_matrix2,"bray")

sbc_producer_means = sbc_producer_means %>% 
  mutate(vector_nums = year - min(year) + 1)

trajectoryPCoA(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("GOLB", "IVEE", "AQUE", "CARP", "NAPL", "AHND", "BULL", "SCDI", "SCTW", "ABUR", "MOHK"),  col = c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                                                                               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                                                                                                                                                                                               "#aec7e8"), lwd=2)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/SBC/sbc_producer_trajectory_plot.png", width = 13, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()



# Individual Sites ----

# Define a list of unique sites
unique_sites <- unique(sbc_fish$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- sbc_fish_means %>% 
    filter(plot == site)
  
  # Prepare the site data for distance matrix calculation
  site_data <- site_raw %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums)
  
  # Compute Bray-Curtis distance matrix
  D1 <- vegan::vegdist(site_data, "bray")
  
  # Calculate segment lengths using trajectoryLengths
  segment_lengths <- trajectoryLengths(D1, sites = site_raw$plot, surveys = site_raw$vector_nums)
  
  # Create a data frame for the segment lengths
  segment_lengths_df <- data.frame(
    Segment = seq_along(segment_lengths),
    Length = segment_lengths
  )
  
  # Save segment lengths as a CSV file
  csv_filename <- paste0("tables/SBC/sites/sbc_fish_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/SBC/sbc_fish_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(D1, sites = site_raw$plot, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}


# Define a list of unique sites
unique_sites_invert <- unique(sbc_invert$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- sbc_invert_means %>% 
    filter(plot == site)
  
  # Prepare the site data for distance matrix calculation
  site_data <- site_raw %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums)
  
  # Compute Bray-Curtis distance matrix
  D1 <- vegan::vegdist(site_data, "bray")
  
  # Calculate segment lengths using trajectoryLengths
  segment_lengths <- trajectoryLengths(D1, sites = site_raw$plot, surveys = site_raw$vector_nums)
  
  # Create a data frame for the segment lengths
  segment_lengths_df <- data.frame(
    Segment = seq_along(segment_lengths),
    Length = segment_lengths
  )
  
  # Save segment lengths as a CSV file
  csv_filename <- paste0("tables/SBC/sites/sbc_invert_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/SBC/sbc_invert_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(D1, sites = site_raw$plot, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}


#producers
unique_sites_producers <- unique(sbc_producer$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8"
)   # Extend this list as more sites are added

for (site in unique_sites_producers) {
  
  # Get the index of the site to select the corresponding color
  site_index <- which(unique_sites_producers == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw_p <- sbc_producer_means %>% 
    filter(plot == site)
  
  # Prepare the site data
  site_data <- site_raw_p %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums)
  
  # Compute Bray-Curtis distance matrix
  D2 <- vegan::vegdist(site_data, "bray")
  
  
  # Calculate segment lengths using trajectoryLengths
  segment_lengths <- trajectoryLengths(D2, sites = site_raw_p$plot, surveys = site_raw_p$vector_nums)
  
  # Create a data frame for the segment lengths
  segment_lengths_df <- data.frame(
    Segment = seq_along(segment_lengths),
    Length = segment_lengths
  )
  
  # Save segment lengths as a CSV file
  csv_filename <- paste0("tables/SBC/sites/sbc_producer_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Open the PNG file for saving
  filename <- paste0("figures/CTA/SBC/sbc_producer_trajectory_plot_", site, ".png")
  png(filename = filename, width = 8, height = 8, res = 300, units = "in")
  
  # Run the trajectoryPCoA function with the specific color for the site
  trajectoryPCoA(D2, sites = site_raw_p$plot, surveys = site_raw_p$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = T)
  
  # Close the PNG device to save the file
  dev.off()
}

# CTA Metrics ----

# Extract trajectory metrics for fish
# Calculate trajectory lengths (not relative to initial)
fish_lengths <- trajectoryLengths(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums)
fish_lengths <- as.data.frame(fish_lengths)
fish_lengths$site <- site_order  
write.csv(fish_lengths, "tables/SBC/fish/fish_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
fish_lengths_initial <- trajectoryLengths(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums, relativeToInitial = TRUE)
fish_lengths_initial <- as.data.frame(fish_lengths_initial)
fish_lengths_initial$site <- site_order
write.csv(fish_lengths_initial, "tables/SBC/fish/fish_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
fish_angles <- trajectoryAngles(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums)
fish_angles <- as.data.frame(fish_angles)
fish_angles$site <- site_order
write.csv(fish_angles, "tables/SBC/fish/fish_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
fish_angles_initial <- trajectoryAngles(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums, relativeToInitial = TRUE)
fish_angles_initial <- as.data.frame(fish_angles_initial)
fish_angles_initial$site <- site_order
write.csv(fish_angles_initial, "tables/SBC/fish/fish_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# fish_trajectory_distances <- trajectoryDistances(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums)
# fish_trajectory_distances <- as.data.frame(fish_trajectory_distances)
# fish_trajectory_distances$site <- site_order
# write.csv(fish_trajectory_distances, "tables/SBC/fish/fish_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
fish_convergence <- trajectoryConvergence(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums)
fish_convergence <- as.data.frame(fish_convergence)
fish_convergence$site <- site_order
write.csv(fish_convergence, "tables/SBC/fish/fish_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
fish_directionality <- trajectoryDirectionality(D, sites = sbc_fish_means$plot, surveys = sbc_fish_means$vector_nums)
fish_directionality <- as.data.frame(fish_directionality)
fish_directionality$site <- site_order
write.csv(fish_directionality, "tables/SBC/fish/fish_directionality.csv", row.names = FALSE)

# Extract trajectory metrics for producers
# Calculate trajectory lengths (not relative to initial)
producer_lengths <- trajectoryLengths(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums)
producer_lengths <- as.data.frame(producer_lengths)
producer_lengths$site <- site_order
write.csv(producer_lengths, "tables/SBC/producer/producer_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
producer_lengths_initial <- trajectoryLengths(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums, relativeToInitial = TRUE)
producer_lengths_initial <- as.data.frame(producer_lengths_initial)
producer_lengths_initial$site <- site_order
write.csv(producer_lengths_initial, "tables/SBC/producer/producer_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
producer_angles <- trajectoryAngles(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums)
producer_angles <- as.data.frame(producer_angles)
producer_angles$site <- site_order
write.csv(producer_angles, "tables/SBC/producer/producer_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
producer_angles_initial <- trajectoryAngles(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums, relativeToInitial = TRUE)
producer_angles_initial <- as.data.frame(producer_angles_initial)
producer_angles_initial$site <- site_order
write.csv(producer_angles_initial, "tables/SBC/producer/producer_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# producer_trajectory_distances <- trajectoryDistances(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums)
# producer_trajectory_distances <- as.data.frame(producer_trajectory_distances)
# producer_trajectory_distances$site <- site_order
# write.csv(producer_trajectory_distances, "tables/SBC/producer/producer_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
producer_convergence <- trajectoryConvergence(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums)
producer_convergence <- as.data.frame(producer_convergence)
producer_convergence$site <- site_order
write.csv(producer_convergence, "tables/SBC/producer/producer_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
producer_directionality <- trajectoryDirectionality(Dd, sites = sbc_producer_means$plot, surveys = sbc_producer_means$vector_nums)
producer_directionality <- as.data.frame(producer_directionality)
producer_directionality$site <- site_order
write.csv(producer_directionality, "tables/SBC/producer/producer_directionality.csv", row.names = FALSE)


# Extract trajectory metrics for invert
# Calculate trajectory lengths (not relative to initial)
invert_lengths <- trajectoryLengths(D, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums)
invert_lengths <- as.data.frame(invert_lengths)
invert_lengths$site <- site_order  
write.csv(invert_lengths, "tables/SBC/invert/invert_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
invert_lengths_initial <- trajectoryLengths(D, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums, relativeToInitial = TRUE)
invert_lengths_initial <- as.data.frame(invert_lengths_initial)
invert_lengths_initial$site <- site_order
write.csv(invert_lengths_initial, "tables/SBC/invert/invert_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
invert_angles <- trajectoryAngles(D, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums)
invert_angles <- as.data.frame(invert_angles)
invert_angles$site <- site_order
write.csv(invert_angles, "tables/SBC/invert/invert_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
invert_angles_initial <- trajectoryAngles(D, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums, relativeToInitial = TRUE)
invert_angles_initial <- as.data.frame(invert_angles_initial)
invert_angles_initial$site <- site_order
write.csv(invert_angles_initial, "tables/SBC/invert/invert_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# invert_trajectory_distances <- trajectoryDistances(D, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums)
# invert_trajectory_distances <- as.data.frame(invert_trajectory_distances)
# invert_trajectory_distances$site <- site_order
# write.csv(invert_trajectory_distances, "tables/SBC/invert/invert_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
invert_convergence <- trajectoryConvergence(D, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums)
invert_convergence <- as.data.frame(invert_convergence)
invert_convergence$site <- site_order
write.csv(invert_convergence, "tables/SBC/invert/invert_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
invert_directionality <- trajectoryDirectionality(D, sites = sbc_invert_means$plot, surveys = sbc_invert_means$vector_nums)
invert_directionality <- as.data.frame(invert_directionality)
invert_directionality$site <- site_order
write.csv(invert_directionality, "tables/SBC/invert/invert_directionality.csv", row.names = FALSE)
