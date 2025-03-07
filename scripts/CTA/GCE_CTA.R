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
gce_consumers = read.csv(file = "data/GCE/gce_consumer.csv")
gce_producers = read.csv(file = "data/GCE/gce_producer.csv")

gce_consumers = gce_consumers %>% 
  mutate(site = as_factor(site))

gce_producers = gce_producers %>% 
  mutate(site = as_factor(site))

gce_consumers = gce_consumers %>% 
  mutate(plot = as_factor(plot))

gce_producers = gce_producers %>% 
  mutate(plot = as_factor(plot))

gce_consumers = gce_consumers %>% 
  filter(abundance >= 0)

gce_consumers = gce_consumers %>%
  filter(!taxon_name %in% c("Crassostrea", "Slug", "Odostomia"))

gce_consumers = gce_consumers %>%
  filter(!plot %in% c("7"))

gce_producers = gce_producers %>% 
  filter(abundance >= 0)

gce_producers = gce_producers %>%
  filter(!plot %in% c("7"))
# Richness Summary Tables ----

# Summary data for consumer species richness across sites
species_n_consumer_site <- gce_consumers %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_consumer_site
write.csv(species_n_consumer_site, "tables/GCE/summary/species_n_consumer_site.csv",row.names = F)

# Summary data for producer species richness across sites
species_n_producer_site <- gce_producers %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_producer_site
write.csv(species_n_producer_site, "tables/GCE/summary/species_n_producer_site.csv",row.names = F)

# Site Selection & Order ----
# # remove 4 sites with less than 10 years of data
# unique(gce_consumers$plot)
site_order = as_factor(c("1","2" ,"3", "4", "5", "6", "8", "9","10"))
# 
# gce_consumers = gce_consumers %>% 
#   filter(!plot %in% c("004d", "004g", "010d", "n00b", "000b")) %>%
#   mutate(plot = factor(plot, levels = c("001d","004b" ,"n01b", "n04d", "002d", "004f", "002c", "0sub","020b", "0spb", "n20a", "n20b", "n01a", "n04a")))
# 
# gce_producers = gce_producers %>% 
#   filter(!plot %in% c("004d", "004g", "010d", "n00b", "000b")) %>%
#   mutate(plot = factor(plot, levels = c("001d","004b" ,"n01b", "n04d", "002d", "004f", "002c", "0sub","020b", "0spb", "n20a", "n20b", "n01a", "n04a")))

#only keep 15 producer sites that match consumer data
# gce_producers <- gce_producers %>%
#   filter(plot == site_order)
# 
# gce_producers <- gce_producers %>%
#   mutate(plot = factor(plot, levels = site_order)) %>%  # Ensure plot matches site_order
#   arrange(match(plot, site_order))

# Survey History Summary Tables ----

# Summary data years of consumer assemblage data per site
site_years_count_c <- gce_consumers %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_c
write.csv(site_years_count_c, "tables/GCE/summary/site_years_count_c.csv",row.names = F)

# Summary data years of producer assemblage data per site
site_years_count_p <- gce_producers %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_p
write.csv(site_years_count_p, "tables/GCE/summary/site_years_count_p.csv",row.names = F)

# Summary data total number of observations of each consumer species
# and the number of plots that species has been observed in at least once
site_spp_summary <- gce_consumers %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_spp_summary
write.csv(site_spp_summary, "tables/GCE/summary/site_spp_summary.csv",row.names = F)

# Summary data total number of observations of each producer species
# and the number of plots that species has been observed in at least once
site_producer_summary <- gce_producers %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_producer_summary
write.csv(site_producer_summary, "tables/GCE/summary/site_producer_summary.csv",row.names = F)


gce_consumer_means <- gce_consumers %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

gce_producer_means <- gce_producers %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))


gce_consumer_means <- gce_consumer_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))
gce_producer_means <- gce_producer_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))


gce_consumer_means = gce_consumer_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

gce_producer_means = gce_producer_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

gce_consumer_matrix = gce_consumer_means %>% 
  column_to_rownames(var = "site.year")

gce_producer_matrix = gce_producer_means %>% 
  column_to_rownames(var = "site.year")

gce_consumer_matrix = gce_consumer_matrix %>%
  dplyr::select(-plot, -year)

gce_producer_matrix = gce_producer_matrix %>%
  dplyr::select(-plot, -year)

# GCE NMDS ----

#doing this as a combined coral/algae community and as just coral or alage cover
nmds_consumer <- metaMDS(gce_consumer_matrix)
nmds_producer <- metaMDS(gce_producer_matrix)

# Extract NMDS coordinates
nmds_consumer_coords <- nmds_consumer$points
nmds_producer_coords <- nmds_producer$points

# Add site and year information to NMDS coordinates
nmds_consumer_data <- data.frame(nmds_consumer_coords, site.year = rownames(gce_consumer_matrix))
nmds_prooducer_data <- data.frame(nmds_producer_coords, site.year = rownames(gce_producer_matrix))

gce_consumer_means = gce_consumer_means %>% 
  left_join(nmds_consumer_data)

gce_consumer_means = gce_consumer_means %>% 
  mutate(site.year = paste0(plot,year))

gce_producer_means = gce_producer_means %>% 
  left_join(nmds_prooducer_data)

gce_producer_means = gce_producer_means %>% 
  mutate(site.year = paste0(plot,year))

# Create NMDS plots for... 

# consumers
plot_nmds_consumer <- ggplot(gce_consumer_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = as_factor(plot),
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = T, size = 3, color = "black", vjust = 1.5) +
  labs(title = "GCE Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_consumer)

ggsave(filename = paste0("figures/NMDS/GCE", "/", "plot_nmds_consumer", ".png"),
       plot = plot_nmds_consumer, width = 8, height = 6, units = "in", dpi = 600)


# producers
plot_nmds_producer <- ggplot(gce_producer_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = as_factor(plot),
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = F, size = 3, color = "black", vjust = 1.5) +
  labs(title = "GCE Producer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_producer)

ggsave(filename = paste0("figures/NMDS/GCE", "/", "plot_nmds_producer", ".png"),
       plot = plot_nmds_producer, width = 8, height = 6, units = "in", dpi = 600)


# GCE CTA ----

#consumers
D <- vegan::vegdist(gce_consumer_matrix,"bray")

custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22"
) 

# Generate the plot using trajectoryPCoA

gce_consumer_means = gce_consumer_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.2, "topright", inset = c(0.215, -0.05), bty="n", legend=c("1", "2", "3", "4", "5", "6", "8", "9", "10"),  col = c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                                                                               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/GCE/gce_consumer_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

# producers
Dd <- vegan::vegdist(gce_producer_matrix,"bray")

gce_producer_means = gce_producer_means %>% 
  mutate(vector_nums = year - min(year) + 1)

trajectoryPCoA(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.2, "topright", inset = c(0.215, -0.05), bty="n", legend=c("1", "2", "3", "4", "5", "6", "8", "9", "10"),  col = c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22"), lwd=2)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/GCE/gce_producer_trajectory_plot.png", width = 13, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()



# Individual Sites ----

# Define a list of unique sites
unique_sites <- unique(gce_consumers$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- gce_consumer_means %>% 
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
  csv_filename <- paste0("tables/GCE/sites/gce_consumer_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/GCE/gce_consumer_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(D1, sites = site_raw$plot, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}


#producers
unique_sites_producers <- unique(gce_consumers$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22"
)   # Extend this list as more sites are added

for (site in unique_sites_producers) {
  
  # Get the index of the site to select the corresponding color
  site_index <- which(unique_sites_producers == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw_p <- gce_producer_means %>% 
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
  csv_filename <- paste0("tables/GCE/sites/gce_producer_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Open the PNG file for saving
  filename <- paste0("figures/CTA/GCE/gce_producer_trajectory_plot_", site, ".png")
  png(filename = filename, width = 8, height = 8, res = 300, units = "in")
  
  # Run the trajectoryPCoA function with the specific color for the site
  trajectoryPCoA(D2, sites = site_raw_p$plot, surveys = site_raw_p$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = T)
  
  # Close the PNG device to save the file
  dev.off()
}

# CTA Metrics ----

# Extract trajectory metrics for consumers
# Calculate trajectory lengths (not relative to initial)
consumer_lengths <- trajectoryLengths(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums)
consumer_lengths <- as.data.frame(consumer_lengths)
consumer_lengths$site <- site_order
write.csv(consumer_lengths, "tables/GCE/consumer/consumer_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
consumer_lengths_initial <- trajectoryLengths(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums, relativeToInitial = TRUE)
consumer_lengths_initial <- as.data.frame(consumer_lengths_initial)
consumer_lengths_initial$site <- site_order
write.csv(consumer_lengths_initial, "tables/GCE/consumer/consumer_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
consumer_angles <- trajectoryAngles(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums)
consumer_angles <- as.data.frame(consumer_angles)
consumer_angles$site <- site_order
write.csv(consumer_angles, "tables/GCE/consumer/consumer_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
consumer_angles_initial <- trajectoryAngles(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums, relativeToInitial = TRUE)
consumer_angles_initial <- as.data.frame(consumer_angles_initial)
consumer_angles_initial$site <- site_order
write.csv(consumer_angles_initial, "tables/GCE/consumer/consumer_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# consumer_trajectory_distances <- trajectoryDistances(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums)
# consumer_trajectory_distances <- as.data.frame(consumer_trajectory_distances)
# consumer_trajectory_distances$site <- site_order
# write.csv(consumer_trajectory_distances, "tables/GCE/consumer/consumer_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
consumer_convergence <- trajectoryConvergence(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums)
consumer_convergence <- as.data.frame(consumer_convergence)
consumer_convergence$site <- site_order
write.csv(consumer_convergence, "tables/GCE/consumer/consumer_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
consumer_directionality <- trajectoryDirectionality(D, sites = gce_consumer_means$plot, surveys = gce_consumer_means$vector_nums)
consumer_directionality <- as.data.frame(consumer_directionality)
consumer_directionality$site <- site_order
write.csv(consumer_directionality, "tables/GCE/consumer/consumer_directionality.csv", row.names = FALSE)

# Extract trajectory metrics for producers
# Calculate trajectory lengths (not relative to initial)
producer_lengths <- trajectoryLengths(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums)
producer_lengths <- as.data.frame(producer_lengths)
producer_lengths$site <- site_order
write.csv(producer_lengths, "tables/GCE/producer/producer_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
producer_lengths_initial <- trajectoryLengths(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums, relativeToInitial = TRUE)
producer_lengths_initial <- as.data.frame(producer_lengths_initial)
producer_lengths_initial$site <- site_order
write.csv(producer_lengths_initial, "tables/GCE/producer/producer_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
producer_angles <- trajectoryAngles(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums)
producer_angles <- as.data.frame(producer_angles)
producer_angles$site <- site_order
write.csv(producer_angles, "tables/GCE/producer/producer_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
producer_angles_initial <- trajectoryAngles(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums, relativeToInitial = TRUE)
producer_angles_initial <- as.data.frame(producer_angles_initial)
producer_angles_initial$site <- site_order
write.csv(producer_angles_initial, "tables/GCE/producer/producer_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# producer_trajectory_distances <- trajectoryDistances(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums)
# producer_trajectory_distances <- as.data.frame(producer_trajectory_distances)
# producer_trajectory_distances$site <- site_order
# write.csv(producer_trajectory_distances, "tables/GCE/producer/producer_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
producer_convergence <- trajectoryConvergence(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums)
producer_convergence <- as.data.frame(producer_convergence)
producer_convergence$site <- site_order
write.csv(producer_convergence, "tables/GCE/producer/producer_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
producer_directionality <- trajectoryDirectionality(Dd, sites = gce_producer_means$plot, surveys = gce_producer_means$vector_nums)
producer_directionality <- as.data.frame(producer_directionality)
producer_directionality$site <- site_order
write.csv(producer_directionality, "tables/GCE/producer/producer_directionality.csv", row.names = FALSE)
