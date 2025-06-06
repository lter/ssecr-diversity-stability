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
pie_consumers = read.csv(file = "data/PIE/pie_consumer.csv")
pie_producers = read.csv(file = "data/PIE/pie_producer.csv")

# Richness Summary Tables ----
pie_consumers = pie_consumers %>% 
  mutate(taxon_name = paste0(genus, species, sep = "_"))

pie_producers = pie_producers %>% 
  mutate(taxon_name = paste0(genus, species, sep = "_"))

# Summary data for consumer species richness across sites
species_n_consumer_site <- pie_consumers %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_consumer_site
write.csv(species_n_consumer_site, "tables/PIE/summary/species_n_consumer_site.csv",row.names = F)

# Summary data for producer species richness across sites
species_n_producer_site <- pie_producers %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_producer_site
write.csv(species_n_producer_site, "tables/PIE/summary/species_n_producer_site.csv",row.names = F)

# Site Selection & Order ----
# remove 4 sites with less than 10 years of data
unique(pie_consumers$plot)
unique(pie_producers$plot)
consumer_site_order = c("RM1","RM2" ,"RM3", "RM4", "RM5", "RM6")
producer_site_order = c("CC1",   "CC2",   "PUH4",  "PUH2",  "McH3",  "EPH1" , "McH4" , "RR5.2", "EPH2",  "PUH1",  "RM3.1","RM2.3", "RR5.1", "RR5.3", "PUH3",  "McH",   "McH2",  "RM2.1", "RM4.1", "RM2.2")


# pie_consumers = pie_consumers %>% 
#   filter(!plot %in% c("004d", "004g", "010d", "n00b", "000b")) %>%
#   mutate(plot = factor(plot, levels = c("001d","004b" ,"n01b", "n04d", "002d", "004f", "002c", "0sub","020b", "0spb", "n20a", "n20b", "n01a", "n04a")))
# 
# pie_producers = pie_producers %>% 
#   filter(!plot %in% c("004d", "004g", "010d", "n00b", "000b")) %>%
#   mutate(plot = factor(plot, levels = c("001d","004b" ,"n01b", "n04d", "002d", "004f", "002c", "0sub","020b", "0spb", "n20a", "n20b", "n01a", "n04a")))
# 
# #only keep 15 producer sites that match consumer data
# pie_producers <- pie_producers %>%
#   filter(plot == site_order)
# 
# pie_producers <- pie_producers %>%
#   mutate(plot = factor(plot, levels = site_order)) %>%  # Ensure plot matches site_order
#   arrange(match(plot, site_order))

# Survey History Summary Tables ----

# Summary data years of consumer assemblage data per site
site_years_count_c <- pie_consumers %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_c
write.csv(site_years_count_c, "tables/PIE/summary/site_years_count_c.csv",row.names = F)

# Summary data years of producer assemblage data per site
site_years_count_p <- pie_producers %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_p
write.csv(site_years_count_p, "tables/PIE/summary/site_years_count_p.csv",row.names = F)

# Summary data total number of observations of each consumer species
# and the number of plots that species has been observed in at least once
site_spp_summary <- pie_consumers %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_spp_summary
write.csv(site_spp_summary, "tables/PIE/summary/site_spp_summary.csv",row.names = F)

# Summary data total number of observations of each producer species
# and the number of plots that species has been observed in at least once
site_producer_summary <- pie_producers %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_producer_summary
write.csv(site_producer_summary, "tables/PIE/summary/site_producer_summary.csv",row.names = F)


pie_consumer_means <- pie_consumers %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

pie_producer_means <- pie_producers %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))


pie_consumer_means <- pie_consumer_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))
pie_producer_means <- pie_producer_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))


pie_consumer_means = pie_consumer_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

pie_producer_means = pie_producer_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

pie_consumer_matrix = pie_consumer_means %>% 
  column_to_rownames(var = "site.year")

pie_producer_matrix = pie_producer_means %>% 
  column_to_rownames(var = "site.year")

pie_consumer_matrix = pie_consumer_matrix %>%
  dplyr::select(-plot, -year)

pie_producer_matrix = pie_producer_matrix %>%
  dplyr::select(-plot, -year)

# PIE NMDS ----

#doing this as a combined coral/algae community and as just coral or alage cover
nmds_consumer <- metaMDS(pie_consumer_matrix)
nmds_producer <- metaMDS(pie_producer_matrix)

# Extract NMDS coordinates
nmds_consumer_coords <- nmds_consumer$points
nmds_producer_coords <- nmds_producer$points

# Add site and year information to NMDS coordinates
nmds_consumer_data <- data.frame(nmds_consumer_coords, site.year = rownames(pie_consumer_matrix))
nmds_prooducer_data <- data.frame(nmds_producer_coords, site.year = rownames(pie_producer_matrix))

pie_consumer_means = pie_consumer_means %>% 
  left_join(nmds_consumer_data)

pie_consumer_means = pie_consumer_means %>% 
  mutate(site.year = paste0(plot,year))

pie_producer_means = pie_producer_means %>% 
  left_join(nmds_prooducer_data)

pie_producer_means = pie_producer_means %>% 
  mutate(site.year = paste0(plot,year))

# Create NMDS plots for... 

# consumers
plot_nmds_consumer <- ggplot(pie_consumer_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = plot,
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "PIE Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8", "#ffbb78", "#98df8a", "#ff9896")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_consumer)

ggsave(filename = paste0("figures/NMDS/PIE", "/", "plot_nmds_consumer", ".png"),
       plot = plot_nmds_consumer, width = 8, height = 6, units = "in", dpi = 600)


# producers
plot_nmds_producer <- ggplot(pie_producer_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = plot,
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "PIE Producer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
                                "#8dd3c7", "#fb8072", "#80b1d3", "#fdb462", "#b3de69")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_producer)

ggsave(filename = paste0("figures/NMDS/PIE", "/", "plot_nmds_producer", ".png"),
       plot = plot_nmds_producer, width = 8, height = 6, units = "in", dpi = 600)


# PIE CTA ----

#consumers
D <- vegan::vegdist(pie_consumer_matrix,"bray")

custom_palette <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b"
) 

# Generate the plot using trajectoryPCoA

pie_consumer_means = pie_consumer_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("RM1","RM2" ,"RM3", "RM4", "RM5", "RM6"),  col = c( "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                                                                               "#8c564b"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/PIE/pie_consumer_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

# producers
Dd <- vegan::vegdist(pie_producer_matrix,"bray")

pie_producer_means = pie_producer_means %>% 
  mutate(vector_nums = year - min(year) + 1)

trajectoryPCoA(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("CC1",   "CC2",   "PUH4",  "PUH2",  "McH3",  "EPH1" , "McH4" , "RR5.2", "EPH2",  "PUH1",  "RM3.1","RM2.3", "RR5.1", "RR5.3", "PUH3",  "McH",   "McH2",  "RM2.1", "RM4.1", "RM2.2"),  col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                                                                              "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                                                                                                                                                                                              "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
                                                                                                                                                                                                              "#8dd3c7", "#fb8072", "#80b1d3", "#fdb462", "#b3de69"), lwd=2)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/PIE/pie_producer_trajectory_plot.png", width = 13, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()



# Individual Sites ----

# Define a list of unique sites
unique_sites <- unique(pie_consumers$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- pie_consumer_means %>% 
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
  csv_filename <- paste0("tables/PIE/sites/pie_consumer_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/PIE/pie_consumer_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(D1, sites = site_raw$plot, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}



#producers
pie_producers = pie_producers %>% 
  filter(!plot %in% c("McH", "McH2"))

unique_sites_producers <- unique(pie_producers$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
                  "#8dd3c7", "#fb8072", "#80b1d3", "#fdb462", "#b3de69")   # Extend this list as more sites are added

for (site in unique_sites_producers) {
  
  # Get the index of the site to select the corresponding color
  site_index <- which(unique_sites_producers == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw_p <- pie_producer_means %>% 
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
  csv_filename <- paste0("tables/PIE/sites/pie_producer_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Open the PNG file for saving
  filename <- paste0("figures/CTA/PIE/pie_producer_trajectory_plot_", site, ".png")
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
consumer_lengths <- trajectoryLengths(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums)
consumer_lengths <- as.data.frame(consumer_lengths)
consumer_lengths$site <- consumer_site_order  
write.csv(consumer_lengths, "tables/PIE/consumer/consumer_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
consumer_lengths_initial <- trajectoryLengths(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums, relativeToInitial = TRUE)
consumer_lengths_initial <- as.data.frame(consumer_lengths_initial)
consumer_lengths_initial$site <- consumer_site_order
write.csv(consumer_lengths_initial, "tables/PIE/consumer/consumer_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
consumer_angles <- trajectoryAngles(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums)
consumer_angles <- as.data.frame(consumer_angles)
consumer_angles$site <- consumer_site_order
write.csv(consumer_angles, "tables/PIE/consumer/consumer_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
consumer_angles_initial <- trajectoryAngles(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums, relativeToInitial = TRUE)
consumer_angles_initial <- as.data.frame(consumer_angles_initial)
consumer_angles_initial$site <- consumer_site_order
write.csv(consumer_angles_initial, "tables/PIE/consumer/consumer_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# consumer_trajectory_distances <- trajectoryDistances(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums)
# consumer_trajectory_distances <- as.data.frame(consumer_trajectory_distances)
# consumer_trajectory_distances$site <- site_order
# write.csv(consumer_trajectory_distances, "tables/PIE/consumer/consumer_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
consumer_convergence <- trajectoryConvergence(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums)
consumer_convergence <- as.data.frame(consumer_convergence)
consumer_convergence$site <- consumer_site_order
write.csv(consumer_convergence, "tables/PIE/consumer/consumer_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
consumer_directionality <- trajectoryDirectionality(D, sites = pie_consumer_means$plot, surveys = pie_consumer_means$vector_nums)
consumer_directionality <- as.data.frame(consumer_directionality)
consumer_directionality$site <- consumer_site_order
write.csv(consumer_directionality, "tables/PIE/consumer/consumer_directionality.csv", row.names = FALSE)

# Extract trajectory metrics for producers

# Calculate trajectory lengths (not relative to initial)
producer_lengths <- trajectoryLengths(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums)
producer_lengths <- as.data.frame(producer_lengths)
producer_lengths$site <- producer_site_order
write.csv(producer_lengths, "tables/PIE/producer/producer_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
producer_lengths_initial <- trajectoryLengths(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums, relativeToInitial = TRUE)
producer_lengths_initial <- as.data.frame(producer_lengths_initial)
producer_lengths_initial$site <- producer_site_order
write.csv(producer_lengths_initial, "tables/PIE/producer/producer_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
producer_angles <- trajectoryAngles(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums)
producer_angles <- as.data.frame(producer_angles)
producer_angles$site <- producer_site_order
write.csv(producer_angles, "tables/PIE/producer/producer_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
producer_angles_initial <- trajectoryAngles(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums, relativeToInitial = TRUE)
producer_angles_initial <- as.data.frame(producer_angles_initial)
producer_angles_initial$site <- producer_site_order
write.csv(producer_angles_initial, "tables/PIE/producer/producer_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# producer_trajectory_distances <- trajectoryDistances(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums)
# producer_trajectory_distances <- as.data.frame(producer_trajectory_distances)
# producer_trajectory_distances$site <- site_order
# write.csv(producer_trajectory_distances, "tables/PIE/producer/producer_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
producer_convergence <- trajectoryConvergence(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums)
producer_convergence <- as.data.frame(producer_convergence)
producer_convergence$site <- producer_site_order
write.csv(producer_convergence, "tables/PIE/producer/producer_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
producer_directionality <- trajectoryDirectionality(Dd, sites = pie_producer_means$plot, surveys = pie_producer_means$vector_nums)
producer_directionality <- as.data.frame(producer_directionality)
producer_directionality$site <- producer_site_order
write.csv(producer_directionality, "tables/PIE/producer/producer_directionality.csv", row.names = FALSE)
