# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: December 24th, 2024

# Data Frame Generation ----

# install.packages(librarian)
librarian::shelf(tidyverse, googledrive, data.table, ecotraj, vegan, lterdatasampler, supportR, cowplot, summarytools, datacleanr)

# read data directly from EDI repo
knz_consumers = read.csv(file = "data/KNZ/knz_consumer.csv")
knz_producers = read.csv(file = "data/KNZ/knz_producer.csv")
# data_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.437.2&entityid=d9c5d73d11fc6a16ec41139b63b27751"
# data_long <- read.csv(file = data_url) # should be 217512 obs, 11 var

# calculate number of species per site
species_n_consumer_site <- knz_consumers %>%
  group_by(plot,year) %>%
  summarise(num_species = n_distinct(species))
species_n_consumer_site

species_n_producer_site <- knz_producers %>%
  group_by(plot,year) %>%
  summarise(num_species = n_distinct(species))
species_n_producer_site

# #create a site vector to make site level dataframes
# sites <- list(knz_consumers$unique_ID)
# 
# for (site_name in sites) {
#   # Filter the data frame for the current site
#   filtered_df <- subset(knz_consumers, unique_ID == site_name)
#   
#   # Dynamically assign the filtered data frame to a uniquely named variable
#   assign(paste0(gsub("\\.", "_", site_name), "_df"), filtered_df)
# }

# KNZ ----

# check the structures of the df
glimpse(knz_consumers)
glimpse(knz_producers)

# remove 4 sites with less than 10 years of data

site_order = c("001d", "004b", "n01b", "n04d", "002d", "004f", "002c", "0sub", "020b", "0spb", "n20a", "n20b", "n01a", "n04a")

knz_consumers = knz_consumers %>% 
  filter(!plot %in% c("004d", "004g", "010d", "n00b", "000b")) %>% 
  mutate(plot = factor(plot, levels = c("001d", "004b", "n01b", "n04d", "002d", "004f", "002c", "0sub", "020b", "0spb", "n20a", "n20b", "n01a", "n04a")))


#only keep 15 producer sites that match consumer data
knz_producers <- knz_producers %>%
  filter(plot == site_order)

knz_producers <- knz_producers %>%
  mutate(plot = factor(plot, levels = site_order)) %>%  # Ensure plot matches site_order
  arrange(match(plot, site_order))


unique(knz_consumers$plot)
unique(knz_producers$plot)

site_years_count_c <- knz_consumers %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)

site_years_count_p <- knz_producers %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)

site_spp_summary <- knz_consumers %>%
  group_by(species) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(species)

site_producer_summary <- knz_producers %>%
  group_by(species) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(species)


knz_consumers_means <- knz_consumers %>%
  group_by(year, plot, species) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

knz_producer_means <- knz_producers %>%
  group_by(year, plot, species) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))


knz_consumers_means <- knz_consumers_means %>%
  pivot_wider(names_from = species, values_from = mean_abundance, values_fill = list(mean_abundance = 0))
knz_producer_means <- knz_producer_means %>%
  pivot_wider(names_from = species, values_from = mean_abundance, values_fill = list(mean_abundance = 0))


knz_consumers_means = knz_consumers_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

knz_producer_means = knz_producer_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

knz_consumer_matrix = knz_consumers_means %>% 
  column_to_rownames(var = "site.year")

knz_producer_matrix = knz_producer_means %>% 
  column_to_rownames(var = "site.year")

knz_consumer_matrix = knz_consumer_matrix %>%
  dplyr::select(-plot, -year)

knz_producer_matrix = knz_producer_matrix %>%
  dplyr::select(-plot, -year)

# KNZ NMDS ----

#doing this as a combined coral/algae community and as just coral or alage cover
nmds_consumer <- metaMDS(knz_consumer_matrix)
nmds_producer <- metaMDS(knz_producer_matrix)

# Extract NMDS coordinates
nmds_consumer_coords <- nmds_consumer$points
nmds_producer_coords <- nmds_producer$points

# Add site and year information to NMDS coordinates
nmds_consumer_data <- data.frame(nmds_consumer_coords, site.year = rownames(knz_consumer_matrix))
nmds_prooducer_data <- data.frame(nmds_producer_coords, site.year = rownames(knz_producer_matrix))

knz_consumers_means = knz_consumers_means %>% 
  left_join(nmds_consumer_data)

knz_consumers_means = knz_consumers_means %>% 
  mutate(site.year = paste0(plot,year))

knz_producer_means = knz_producer_means %>% 
  left_join(nmds_prooducer_data)

knz_producer_means = knz_producer_means %>% 
  mutate(site.year = paste0(plot,year))

# Create NMDS plots for... 

# consumers
plot_nmds_consumer <- ggplot(knz_consumers_means, aes(x = MDS1,
                                                y = MDS2,
                                                color = plot,
                                                label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "KNZ Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8", "#ffbb78", "#98df8a", "#ff9896")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_consumer)

ggsave(filename = paste0("figures/NMDS/KNZ", "/", "plot_nmds_consumer", ".png"),
       plot = plot_nmds_consumer, width = 8, height = 6, units = "in", dpi = 600)


# producers
plot_nmds_producer <- ggplot(knz_producer_means, aes(x = MDS1,
                                                      y = MDS2,
                                                      color = plot,
                                                      label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "KNZ Producer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_producer)

ggsave(filename = paste0("figures/NMDS/KNZ", "/", "plot_nmds_producer", ".png"),
       plot = plot_nmds_producer, width = 8, height = 6, units = "in", dpi = 600)


# KNZ CTA ----

#consumers
D <- vegan::vegdist(knz_consumer_matrix,"bray")

custom_palette <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896"
) 

# Generate the plot using trajectoryPCoA

knz_consumers_means = knz_consumers_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D, sites = knz_consumers_means$plot, surveys = knz_consumers_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0, 0), bty="n", legend=c("001d", "004b", "n01b", "n04d", "002d", "004f", "002c", "0sub", "020b", "0spb", "n20a", "n20b", "n01a", "n04a"),  col = c(    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                              "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                                                                              "#aec7e8", "#ffbb78", "#98df8a", "#ff9896"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/KNZ/knz_consumer_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()


# producers
Dd <- vegan::vegdist(knz_producer_matrix,"bray")

knz_producer_means = knz_producer_means %>% 
  mutate(vector_nums = year - min(year) + 1)

unique(knz_producers$plot)
trajectoryPCoA(Dd, sites = knz_producer_means$plot, surveys = knz_producer_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(-0.05, 0), bty="n", legend=c("001d", "004b", "n01b", "n04d", "002d", "004f", "002c", "0sub", "020b", "0spb", "n20a", "n20b", "n01a", "n04a"),  col = c(    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                                                                                                                               "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                                                                                                                                                                                               "#aec7e8", "#ffbb78", "#98df8a", "#ff9896"), lwd=2)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/KNZ/knz_producer_trajectory_plot.png", width = 13, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()



# Individual Sites ----
# Define a list of unique sites
unique_sites <- unique(knz_consumers$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- knz_consumers_means %>% 
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
  csv_filename <- paste0("tables/KNZ/knz_consumer_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/KNZ/knz_consumer_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(D1, sites = site_raw$plot, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}


#producers
unique_sites_producers <- unique(knz_consumers$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896"
)   # Extend this list as more sites are added

for (site in unique_sites_producers) {
  
  # Get the index of the site to select the corresponding color
  site_index <- which(unique_sites_producers == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw_p <- knz_producer_means %>% 
    filter(plot == site)
  
  # Prepare the site data
  site_data <- site_raw_p %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums)
  
  # Compute Bray-Curtis distance matrix
  D1 <- vegan::vegdist(site_data, "bray")
  
  # Open the PNG file for saving
  filename <- paste0("figures/CTA/KNZ/knz_producer_trajectory_plot_", site, ".png")
  png(filename = filename, width = 8, height = 8, res = 300, units = "in")
  
  # Run the trajectoryPCoA function with the specific color for the site
  trajectoryPCoA(D1, sites = site_raw_p$plot, surveys = site_raw_p$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = T)
  
  # Close the PNG device to save the file
  dev.off()
}

# CTA Metrics ----

