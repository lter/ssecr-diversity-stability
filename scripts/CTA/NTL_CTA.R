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
ntl_macroinverts = read.csv(file = "data/NTL/benthic_macroinvertebrates_trout_lake_area.csv")
ntl_zooplankton = read.csv(file = "data/NTL/zooplankton_trout_lake_area.csv")
ntl_phytoplankton = read.csv(file = "data/NTL/phytoplankton_trout_lake_area.csv")

# Richness Summary Tables ----

# Summary data for consumer species richness across sites
species_n_zooplankton_site <- ntl_zooplankton %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_zooplankton_site
write.csv(species_n_zooplankton_site, "tables/NTL/summary/species_n_zooplankton_site.csv",row.names = F)

# Summary data for producer species richness across sites
species_n_macroinverts_site <- ntl_macroinverts %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_macroinverts_site
write.csv(species_n_macroinverts_site, "tables/NTL/summary/species_n_macroinverts_site.csv",row.names = F)

species_n_phytoplankton_site <- ntl_phytoplankton %>%
  group_by(plot, year) %>%
  summarise(num_species = n_distinct(taxon_name))
species_n_phytoplankton_site
write.csv(species_n_phytoplankton_site, "tables/NTL/summary/species_n_phytoplankton_site.csv",row.names = F)

# Site Selection & Order ----
# remove 4 sites with less than 10 years of data
unique(ntl_zooplankton$plot)
unique(ntl_macroinverts$plot)
unique(ntl_phytoplankton$plot)



ntl_zooplankton = ntl_zooplankton %>% 
  mutate(plot = as_factor(plot))

ntl_macroinverts = ntl_macroinverts %>% 
  mutate(plot = as_factor(plot))

ntl_phytoplankton = ntl_phytoplankton %>% 
  mutate(plot = as_factor(plot))



site_order_zoo = c("1","2")
site_order_mac = c("27",   "43",   "6",    "9",    "GILL", "1",    "19",   "21" ,  "24",   "17",   "31" ,  "50",   "56","67" ,  "7"  )
site_order_phyto = c("1")


# ntl_consumers = ntl_consumers %>% 
#   filter(!plot %in% c("004d", "004g", "010d", "n00b", "000b")) %>%
#   mutate(plot = factor(plot, levels = c("001d","004b" ,"n01b", "n04d", "002d", "004f", "002c", "0sub","020b", "0spb", "n20a", "n20b", "n01a", "n04a")))
# 
# ntl_producers = ntl_producers %>% 
#   filter(!plot %in% c("004d", "004g", "010d", "n00b", "000b")) %>%
#   mutate(plot = factor(plot, levels = c("001d","004b" ,"n01b", "n04d", "002d", "004f", "002c", "0sub","020b", "0spb", "n20a", "n20b", "n01a", "n04a")))
# 
# #only keep 15 producer sites that match consumer data
# ntl_producers <- ntl_producers %>%
#   filter(plot == site_order)
# 
# ntl_producers <- ntl_producers %>%
#   mutate(plot = factor(plot, levels = site_order)) %>%  # Ensure plot matches site_order
#   arrange(match(plot, site_order))

# Survey History Summary Tables ----

# Summary data years of consumer assemblage data per site
site_years_count_zooplankton <- ntl_zooplankton %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_zooplankton
write.csv(site_years_count_zooplankton, "tables/NTL/summary/site_years_count_zooplankton.csv",row.names = F)


# Summary data years of consumer assemblage data per site
site_years_count_phytoplankton <- ntl_phytoplankton %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_phytoplankton
write.csv(site_years_count_phytoplankton, "tables/NTL/summary/site_years_count_phytoplankton.csv",row.names = F)


# Summary data years of consumer assemblage data per site
site_years_count_macroinverts <- ntl_macroinverts %>%
  group_by(plot) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(plot)
site_years_count_macroinverts
write.csv(site_years_count_macroinverts, "tables/NTL/summary/site_years_count_macroinverts.csv",row.names = F)




# Summary data total number of observations of each consumer species
# and the number of plots that species has been observed in at least once
site_zooplankton_summary <- ntl_zooplankton %>%
  group_by(taxon_code) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_code)
site_zooplankton_summary
write.csv(site_zooplankton_summary, "tables/NTL/summary/site_zooplankton_summary.csv",row.names = F)


site_macroinverts_summary <- ntl_macroinverts %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_macroinverts_summary
write.csv(site_macroinverts_summary, "tables/NTL/summary/site_macroinverts_summary.csv",row.names = F)


site_phytoplankton_summary <- ntl_phytoplankton %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(plot)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_phytoplankton_summary
write.csv(site_phytoplankton_summary, "tables/NTL/summary/site_phytoplankton_summary.csv",row.names = F)


# Summary data total number of observations of each producer species
# and the number of plots that species has been observed in at least once

ntl_zooplankton_means <- ntl_zooplankton %>%
  group_by(year, plot, taxon_code) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

ntl_phytoplankton_means <- ntl_phytoplankton %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

ntl_macroinverts_means <- ntl_macroinverts %>%
  group_by(year, plot, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))


ntl_zooplankton_means <- ntl_zooplankton_means %>%
  pivot_wider(names_from = taxon_code, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

ntl_phytoplankton_means <- ntl_phytoplankton_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

ntl_macroinverts_means <- ntl_macroinverts_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))


ntl_zooplankton_means = ntl_zooplankton_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

ntl_phytoplankton_means = ntl_phytoplankton_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))

ntl_macroinverts_means = ntl_macroinverts_means %>% 
  mutate(site.year = paste(plot, year, sep = "-"))



ntl_zooplankton_matrix = ntl_zooplankton_means %>% 
  column_to_rownames(var = "site.year")

ntl_phytoplankton_matrix = ntl_phytoplankton_means %>% 
  column_to_rownames(var = "site.year")

ntl_macroinverts_matrix = ntl_macroinverts_means %>% 
  column_to_rownames(var = "site.year")



ntl_zooplankton_matrix = ntl_zooplankton_matrix %>%
  dplyr::select(-plot, -year)

ntl_phytoplankton_matrix = ntl_phytoplankton_matrix %>%
  dplyr::select(-plot, -year)

ntl_macroinverts_matrix = ntl_macroinverts_matrix %>%
  dplyr::select(-plot, -year)

# NTL NMDS ----

#doing this as a combined coral/algae community and as just coral or alage cover
nmds_zooplankton <- metaMDS(ntl_zooplankton_matrix)
# nmds_phytoplankton <- metaMDS(ntl_phytoplankton_matrix)
nmds_macroinverts <- metaMDS(ntl_macroinverts_matrix)
# Extract NMDS coordinates
nmds_zooplankton_coords <- nmds_zooplankton$points
# nmds_phytoplankton_coords <- nmds_phytoplankton$points
nmds_macroinverts_coords <- nmds_macroinverts$points

# Add site and year information to NMDS coordinates
nmds_zooplankton_data <- data.frame(nmds_zooplankton_coords, site.year = rownames(ntl_zooplankton_matrix))

# nmds_phytoplankton_data <- data.frame(nmds_phytoplankton_coords, site.year = rownames(ntl_phytoplankton_matrix))

nmds_macroinverts_data <- data.frame(nmds_macroinverts_coords, site.year = rownames(ntl_macroinverts_matrix))



ntl_zooplankton_means = ntl_zooplankton_means %>% 
  left_join(nmds_zooplankton_data)

ntl_zooplankton_means = ntl_zooplankton_means %>% 
  mutate(site.year = paste0(plot,year))

# 
# ntl_phytoplankton_means = ntl_phytoplankton_means %>% 
#   left_join(nmds_phytoplankton_data)
# 
# ntl_phytoplankton_means = ntl_phytoplankton_means %>% 
#   mutate(site.year = paste0(plot,year))


ntl_macroinverts_means = ntl_macroinverts_means %>% 
  left_join(nmds_macroinverts_data)

ntl_macroinverts_means = ntl_macroinverts_means %>% 
  mutate(site.year = paste0(plot,year))

# Create NMDS plots for... 

# zooplankton
plot_nmds_zooplankton <- ggplot(ntl_zooplankton_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = plot,
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "NTL Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_zooplankton)

ggsave(filename = paste0("figures/NMDS/NTL", "/", "plot_nmds_consumer", ".png"),
       plot = plot_nmds_zooplankton, width = 8, height = 6, units = "in", dpi = 600)

# # phytoplankton
# plot_nmds_phytoplankton <- ggplot(ntl_phytoplankton_means, aes(x = MDS1,
#                                                            y = MDS2,
#                                                            color = plot,
#                                                            label = year)) +
#   geom_point(size = 3) +
#   geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
#   labs(title = "NTL Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
#   scale_color_manual(values = c("#1f77b4", "#ff7f0e")) +
#   # scale_shape_manual(values = c(17,15,18,19)) +
#   guides(color = guide_legend(title = "Site")) +
#   theme_classic()
# 
# print(plot_nmds_phytoplankton)
# 
# ggsave(filename = paste0("figures/NMDS/NTL", "/", "plot_nmds_consumer", ".png"),
#        plot = plot_nmds_phytoplankton, width = 8, height = 6, units = "in", dpi = 600)


# macroinverts
plot_nmds_macroinverts <- ggplot(ntl_macroinverts_means, aes(x = MDS1,
                                                           y = MDS2,
                                                           color = plot,
                                                           label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "NTL Consumer Community by Site", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c(  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896","purple" )) +
  # scale_shape_manual(values = c(17,15,18,19)) +
  guides(color = guide_legend(title = "Site")) +
  theme_classic()

print(plot_nmds_macroinverts)

ggsave(filename = paste0("figures/NMDS/NTL", "/", "plot_nmds_consumer", ".png"),
       plot = plot_nmds_macroinverts, width = 8, height = 6, units = "in", dpi = 600)
# NTL CTA ----

#consumers
D <- vegan::vegdist(ntl_zooplankton_matrix,"bray")

custom_palette <- c(
  "#1f77b4", "#ff7f0e"
) 

custom_palette_i <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896","purple" 
)

# Generate the plot using trajectoryPCoA

ntl_zooplankton_means = ntl_zooplankton_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("1", "2"),  col = c( "#1f77b4", "#ff7f0e"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/NTL/ntl_zooplankton_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

# producers
# Dd <- vegan::vegdist(ntl_phytoplankton_matrix,"bray")
# 
# ntl_phytoplankton_means = ntl_phytoplankton_means %>% 
#   mutate(vector_nums = year - min(year) + 1)
# 
# trajectoryPCoA(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
# legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("1"),  col = c( "#1f77b4"), lwd=2)
# 
# # Capture the plot output
# trajectory_plot <- recordPlot()
# 
# # Save the plot as a PNG file
# png(filename = "figures/CTA/NTL/ntl_phytoplankton_trajectory_plot.png", width = 13, height = 8, res = 300, units = "in")
# trajectory_plot
# dev.off()

# macroinverts
Di <- vegan::vegdist(ntl_macroinverts_matrix,"bray")

ntl_macroinverts_means = ntl_macroinverts_means %>%
  mutate(vector_nums = year - min(year) + 1)

trajectoryPCoA(Di, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums, traj.colors = custom_palette_i, lwd = 2, survey.labels = T)
legend(ncol = 1, y.intersp = 0.5, "topright", inset = c(0.02, 0), bty="n", legend=c("27",   "43",   "6",    "9",    "GILL", "1",    "19",   "21" ,  "24",   "17",   "31" ,  "50",   "56","67" ,  "7"  ),  col = c(   "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                                                                                                      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                                                                                                      "#aec7e8", "#ffbb78", "#98df8a", "#ff9896","purple" ), lwd=2)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/NTL/ntl_macroinverts_trajectory_plot.png", width = 13, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

# Individual Sites ----

# Define a list of unique sites
unique_sites <- unique(ntl_zooplankton$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#1f77b4", "#ff7f0e"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- ntl_zooplankton_means %>% 
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
  csv_filename <- paste0("tables/NTL/sites/ntl_zooplankton_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/NTL/ntl_zooplankton_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(D1, sites = site_raw$plot, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}


# #producers
# unique_sites_phytoplankton <- unique(ntl_phytoplankton$plot)
# 
# # Loop through each site
# # Define a vector of colors (add more as needed)
# color_vector_2 <- c(
#   "#1f77b4"
# )   # Extend this list as more sites are added
# 
# for (site in unique_sites_phytoplankton) {
#   
#   # Get the index of the site to select the corresponding color
#   site_index <- which(unique_sites_phytoplankton == site)
#   site_color <- color_vector_2[site_index]
#   
#   # Filter data for the current site
#   site_raw_p <- ntl_phytoplankton_means %>% 
#     filter(plot == site)
#   
#   # Prepare the site data
#   site_data <- site_raw_p %>% 
#     column_to_rownames(var = "site.year") %>%
#     dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums)
#   
#   # Compute Bray-Curtis distance matrix
#   D2 <- vegan::vegdist(site_data, "bray")
#   
#   
#   # Calculate segment lengths using trajectoryLengths
#   segment_lengths <- trajectoryLengths(D2, sites = site_raw_p$plot, surveys = site_raw_p$vector_nums)
#   
#   # Create a data frame for the segment lengths
#   segment_lengths_df <- data.frame(
#     Segment = seq_along(segment_lengths),
#     Length = segment_lengths
#   )
#   
#   # Save segment lengths as a CSV file
#   csv_filename <- paste0("tables/NTL/sites/ntl_phytoplankton_segment_lengths_", site, ".csv")
#   write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
#   
#   # Open the PNG file for saving
#   filename <- paste0("figures/CTA/NTL/ntl_phytoplankton_trajectory_plot_", site, ".png")
#   png(filename = filename, width = 8, height = 8, res = 300, units = "in")
#   
#   # Run the trajectoryPCoA function with the specific color for the site
#   trajectoryPCoA(D2, sites = site_raw_p$plot, surveys = site_raw_p$vector_nums, 
#                  traj.colors = site_color, lwd = 2, survey.labels = T)
#   
#   # Close the PNG device to save the file
#   dev.off()
# }

# Define a list of unique sites
unique_sites <- unique(ntl_macroinverts$plot)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector_3 <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
  "#aec7e8", "#ffbb78", "#98df8a", "#ff9896","purple"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector_3[site_index]
  
  # Filter data for the current site
  site_raw <- ntl_macroinverts_means %>% 
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
  csv_filename <- paste0("tables/NTL/sites/ntl_macroinverts_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/NTL/ntl_macroinverts_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(D1, sites = site_raw$plot, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}



# CTA Metrics ----

# Extract trajectory metrics for zooplankton
# Calculate trajectory lengths (not relative to initial)
zooplankton_lengths <- trajectoryLengths(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums)
zooplankton_lengths <- as.data.frame(zooplankton_lengths)
zooplankton_lengths$site <- site_order_zoo  
write.csv(zooplankton_lengths, "tables/NTL/zooplankton/zooplankton_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
zooplankton_lengths_initial <- trajectoryLengths(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums, relativeToInitial = TRUE)
zooplankton_lengths_initial <- as.data.frame(zooplankton_lengths_initial)
zooplankton_lengths_initial$site <- site_order_zoo
write.csv(zooplankton_lengths_initial, "tables/NTL/zooplankton/zooplankton_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
zooplankton_angles <- trajectoryAngles(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums)
zooplankton_angles <- as.data.frame(zooplankton_angles)
zooplankton_angles$site <- site_order_zoo
write.csv(zooplankton_angles, "tables/NTL/zooplankton/zooplankton_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
zooplankton_angles_initial <- trajectoryAngles(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums, relativeToInitial = TRUE)
zooplankton_angles_initial <- as.data.frame(zooplankton_angles_initial)
zooplankton_angles_initial$site <- site_order_zoo
write.csv(zooplankton_angles_initial, "tables/NTL/zooplankton/zooplankton_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# zooplankton_trajectory_distances <- trajectoryDistances(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums)
# zooplankton_trajectory_distances <- as.data.frame(zooplankton_trajectory_distances)
# zooplankton_trajectory_distances$site <- site_order_zoo
# write.csv(zooplankton_trajectory_distances, "tables/NTL/zooplankton/zooplankton_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
zooplankton_convergence <- trajectoryConvergence(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums)
zooplankton_convergence <- as.data.frame(zooplankton_convergence)
zooplankton_convergence$site <- site_order_zoo
write.csv(zooplankton_convergence, "tables/NTL/zooplankton/zooplankton_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
zooplankton_directionality <- trajectoryDirectionality(D, sites = ntl_zooplankton_means$plot, surveys = ntl_zooplankton_means$vector_nums)
zooplankton_directionality <- as.data.frame(zooplankton_directionality)
zooplankton_directionality$site <- site_order_zoo
write.csv(zooplankton_directionality, "tables/NTL/zooplankton/zooplankton_directionality.csv", row.names = FALSE)

# # Extract trajectory metrics for phytoplanktons
# # Calculate trajectory lengths (not relative to initial)
# phytoplankton_lengths <- trajectoryLengths(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums)
# phytoplankton_lengths <- as.data.frame(phytoplankton_lengths)
# phytoplankton_lengths$site <- site_order
# write.csv(phytoplankton_lengths, "tables/NTL/phytoplankton/phytoplankton_lengths.csv", row.names = FALSE)
# 
# # Calculate trajectory lengths (relative to initial)
# phytoplankton_lengths_initial <- trajectoryLengths(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums, relativeToInitial = TRUE)
# phytoplankton_lengths_initial <- as.data.frame(phytoplankton_lengths_initial)
# phytoplankton_lengths_initial$site <- site_order
# write.csv(phytoplankton_lengths_initial, "tables/NTL/phytoplankton/phytoplankton_lengths_initial.csv", row.names = FALSE)
# 
# # Calculate trajectory angles (not relative to initial)
# phytoplankton_angles <- trajectoryAngles(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums)
# phytoplankton_angles <- as.data.frame(phytoplankton_angles)
# phytoplankton_angles$site <- site_order
# write.csv(phytoplankton_angles, "tables/NTL/phytoplankton/phytoplankton_angles.csv", row.names = FALSE)
# 
# # Calculate trajectory angles (relative to initial)
# phytoplankton_angles_initial <- trajectoryAngles(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums, relativeToInitial = TRUE)
# phytoplankton_angles_initial <- as.data.frame(phytoplankton_angles_initial)
# phytoplankton_angles_initial$site <- site_order
# write.csv(phytoplankton_angles_initial, "tables/NTL/phytoplankton/phytoplankton_angles_initial.csv", row.names = FALSE)
# 
# # # Calculate trajectory distances
# # phytoplankton_trajectory_distances <- trajectoryDistances(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums)
# # phytoplankton_trajectory_distances <- as.data.frame(phytoplankton_trajectory_distances)
# # phytoplankton_trajectory_distances$site <- site_order
# # write.csv(phytoplankton_trajectory_distances, "tables/NTL/phytoplankton/phytoplankton_trajectory_distances.csv", row.names = FALSE)
# 
# # Calculate trajectory convergence
# phytoplankton_convergence <- trajectoryConvergence(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums)
# phytoplankton_convergence <- as.data.frame(phytoplankton_convergence)
# phytoplankton_convergence$site <- site_order
# write.csv(phytoplankton_convergence, "tables/NTL/phytoplankton/phytoplankton_convergence.csv", row.names = FALSE)
# 
# # Calculate trajectory directionality
# phytoplankton_directionality <- trajectoryDirectionality(Dd, sites = ntl_phytoplankton_means$plot, surveys = ntl_phytoplankton_means$vector_nums)
# phytoplankton_directionality <- as.data.frame(phytoplankton_directionality)
# phytoplankton_directionality$site <- site_order
# write.csv(phytoplankton_directionality, "tables/NTL/phytoplankton/phytoplankton_directionality.csv", row.names = FALSE)


# Extract trajectory metrics for macroinvertss
# Calculate trajectory lengths (not relative to initial)
macroinverts_lengths <- trajectoryLengths(Di, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums)
macroinverts_lengths <- as.data.frame(macroinverts_lengths)
macroinverts_lengths$site <- site_order_mac
write.csv(macroinverts_lengths, "tables/NTL/macroinverts/macroinverts_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
macroinverts_lengths_initial <- trajectoryLengths(Di, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums, relativeToInitial = TRUE)
macroinverts_lengths_initial <- as.data.frame(macroinverts_lengths_initial)
macroinverts_lengths_initial$site <- site_order_mac
write.csv(macroinverts_lengths_initial, "tables/NTL/macroinverts/macroinverts_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
macroinverts_angles <- trajectoryAngles(Di, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums)
macroinverts_angles <- as.data.frame(macroinverts_angles)
macroinverts_angles$site <- site_order_mac
write.csv(macroinverts_angles, "tables/NTL/macroinverts/macroinverts_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
macroinverts_angles_initial <- trajectoryAngles(Di, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums, relativeToInitial = TRUE)
macroinverts_angles_initial <- as.data.frame(macroinverts_angles_initial)
macroinverts_angles_initial$site <- site_order_mac
write.csv(macroinverts_angles_initial, "tables/NTL/macroinverts/macroinverts_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# macroinverts_trajectory_distances <- trajectoryDistances(Dd, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums)
# macroinverts_trajectory_distances <- as.data.frame(macroinverts_trajectory_distances)
# macroinverts_trajectory_distances$site <- site_order_mac
# write.csv(macroinverts_trajectory_distances, "tables/NTL/macroinverts/macroinverts_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
macroinverts_convergence <- trajectoryConvergence(Di, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums)
macroinverts_convergence <- as.data.frame(macroinverts_convergence)
macroinverts_convergence$site <- site_order_mac
write.csv(macroinverts_convergence, "tables/NTL/macroinverts/macroinverts_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
macroinverts_directionality <- trajectoryDirectionality(Di, sites = ntl_macroinverts_means$plot, surveys = ntl_macroinverts_means$vector_nums)
macroinverts_directionality <- as.data.frame(macroinverts_directionality)
macroinverts_directionality$site <- site_order_mac
write.csv(macroinverts_directionality, "tables/NTL/macroinverts/macroinverts_directionality.csv", row.names = FALSE)
