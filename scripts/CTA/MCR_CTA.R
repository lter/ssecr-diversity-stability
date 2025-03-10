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
mcr_algae = read.csv(file = "data/MCR/mcr_algae.csv")
mcr_fish = read.csv(file = "data/MCR/mcr_fish.csv")
mcr_invertebrate = read.csv(file = "data/MCR/mcr_invertebrate.csv")
mcr_macroalgae = read.csv(file = "data/MCR/mcr_macroalgae.csv")
# Richness Summary Tables ----

# Summary data for consumer species richness across sites
species_n_fish_site <- mcr_fish %>%
  group_by(plot, year, habitat_fine) %>%
  summarise(num_species = n_distinct(taxon_name))
# species_n_fish_site
write.csv(species_n_fish_site, "tables/MCR/summary/species_n_fish_site.csv",row.names = F)

species_n_invertebrate_site <- mcr_invertebrate %>%
  group_by(plot, year, habitat_fine) %>%
  summarise(num_species = n_distinct(taxon_name))
# species_n_invertebrate_site
write.csv(species_n_invertebrate_site, "tables/MCR/summary/species_n_invertebrate_site.csv",row.names = F)

# Summary data for producer species richness across sites
species_n_algae_site <- mcr_algae %>%
  group_by(plot, year, habitat_fine) %>%
  summarise(num_species = n_distinct(taxon_name))
# species_n_algae_site
write.csv(species_n_algae_site, "tables/KNZ/summary/species_n_algae_site.csv",row.names = F)

# Summary data for producer species richness across sites
species_n_macroalgae_site <- mcr_macroalgae %>%
  group_by(plot, year, habitat_fine) %>%
  summarise(num_species = n_distinct(taxon_name))
# species_n_macroalgae_site
write.csv(species_n_macroalgae_site, "tables/KNZ/summary/species_n_macroalgae_site.csv",row.names = F)

# Site Selection & Order ----
unique(mcr_fish$plot)
unique(mcr_invertebrate$plot)
unique(mcr_algae$plot)
unique(mcr_macroalgae$plot)

# primary producers and inverts only measured spatially with lter_1
# filtered mcr_fish to only one plot level (using dataset in SPARC2)

mcr_fish = mcr_fish %>% 
  filter(plot == "lter_1")

unique(mcr_fish$habitat_fine)
unique(mcr_invertebrate$habitat_fine)
unique(mcr_algae$habitat_fine)
unique(mcr_macroalgae$habitat_fine)

# mutating habitat of outer 17 and 10 to forereef in non fish datasets
# better spatial representation of biomass densities 
# also creating a second habitat variable to use originals later
mcr_fish <- mcr_fish %>%
  mutate(habitat_fine2 = case_when(
    habitat_fine %in% c("outer 10", "outer 17") ~ "forereef",
    TRUE ~ habitat_fine
  ))

mcr_invertebrate <- mcr_invertebrate %>%
  mutate(habitat_fine2 = case_when(
    habitat_fine %in% c("outer 10", "outer 17") ~ "forereef",
    TRUE ~ habitat_fine
  ))

mcr_algae <- mcr_algae %>%
  mutate(habitat_fine2 = case_when(
    habitat_fine %in% c("outer 10", "outer 17") ~ "forereef",
    TRUE ~ habitat_fine
  ))

mcr_macroalgae <- mcr_macroalgae %>%
  mutate(habitat_fine2 = case_when(
    habitat_fine %in% c("outer 10", "outer 17") ~ "forereef",
    TRUE ~ habitat_fine
  ))

site_order = c("backreef","fringing" ,"forereef")

# Survey History Summary Tables ----

# Summary data years of consumer assemblage data per site
site_years_count_fish <- mcr_fish %>%
  group_by(habitat_fine2) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(habitat_fine2)
# site_years_count_fish
write.csv(site_years_count_fish, "tables/KNZ/summary/site_years_count_fish.csv",row.names = F)

site_years_count_invertebrate <- mcr_invertebrate %>%
  group_by(habitat_fine2) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(habitat_fine2)
# site_years_count_invertebrate
write.csv(site_years_count_invertebrate, "tables/KNZ/summary/site_years_count_invertebrate.csv",row.names = F)
# Summary data years of producer assemblage data per site
site_years_count_algae <- mcr_algae %>%
  group_by(habitat_fine2) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(habitat_fine2)
# site_years_count_algae
write.csv(site_years_count_algae, "tables/KNZ/summary/site_years_count_algae.csv",row.names = F)

site_years_count_macroalgae <- mcr_macroalgae %>%
  group_by(habitat_fine2) %>%
  summarise(years_count = n_distinct(year)) %>%
  arrange(habitat_fine2)
# site_years_count_macroalgae
write.csv(site_years_count_macroalgae, "tables/KNZ/summary/site_years_count_macroalgae.csv",row.names = F)

# Summary data total number of observations of each consumer species
# and the number of plots that species has been observed in at least once
site_fish_spp_summary <- mcr_fish %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(habitat_fine2)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_fish_spp_summary
write.csv(site_fish_spp_summary, "tables/KNZ/summary/site_fish_spp_summary.csv",row.names = F)


site_invertebrate_spp_summary <- mcr_invertebrate %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(habitat_fine2)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_invertebrate_spp_summary
write.csv(site_invertebrate_spp_summary, "tables/KNZ/summary/site_invertebrate_spp_summary.csv",row.names = F)

site_algae_spp_summary <- mcr_algae %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(habitat_fine2)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_algae_spp_summary
write.csv(site_algae_spp_summary, "tables/KNZ/summary/site_algae_spp_summary.csv",row.names = F)

site_macroalgae_spp_summary <- mcr_macroalgae %>%
  group_by(taxon_name) %>%
  summarise(
    spp_sum = sum(abundance),  # Total abundance of the species
    unique_plots_detected = n_distinct(habitat_fine2)  # Count of unique plots
  ) %>%
  arrange(taxon_name)
site_macroalgae_spp_summary
write.csv(site_macroalgae_spp_summary, "tables/KNZ/summary/site_macroalgae_spp_summary.csv",row.names = F)

# calculate mean abundance across habitats and years
mcr_fish_means <- mcr_fish %>%
  group_by(year, plot, habitat_fine2, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

mcr_invertebrate_means <- mcr_invertebrate %>%
  group_by(year, plot, habitat_fine2, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

mcr_algae_means <- mcr_algae %>%
  group_by(year, plot, habitat_fine2, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))


mcr_macroalgae_means <- mcr_macroalgae %>%
  group_by(year, plot, habitat_fine2, taxon_name) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))


# Make 0 filled community inter annual matrix
mcr_fish_means <- mcr_fish_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

mcr_invertebrate_means <- mcr_invertebrate_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

mcr_algae_means <- mcr_algae_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

mcr_macroalgae_means <- mcr_macroalgae_means %>%
  pivot_wider(names_from = taxon_name, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

#create unique id variable for row names
mcr_fish_means = mcr_fish_means %>% 
  mutate(site.year = paste(habitat_fine2, year, sep = "-"))

mcr_invertebrate_means = mcr_invertebrate_means %>% 
  mutate(site.year = paste(habitat_fine2, year, sep = "-"))

mcr_algae_means = mcr_algae_means %>% 
  mutate(site.year = paste(habitat_fine2, year, sep = "-"))

mcr_macroalgae_means = mcr_macroalgae_means %>% 
  mutate(site.year = paste(habitat_fine2, year, sep = "-"))

# make unique row names for Vegan functions
mcr_fish_matrix = mcr_fish_means %>% 
  column_to_rownames(var = "site.year")

mcr_invertebrate_matrix = mcr_invertebrate_means %>% 
  column_to_rownames(var = "site.year")

mcr_algae_matrix = mcr_algae_means %>% 
  column_to_rownames(var = "site.year")

mcr_macroalgae_matrix = mcr_macroalgae_means %>% 
  column_to_rownames(var = "site.year")



mcr_fish_matrix = mcr_fish_matrix %>%
  dplyr::select(-plot, -year, -habitat_fine2)

mcr_invertebrate_matrix = mcr_invertebrate_matrix %>%
  dplyr::select(-plot, -year, -habitat_fine2)

mcr_algae_matrix = mcr_algae_matrix %>%
  dplyr::select(-plot, -year, -habitat_fine2)

mcr_macroalgae_matrix = mcr_macroalgae_matrix %>%
  dplyr::select(-plot, -year, -habitat_fine2)

# MCR NMDS ----

#doing this as a combined coral/algae community and as just coral or alage cover
nmds_fish <- metaMDS(mcr_fish_matrix)
nmds_invertebrate <- metaMDS(mcr_invertebrate_matrix)
nmds_algae <- metaMDS(mcr_algae_matrix)
nmds_macroalgae <- metaMDS(mcr_macroalgae_matrix)

# Extract NMDS coordinates
nmds_fish_coords <- nmds_fish$points
nmds_invertebrate_coords <- nmds_invertebrate$points
nmds_algae_coords <- nmds_algae$points
nmds_macroalgae_coords <- nmds_macroalgae$points

# Add site and year information to NMDS coordinates
nmds_fish_data <- data.frame(nmds_fish_coords, site.year = rownames(mcr_fish_matrix))
nmds_invertebrate_data <- data.frame(nmds_invertebrate_coords, site.year = rownames(mcr_invertebrate_matrix))
nmds_algae_data <- data.frame(nmds_algae_coords, site.year = rownames(mcr_algae_matrix))
nmds_macroalgae_data <- data.frame(nmds_macroalgae_coords, site.year = rownames(mcr_macroalgae_matrix))


mcr_fish_means = mcr_fish_means %>% 
  left_join(nmds_fish_data)

mcr_fish_means = mcr_fish_means %>% 
  mutate(site.year = paste0(plot,year))

mcr_invertebrate_means = mcr_invertebrate_means %>% 
  left_join(nmds_invertebrate_data)

mcr_invertebrate_means = mcr_invertebrate_means %>% 
  mutate(site.year = paste0(plot,year))

mcr_algae_means = mcr_algae_means %>% 
  left_join(nmds_algae_data)

mcr_algae_means = mcr_algae_means %>% 
  mutate(site.year = paste0(plot,year))

mcr_macroalgae_means = mcr_macroalgae_means %>% 
  left_join(nmds_macroalgae_data)

mcr_macroalgae_means = mcr_macroalgae_means %>% 
  mutate(site.year = paste0(plot,year))

# Create NMDS plots for... 

# consumers
plot_nmds_fish <- ggplot(mcr_fish_means, aes(x = MDS1,
                                                     y = MDS2,
                                                     color = habitat_fine2,
                                                     shape = habitat_fine2,
                                                     label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR Fish Community by Reef Habitat", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#009E73", "#0072B2", "#E69F00")) +
  scale_shape_manual(values = c(15,16,17)) +
  guides(color = guide_legend(title = "Reef Habitat"),
         shape = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_fish)

ggsave(filename = paste0("figures/NMDS/MCR", "/", "plot_nmds_fish", ".png"),
       plot = plot_nmds_fish, width = 8, height = 6, units = "in", dpi = 600)

plot_nmds_invertebrate <- ggplot(mcr_invertebrate_means, aes(x = MDS1,
                                             y = MDS2,
                                             color = habitat_fine2,
                                             shape = habitat_fine2,
                                             label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR invertebrate Community by Reef Habitat", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#009E73", "#0072B2", "#E69F00")) +
  scale_shape_manual(values = c(15,16,17)) +
  guides(color = guide_legend(title = "Reef Habitat"),
         shape = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_invertebrate)

ggsave(filename = paste0("figures/NMDS/MCR", "/", "plot_nmds_invertebrate", ".png"),
       plot = plot_nmds_invertebrate, width = 8, height = 6, units = "in", dpi = 600)

plot_nmds_algae <- ggplot(mcr_algae_means, aes(x = MDS1,
                                             y = MDS2,
                                             color = habitat_fine2,
                                             shape = habitat_fine2,
                                             label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR algae Community by Reef Habitat", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#009E73", "#0072B2", "#E69F00"))+
  scale_shape_manual(values = c(15,16,17)) +
  guides(color = guide_legend(title = "Reef Habitat"),
         shape = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_algae)

ggsave(filename = paste0("figures/NMDS/MCR", "/", "plot_nmds_algae", ".png"),
       plot = plot_nmds_algae, width = 8, height = 6, units = "in", dpi = 600)

plot_nmds_macroalgae <- ggplot(mcr_macroalgae_means, aes(x = MDS1,
                                             y = MDS2,
                                             color = habitat_fine2,
                                             shape = habitat_fine2,
                                             label = year)) +
  geom_point(size = 3) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR macroalgae Community by Reef Habitat", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#009E73", "#0072B2", "#E69F00")) +
  scale_shape_manual(values = c(15,16,17)) +
  guides(color = guide_legend(title = "Reef Habitat"),
         shape = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_macroalgae)

ggsave(filename = paste0("figures/NMDS/MCR", "/", "plot_nmds_macroalgae", ".png"),
       plot = plot_nmds_macroalgae, width = 8, height = 6, units = "in", dpi = 600)
# MCR CTA ----

#fish
Dfish <- vegan::vegdist(mcr_fish_matrix,"bray")

custom_palette <- c(
  "#009E73", "#0072B2", "#E69F00"
) 

# Generate the plot using trajectoryPCoA

mcr_fish_means = mcr_fish_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.2, x.intersp = 0.5, "topright", inset = c(0.15, 0),  bty="n", legend=c("Backreef", "Forereef", "Fringing"),  col = c("#009E73", "#0072B2", "#E69F00"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/MCR/mcr_fish_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

#invertebrates
Dinvertebrate <- vegan::vegdist(mcr_invertebrate_matrix,"bray")

custom_palette <- c(
  "#009E73", "#0072B2", "#E69F00"
) 

# Generate the plot using trajectoryPCoA

mcr_invertebrate_means = mcr_invertebrate_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.2, x.intersp = 0.5, "topright", inset = c(0.15, 0),  bty="n", legend=c("Backreef", "Forereef", "Fringing"),  col = c("#009E73", "#0072B2", "#E69F00"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/MCR/mcr_invertebrate_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

#algae
Dalgae <- vegan::vegdist(mcr_algae_matrix,"bray")

custom_palette <- c(
  "#009E73", "#0072B2", "#E69F00"
) 

# Generate the plot using trajectoryPCoA

mcr_algae_means = mcr_algae_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.2, x.intersp = 0.5, "topright", inset = c(0.15, 0),  bty="n", legend=c("Backreef", "Forereef", "Fringing"),  col = c("#009E73", "#0072B2", "#E69F00"), lwd=2)


# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/MCR/mcr_algae_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

#macroalgae
Dmacroalgae <- vegan::vegdist(mcr_macroalgae_matrix,"bray")

custom_palette <- c(
  "#009E73", "#0072B2", "#E69F00"
) 

# Generate the plot using trajectoryPCoA

mcr_macroalgae_means = mcr_macroalgae_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums, traj.colors = custom_palette, lwd = 1, survey.labels = T) 
legend(ncol = 1, y.intersp = 0.2, x.intersp = 0.5, "topright", inset = c(0.15, 0),  bty="n", legend=c("Backreef", "Forereef", "Fringing"),  col = c("#009E73", "#0072B2", "#E69F00"), lwd=2)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/MCR/mcr_macroalgae_trajectory_plot.png", width = 12, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()
# Individual Sites ----

#fish assemblage
# Define a list of unique sites
unique_sites <- unique(mcr_fish$habitat_fine2)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#009E73", "#0072B2", "#E69F00"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- mcr_fish_means %>% 
    filter(habitat_fine2 == site)
  
  # Prepare the site data for distance matrix calculation
  site_data <- site_raw %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums, -habitat_fine2)
  
  # Compute Bray-Curtis distance matrix
  Dfish2 <- vegan::vegdist(site_data, "bray")
  
  # Calculate segment lengths using trajectoryLengths
  segment_lengths <- trajectoryLengths(Dfish2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums)
  
  # Create a data frame for the segment lengths
  segment_lengths_df <- data.frame(
    Segment = seq_along(segment_lengths),
    Length = segment_lengths
  )
  
  # Save segment lengths as a CSV file
  csv_filename <- paste0("tables/MCR/sites/mcr_fish_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/MCR/mcr_fish_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(Dfish2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}


#invertebrate assemblage
# Define a list of unique sites
unique_sites <- unique(mcr_invertebrate$habitat_fine2)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#009E73", "#0072B2", "#E69F00"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- mcr_invertebrate_means %>% 
    filter(habitat_fine2 == site)
  
  # Prepare the site data for distance matrix calculation
  site_data <- site_raw %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums, -habitat_fine2)
  
  # Compute Bray-Curtis distance matrix
  Dinvertebrate2 <- vegan::vegdist(site_data, "bray")
  
  # Calculate segment lengths using trajectoryLengths
  segment_lengths <- trajectoryLengths(Dinvertebrate2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums)
  
  # Create a data frame for the segment lengths
  segment_lengths_df <- data.frame(
    Segment = seq_along(segment_lengths),
    Length = segment_lengths
  )
  
  # Save segment lengths as a CSV file
  csv_filename <- paste0("tables/MCR/sites/mcr_invertebrate_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/MCR/mcr_invertebrate_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(Dinvertebrate2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}


#algae assemblage
# Define a list of unique sites
unique_sites <- unique(mcr_algae$habitat_fine2)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#009E73", "#0072B2", "#E69F00"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- mcr_algae_means %>% 
    filter(habitat_fine2 == site)
  
  # Prepare the site data for distance matrix calculation
  site_data <- site_raw %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums, -habitat_fine2)
  
  # Compute Bray-Curtis distance matrix
  Dalgae2 <- vegan::vegdist(site_data, "bray")
  
  # Calculate segment lengths using trajectoryLengths
  segment_lengths <- trajectoryLengths(Dalgae2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums)
  
  # Create a data frame for the segment lengths
  segment_lengths_df <- data.frame(
    Segment = seq_along(segment_lengths),
    Length = segment_lengths
  )
  
  # Save segment lengths as a CSV file
  csv_filename <- paste0("tables/MCR/sites/mcr_algae_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/MCR/mcr_algae_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(Dalgae2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}



#macroalgae assemblage
# Define a list of unique sites
unique_sites <- unique(mcr_macroalgae$habitat_fine2)

# Loop through each site
# Define a vector of colors (add more as needed)
color_vector <- c(
  "#009E73", "#0072B2", "#E69F00"
)   # Extend this list as more sites are added

for (site in unique_sites) {
  
  # Get the index of the site and its corresponding color
  site_index <- which(unique_sites == site)
  site_color <- color_vector[site_index]
  
  # Filter data for the current site
  site_raw <- mcr_macroalgae_means %>% 
    filter(habitat_fine2 == site)
  
  # Prepare the site data for distance matrix calculation
  site_data <- site_raw %>% 
    column_to_rownames(var = "site.year") %>%
    dplyr::select(-plot, -year, -MDS1, -MDS2, -vector_nums, -habitat_fine2)
  
  # Compute Bray-Curtis distance matrix
  Dmacroalgae2 <- vegan::vegdist(site_data, "bray")
  
  # Calculate segment lengths using trajectoryLengths
  segment_lengths <- trajectoryLengths(Dmacroalgae2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums)
  
  # Create a data frame for the segment lengths
  segment_lengths_df <- data.frame(
    Segment = seq_along(segment_lengths),
    Length = segment_lengths
  )
  
  # Save segment lengths as a CSV file
  csv_filename <- paste0("tables/MCR/sites/mcr_macroalgae_segment_lengths_", site, ".csv")
  write.csv(segment_lengths_df, csv_filename, row.names = FALSE)
  
  # Generate and save the trajectory plot
  plot_filename <- paste0("figures/CTA/MCR/mcr_macroalgae_trajectory_plot_", site, ".png")
  png(filename = plot_filename, width = 8, height = 8, res = 300, units = "in")
  
  # Plot the trajectory with specific site color
  trajectoryPCoA(Dmacroalgae2, sites = site_raw$habitat_fine2, surveys = site_raw$vector_nums, 
                 traj.colors = site_color, lwd = 2, survey.labels = TRUE)
  
  # Close the PNG device to save the plot
  dev.off()
}




# CTA Metrics ----

# Extract trajectory metrics for fish
# Calculate trajectory lengths (not relative to initial)
fish_lengths <- trajectoryLengths(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums)
fish_lengths <- as.data.frame(fish_lengths)
fish_lengths$site <- site_order  
write.csv(fish_lengths, "tables/MCR/fish/fish_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
fish_lengths_initial <- trajectoryLengths(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums, relativeToInitial = TRUE)
fish_lengths_initial <- as.data.frame(fish_lengths_initial)
fish_lengths_initial$site <- site_order
write.csv(fish_lengths_initial, "tables/MCR/fish/fish_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
fish_angles <- trajectoryAngles(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums)
fish_angles <- as.data.frame(fish_angles)
fish_angles$site <- site_order
write.csv(fish_angles, "tables/MCR/fish/fish_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
fish_angles_initial <- trajectoryAngles(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums, relativeToInitial = TRUE)
fish_angles_initial <- as.data.frame(fish_angles_initial)
fish_angles_initial$site <- site_order
write.csv(fish_angles_initial, "tables/MCR/fish/fish_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# fish_trajectory_distances <- trajectoryDistances(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums)
# fish_trajectory_distances <- as.data.frame(fish_trajectory_distances)
# fish_trajectory_distances$site <- site_order
# write.csv(fish_trajectory_distances, "tables/MCR/fish/fish_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
fish_convergence <- trajectoryConvergence(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums)
fish_convergence <- as.data.frame(fish_convergence)
fish_convergence$site <- site_order
write.csv(fish_convergence, "tables/MCR/fish/fish_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
fish_directionality <- trajectoryDirectionality(Dfish, sites = mcr_fish_means$habitat_fine2, surveys = mcr_fish_means$vector_nums)
fish_directionality <- as.data.frame(fish_directionality)
fish_directionality$site <- site_order
write.csv(fish_directionality, "tables/MCR/fish/fish_directionality.csv", row.names = FALSE)


# Extract trajectory metrics for invertebrate
# Calculate trajectory lengths (not relative to initial)
invertebrate_lengths <- trajectoryLengths(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums)
invertebrate_lengths <- as.data.frame(invertebrate_lengths)
invertebrate_lengths$site <- site_order  
write.csv(invertebrate_lengths, "tables/MCR/invertebrate/invertebrate_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
invertebrate_lengths_initial <- trajectoryLengths(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums, relativeToInitial = TRUE)
invertebrate_lengths_initial <- as.data.frame(invertebrate_lengths_initial)
invertebrate_lengths_initial$site <- site_order
write.csv(invertebrate_lengths_initial, "tables/MCR/invertebrate/invertebrate_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
invertebrate_angles <- trajectoryAngles(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums)
invertebrate_angles <- as.data.frame(invertebrate_angles)
invertebrate_angles$site <- site_order
write.csv(invertebrate_angles, "tables/MCR/invertebrate/invertebrate_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
invertebrate_angles_initial <- trajectoryAngles(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums, relativeToInitial = TRUE)
invertebrate_angles_initial <- as.data.frame(invertebrate_angles_initial)
invertebrate_angles_initial$site <- site_order
write.csv(invertebrate_angles_initial, "tables/MCR/invertebrate/invertebrate_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# invertebrate_trajectory_distances <- trajectoryDistances(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums)
# invertebrate_trajectory_distances <- as.data.frame(invertebrate_trajectory_distances)
# invertebrate_trajectory_distances$site <- site_order
# write.csv(invertebrate_trajectory_distances, "tables/MCR/invertebrate/invertebrate_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
invertebrate_convergence <- trajectoryConvergence(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums)
invertebrate_convergence <- as.data.frame(invertebrate_convergence)
invertebrate_convergence$site <- site_order
write.csv(invertebrate_convergence, "tables/MCR/invertebrate/invertebrate_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
invertebrate_directionality <- trajectoryDirectionality(Dinvertebrate, sites = mcr_invertebrate_means$habitat_fine2, surveys = mcr_invertebrate_means$vector_nums)
invertebrate_directionality <- as.data.frame(invertebrate_directionality)
invertebrate_directionality$site <- site_order
write.csv(invertebrate_directionality, "tables/MCR/invertebrate/invertebrate_directionality.csv", row.names = FALSE)



# Extract trajectory metrics for algae
# Calculate trajectory lengths (not relative to initial)
algae_lengths <- trajectoryLengths(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums)
algae_lengths <- as.data.frame(algae_lengths)
algae_lengths$site <- site_order  
write.csv(algae_lengths, "tables/MCR/algae/algae_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
algae_lengths_initial <- trajectoryLengths(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums, relativeToInitial = TRUE)
algae_lengths_initial <- as.data.frame(algae_lengths_initial)
algae_lengths_initial$site <- site_order
write.csv(algae_lengths_initial, "tables/MCR/algae/algae_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
algae_angles <- trajectoryAngles(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums)
algae_angles <- as.data.frame(algae_angles)
algae_angles$site <- site_order
write.csv(algae_angles, "tables/MCR/algae/algae_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
algae_angles_initial <- trajectoryAngles(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums, relativeToInitial = TRUE)
algae_angles_initial <- as.data.frame(algae_angles_initial)
algae_angles_initial$site <- site_order
write.csv(algae_angles_initial, "tables/MCR/algae/algae_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# algae_trajectory_distances <- trajectoryDistances(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums)
# algae_trajectory_distances <- as.data.frame(algae_trajectory_distances)
# algae_trajectory_distances$site <- site_order
# write.csv(algae_trajectory_distances, "tables/MCR/algae/algae_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
algae_convergence <- trajectoryConvergence(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums)
algae_convergence <- as.data.frame(algae_convergence)
algae_convergence$site <- site_order
write.csv(algae_convergence, "tables/MCR/algae/algae_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
algae_directionality <- trajectoryDirectionality(Dalgae, sites = mcr_algae_means$habitat_fine2, surveys = mcr_algae_means$vector_nums)
algae_directionality <- as.data.frame(algae_directionality)
algae_directionality$site <- site_order
write.csv(algae_directionality, "tables/MCR/algae/algae_directionality.csv", row.names = FALSE)

# Extract trajectory metrics for macroalgae
# Calculate trajectory lengths (not relative to initial)
macroalgae_lengths <- trajectoryLengths(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums)
macroalgae_lengths <- as.data.frame(macroalgae_lengths)
macroalgae_lengths$site <- site_order  
write.csv(macroalgae_lengths, "tables/MCR/macroalgae/macroalgae_lengths.csv", row.names = FALSE)

# Calculate trajectory lengths (relative to initial)
macroalgae_lengths_initial <- trajectoryLengths(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums, relativeToInitial = TRUE)
macroalgae_lengths_initial <- as.data.frame(macroalgae_lengths_initial)
macroalgae_lengths_initial$site <- site_order
write.csv(macroalgae_lengths_initial, "tables/MCR/macroalgae/macroalgae_lengths_initial.csv", row.names = FALSE)

# Calculate trajectory angles (not relative to initial)
macroalgae_angles <- trajectoryAngles(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums)
macroalgae_angles <- as.data.frame(macroalgae_angles)
macroalgae_angles$site <- site_order
write.csv(macroalgae_angles, "tables/MCR/macroalgae/macroalgae_angles.csv", row.names = FALSE)

# Calculate trajectory angles (relative to initial)
macroalgae_angles_initial <- trajectoryAngles(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums, relativeToInitial = TRUE)
macroalgae_angles_initial <- as.data.frame(macroalgae_angles_initial)
macroalgae_angles_initial$site <- site_order
write.csv(macroalgae_angles_initial, "tables/MCR/macroalgae/macroalgae_angles_initial.csv", row.names = FALSE)

# # Calculate trajectory distances
# macroalgae_trajectory_distances <- trajectoryDistances(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums)
# macroalgae_trajectory_distances <- as.data.frame(macroalgae_trajectory_distances)
# macroalgae_trajectory_distances$site <- site_order
# write.csv(macroalgae_trajectory_distances, "tables/MCR/macroalgae/macroalgae_trajectory_distances.csv", row.names = FALSE)

# Calculate trajectory convergence
macroalgae_convergence <- trajectoryConvergence(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums)
macroalgae_convergence <- as.data.frame(macroalgae_convergence)
macroalgae_convergence$site <- site_order
write.csv(macroalgae_convergence, "tables/MCR/macroalgae/macroalgae_convergence.csv", row.names = FALSE)

# Calculate trajectory directionality
macroalgae_directionality <- trajectoryDirectionality(Dmacroalgae, sites = mcr_macroalgae_means$habitat_fine2, surveys = mcr_macroalgae_means$vector_nums)
macroalgae_directionality <- as.data.frame(macroalgae_directionality)
macroalgae_directionality$site <- site_order
write.csv(macroalgae_directionality, "tables/MCR/macroalgae/macroalgae_directionality.csv", row.names = FALSE)
