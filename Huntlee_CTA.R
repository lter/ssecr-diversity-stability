# Project Information ----
# SSECR: Diversity-Stability Relationships
# Authors: James Sturges & Noam-Altman-Kurosaki
# Last Updated: November 4th, 2024

# Data Frame Generation ----

# install.packages(librarian)
librarian::shelf(tidyverse, googledrive, data.table, ecotraj, vegan, lterdatasampler, supportR, cowplot, summarytools, datacleanr)

# read data directly from EDI repo
data_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.437.2&entityid=d9c5d73d11fc6a16ec41139b63b27751"
data_long <- read.csv(file = data_url) # should be 217512 obs, 11 var

# calculate number of species per site
species_richness_per_site <- data_long %>%
  group_by(site) %>%
  summarise(num_species = n_distinct(species))
species_richness_per_site

#create a site vector to make site level dataframes
sites <- c("mcr", "sbc", "csun.usvi", "Main_Hawaiian_Islands", "Florida_Keys")

# Loop through each site and create a data frame for each
for (site_name in sites) {
  # Create a filtered data frame and assign it a unique name
  assign(paste0(gsub("\\.", "_", site_name), "_df"), subset(data_long, site == site_name))
}

# MCR ----

# check the structures of the df
glimpse(mcr_df)

# individual df's to explore stability trends of coral, algae, and combined communities
mcr_df_reef = mcr_df %>% 
  filter(guild == "coral")

mcr_df_algae = mcr_df %>% 
  filter(guild == "algae")

mcr_habitat_means <- mcr_df %>%
  group_by(year, habitat, species) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

mcr_coral_means <- mcr_df_reef %>%
  group_by(year, habitat, species) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

mcr_algae_means <- mcr_df_algae %>%
  group_by(year, habitat, species) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

mcr_plot_means <- mcr_df %>%
  group_by(year, habitat, species, plot) %>%
  summarise(mean_abundance = sqrt(mean(abundance, na.rm = TRUE)), .groups = "drop") %>%
  mutate(mean_abundance = ifelse(is.na(mean_abundance), 0, mean_abundance))

# View the resulting summary
glimpse(mcr_habitat_means)

mcr_habitat_means <- mcr_habitat_means %>%
  pivot_wider(names_from = species, values_from = mean_abundance, values_fill = list(mean_abundance = 0))
mcr_coral_means <- mcr_coral_means %>%
  pivot_wider(names_from = species, values_from = mean_abundance, values_fill = list(mean_abundance = 0))
mcr_algae_means <- mcr_algae_means %>%
  pivot_wider(names_from = species, values_from = mean_abundance, values_fill = list(mean_abundance = 0))
mcr_plot_means <- mcr_plot_means %>%
  pivot_wider(names_from = species, values_from = mean_abundance, values_fill = list(mean_abundance = 0))

mcr_habitat_means = mcr_habitat_means %>% 
  mutate(habitat.year = paste(habitat, year, sep = "-"))

mcr_comm_matrix = mcr_habitat_means %>% 
  column_to_rownames(var = "habitat.year")

mcr_comm_matrix = mcr_comm_matrix %>%
  dplyr::select(-habitat, -year)

mcr_coral_means = mcr_coral_means %>% 
  mutate(habitat.year = paste(habitat, year, sep = "-"))

mcr_coral_matrix = mcr_coral_means %>% 
  column_to_rownames(var = "habitat.year")

mcr_coral_matrix = mcr_coral_matrix %>%
  dplyr::select(-habitat, -year)


mcr_algae_means = mcr_algae_means %>% 
  mutate(habitat.year = paste(habitat, year, sep = "-"))

mcr_algae_matrix = mcr_algae_means %>% 
  column_to_rownames(var = "habitat.year")

mcr_algae_matrix = mcr_algae_matrix %>%
  dplyr::select(-habitat, -year)


mcr_plot_means = mcr_plot_means %>% 
  mutate(habitat.year.plot = paste(habitat, year, plot, sep = "-"))

mcr_plot_matrix = mcr_plot_means %>% 
  column_to_rownames(var = "habitat.year.plot")

mcr_plot_matrix = mcr_plot_matrix %>%
  dplyr::select(-habitat, -year, -plot)


# MCR NMDS ----

#doing this as a combined coral/algae community and as just coral or alage cover
nmds_comm <- metaMDS(mcr_comm_matrix)
nmds_coral <- metaMDS(mcr_coral_matrix)
nmds_algae <- metaMDS(mcr_algae_matrix)
nmds_plot_location <- metaMDS(mcr_plot_matrix)

# Extract NMDS coordinates
nmds_comm_coords <- nmds_comm$points
nmds_coral_coords <- nmds_coral$points
nmds_algae_coords <- nmds_algae$points
nmds_plot_coords <- nmds_plot_location$points

# Add site and year information to NMDS coordinates
nmds_comm_data <- data.frame(nmds_comm_coords, habitat.year = rownames(mcr_comm_matrix))
nmds_coral_data <- data.frame(nmds_coral_coords, habitat.year = rownames(mcr_coral_matrix))
nmds_algae_data <- data.frame(nmds_algae_coords, habitat.year = rownames(mcr_algae_matrix))
nmds_plot_data <- data.frame(nmds_plot_coords, habitat.year.plot = rownames(mcr_plot_matrix))


mcr_habitat_means = mcr_habitat_means %>% 
  left_join(nmds_comm_data)

mcr_habitat_means = mcr_habitat_means %>% 
  mutate(habitat.year = paste0(habitat,year))

mcr_coral_means = mcr_coral_means %>% 
  left_join(nmds_coral_data)

mcr_coral_means = mcr_coral_means %>% 
  mutate(habitat.year = paste0(habitat,year))

mcr_algae_means = mcr_algae_means %>% 
  left_join(nmds_algae_data)

mcr_algae_means = mcr_algae_means %>% 
  mutate(habitat.year = paste0(habitat,year))

mcr_plot_means = mcr_plot_means %>% 
  left_join(nmds_plot_data)

mcr_plot_means = mcr_plot_means %>% 
  mutate(habitat.year.plot = paste0(habitat,year,plot))

# Create NMDS plots for... 

# set levels should move this up in the pipeline eventually
mcr_habitat_means = mcr_habitat_means %>% 
  mutate(habitat = factor(habitat, levels = c('Fringing', 'Outer 10',
                                      "Outer 17","Backreef")))

mcr_coral_means = mcr_coral_means %>% 
  mutate(habitat = factor(habitat, levels = c('Fringing', 'Outer 10',
                                              "Outer 17","Backreef")))

mcr_algae_means = mcr_algae_means %>% 
  mutate(habitat = factor(habitat, levels = c('Fringing', 'Outer 10',
                                              "Outer 17","Backreef")))
# whole community
plot_nmds_comm <- ggplot(mcr_habitat_means, aes(x = MDS1,
                                          y = MDS2,
                                          color = habitat,
                                          label = year)) +
  geom_point(size = 3, aes(shape = as.factor(habitat))) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR Whole Community by Habitat", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#003300",  
                                "#990000",  
                                "#ff7f0e",  
                                "#000066")) + 
  scale_shape_manual(values = c(17,15,18,19)) +
  guides(shape = guide_legend(title = "Reef Habitat"), color = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_comm)

ggsave(filename = paste0("figures/NMDS", "/", "mcr_comm_nmds_plot", ".png"),
       plot = plot_nmds_comm, width = 8, height = 6, units = "in", dpi = 600)


# coral only
plot_nmds_coral <- ggplot(mcr_coral_means, aes(x = MDS1,
                                                y = MDS2,
                                                color = habitat,
                                                label = year)) +
  geom_point(size = 3, aes(shape = as.factor(habitat))) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR Coral Community by Habitat", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#003300",  
                                "#990000",  
                                "#ff7f0e",  
                                "#000066")) + 
  scale_shape_manual(values = c(17,15,18,19)) +
  guides(shape = guide_legend(title = "Reef Habitat"), color = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_coral)

ggsave(filename = paste0("figures/NMDS", "/", "mcr_coral_nmds_plot", ".png"),
       plot = plot_nmds_coral, width = 8, height = 6, units = "in", dpi = 600)

# algae only
plot_nmds_algae <- ggplot(mcr_algae_means, aes(x = MDS1,
                                               y = MDS2,
                                               color = habitat,
                                               label = year)) +
  geom_point(size = 3, aes(shape = as.factor(habitat))) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR Algae Community by Habitat", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#003300",  
                                "#990000",  
                                "#ff7f0e",  
                                "#000066")) + 
  scale_shape_manual(values = c(17,15,18,19)) +
  guides(shape = guide_legend(title = "Reef Habitat"), color = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_algae)

ggsave(filename = paste0("figures/NMDS", "/", "mcr_algae_nmds_plot", ".png"),
      plot = plot_nmds_algae, width = 8, height = 6, units = "in", dpi = 600)

# whole community by plot location

#adding a new label column so we can display year and plot location as text on NMDS
mcr_plot_means = mcr_plot_means %>%
  mutate(number_only = str_extract(plot, "\\d+"))
  
mcr_plot_means = mcr_plot_means %>% 
  mutate(nmds_text_lab = paste(year, number_only, sep = "-") )


plot_nmds_plot_location <- ggplot(mcr_plot_means, aes(x = MDS1,
                                               y = MDS2,
                                               color = habitat,
                                               label = nmds_text_lab)) +
  geom_point(size = 3, aes(shape = as.factor(habitat))) +
  geom_text(check_overlap = TRUE, size = 3, color = "black", vjust = 1.5) +
  labs(title = "MCR Whole Community by Plot", x = "NMDS1", y = "NMDS2") +
  scale_color_manual(values = c("#003300",  
                                "#990000",  
                                "#ff7f0e",  
                                "#000066")) + 
  scale_shape_manual(values = c(17,15,18,19)) +
  guides(shape = guide_legend(title = "Reef Habitat"), color = guide_legend(title = "Reef Habitat")) +
  theme_classic()

print(plot_nmds_plot_location)

ggsave(filename = paste0("figures/NMDS", "/", "mcr_algae_nmds_plot", ".png"),
       plot = plot_nmds_algae, width = 8, height = 6, units = "in", dpi = 600)


# MCR CTA ----

D <- vegan::vegdist(mcr_comm_matrix,"bray")

custom_palette <- c("#003300",  
                    "#990000",  
                    "#ff7f0e",  
                    "#000066") 

# Generate the plot using trajectoryPCoA

mcr_habitat_means = mcr_habitat_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D, sites = mcr_habitat_means$habitat, surveys = mcr_habitat_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
legend("topright",inset = c(0, -0.25), bty="n", legend=c("Fringing", "Outer 10", "Outer 17", "Backreef"),  
       col = custom_palette, lwd = 2, xpd = TRUE)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/mcr_comm_trajectory_plot.png", width = 8, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()

### coral CTA
D2 <- vegan::vegdist(mcr_coral_matrix,"bray")

custom_palette <- c("#003300",
                    "#990000",  
                    "#ff7f0e"  
                    ) 

# Generate the plot using trajectoryPCoA

mcr_coral_means = mcr_coral_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D2, sites = mcr_coral_means$habitat, surveys = mcr_coral_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
legend("bottomleft", inset = c(0.14, -0.05), bty="n", legend=c("Fringing", "Outer 10", "Outer 17"),  
       col = custom_palette, lwd = 2, xpd = TRUE)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/mcr_coral_trajectory_plot.png", width = 8, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()


### coral CTA
D3 <- vegan::vegdist(mcr_algae_matrix,"bray")

custom_palette <- c("#003300",  
                    "#990000",  
                    "#ff7f0e",  
                    "#000066") 

# Generate the plot using trajectoryPCoA

mcr_algae_means = mcr_algae_means %>% 
  mutate(vector_nums = year - min(year) + 1)


trajectoryPCoA(D3, sites = mcr_algae_means$habitat, surveys = mcr_algae_means$vector_nums, traj.colors = custom_palette, lwd = 2, survey.labels = T) 
legend("bottomright", inset = c(0, -0.2), bty="n", legend=c("Fringing", "Outer 10", "Outer 17","Backreef"),  
       col = custom_palette, lwd = 2, xpd = TRUE)

# Capture the plot output
trajectory_plot <- recordPlot()


# Save the plot as a PNG file
png(filename = "figures/CTA/mcr_algae_trajectory_plot.png", width = 8, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()


D <- vegan::vegdist(mcr_comm_matrix,"bray")

custom_palette <- c("#003300",  
                    "#990000",  
                    "#ff7f0e",  
                    "#000066") 

# whole community by plot location CTAs

D <- vegan::vegdist(mcr_comm_matrix, "bray")

# Define a custom color palette
custom_palette <- c("#003300",  
                    "#990000",  
                    "#ff7f0e",  
                    "#000066") 

# Adjust mcr_habitat_means to include both habitat and plot information
mcr_habitat_means <- mcr_habitat_means %>%
  mutate(vector_nums = year - min(year) + 1,
         habitat_plot = paste(habitat, plot, sep = "_"))  # Create unique habitat-plot identifier

# Generate the plot using trajectoryPCoA
trajectoryPCoA(
  D, 
  sites = mcr_habitat_means$habitat_plot,  # Use unique habitat-plot identifiers
  surveys = mcr_habitat_means$vector_nums, 
  traj.colors = custom_palette, 
  lwd = 2, 
  survey.labels = TRUE
) 

# Add a legend with custom colors for each habitat
legend("topright", inset = c(0, -0.25), bty = "n", 
       legend = c("Fringing", "Outer 10", "Outer 17", "Backreef"),  
       col = custom_palette, lwd = 2, xpd = TRUE)

# Capture the plot output
trajectory_plot <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/CTA/mcr_comm_trajectory_plot.png", width = 8, height = 8, res = 300, units = "in")
trajectory_plot
dev.off()