#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author: James Sturges
#### Last Updated: October 9th, 2024

#### Required packages ----

# Load necessary libraries
# Un-comment line 15 to install librarian if necessary
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, data.table, ecotraj)

#### Read in data ----
# will update to a google drive pathway as we move forward
# Data provided from an LTER Synthesis Working Group lead by Mack White
# EDI: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.172.2
FCE <- fread("data/harmonized_consumer_excretion_CLEAN.csv", 
             select = c("project", "year", "site", "subsite_level1", "subsite_level2",  "species", "common_name", "dmperind_g/ind", "density_num/m", "density_num/m2"), # if you want specific columns
             colClasses = list(character = "project"))[
               project == "FCE"]
#### Data wrangling ----
# creates discrete unit of site replication evaluated in CTA

FCE = FCE %>% 
  mutate(drainage = as.character(site),
         site = as.character(subsite_level1),
         bout = as.character(subsite_level2))
# A combination of draingage, site, and bout columns will be the ID'ing variable
FCE = FCE %>% 
  mutate(drainage.site.bout = paste0(drainage,site,"B",bout))

# Always check the species observations before deciding on assemblage composition
# Rare species will impact ordinations
spp_n_all_time = FCE %>% 
  filter(`dmperind_g/ind` > 0) %>%
  group_by(common_name, species) %>% 
  count(common_name, sort = T) %>% 
  arrange(desc(n))


#### Creating Assemblage Vectors ----

# define your assemblage(s) of interest 
# taxonomic group, functional group, feeding guilds etc
prey = c("Bluegill", "Redear", "Spotted sunfish", "Dollar sunfish",
         "Striped mojarra", "Tidewater mojarra", "Bluespotted Sunfish")

predator = c("Florida gar", "Largemouth bass", "Snook", "Bowfin", "Redfish")


full = c("Bluegill", "Redear", "Spotted sunfish", "Dollar sunfish",
         "Striped mojarra", "Tidewater mojarra", "Bluespotted Sunfish",
         "Florida gar", "Largemouth bass", "Snook", "Bowfin", "Redfish",
         "Hogchoker", "Peacock eel", "Striped mullet", "Gray snapper",
         "Blue tilapia", "Mayan cichlid", "American eel")

# filter out the main dataframe so you have assemblage specific tibbles
df_full = FCE %>% 
  filter(common_name %in% full)

df_prey = FCE %>% 
  filter(common_name %in% prey)

df_predator = FCE %>% 
  filter(common_name %in% predator)


#### Create Community Matrices ----
community_matrix_full <- df_full %>%
  group_by(drainage, site, year, common_name) %>%
  summarise(sum_dmperind = sum(`dmperind_g/ind`, na.rm = TRUE)) %>%  
  ungroup() %>%
  pivot_wider(names_from = common_name, values_from = sum_dmperind, values_fill = 0)

community_matrix_prey <- df_prey %>%
  group_by(drainage, site, year, common_name) %>%
  summarise(sum_dmperind = sum(`dmperind_g/ind`, na.rm = TRUE)) %>%  
  ungroup() %>%
  pivot_wider(names_from = common_name, values_from = sum_dmperind, values_fill = 0)

community_matrix_predator <- df_predator %>%
  group_by(drainage, site, year, common_name) %>%
  summarise(sum_dmperind = sum(`dmperind_g/ind`, na.rm = TRUE)) %>%  
  ungroup() %>%
  pivot_wider(names_from = common_name, values_from = sum_dmperind, values_fill = 0)


# create ID column that will become row names
community_matrix_full = community_matrix_full %>% 
  mutate(drainage.site.year = paste(drainage, site, year, sep = "-"))

community_matrix_prey = community_matrix_prey %>% 
  mutate(drainage.site.year = paste(drainage, site, year, sep = "-"))

community_matrix_predator = community_matrix_predator %>% 
  mutate(drainage.site.year = paste(drainage, site, year, sep = "-"))


#create the matrix with proper row names
community_matrix_full_nmds = community_matrix_full %>% 
  column_to_rownames(var = "drainage.site.year")

community_matrix_full_nmds = community_matrix_full_nmds %>%
  dplyr::select(-drainage, -site, -year)


community_matrix_prey_nmds = community_matrix_prey %>% 
  column_to_rownames(var = "drainage.site.year")

community_matrix_prey_nmds = community_matrix_prey_nmds %>%
  dplyr::select(-drainage, -site, -year)


community_matrix_predator_nmds = community_matrix_predator %>% 
  column_to_rownames(var = "drainage.site.year")

community_matrix_predator_nmds = community_matrix_predator_nmds %>% 
  dplyr::select(-drainage, -site, -year)

core = c("8", "9", "10", "11", "13")

comm_best_sites = community_matrix_full %>%
  filter(site %in% core) %>% 
  column_to_rownames(var = "drainage.site.year")

comm_best_sites = comm_best_sites %>% 
  dplyr::select(-drainage, -site, -year)  
  
D <- vegan::vegdist(comm_best_sites,"bray")

community_matrix_full = community_matrix_full %>% 
  column_to_rownames(var = "drainage.site.year")

comm_best_sites = comm_best_sites %>% 
  left_join(community_matrix_full)

comm_best_sites = comm_best_sites %>%
  mutate(drainage.site.year = paste(drainage, site, year, sep = "_"))

comm_best_sites = comm_best_sites %>% 
  column_to_rownames(var = "drainage.site.year")

custom_palette <- c("#9966ff", "#3399ff", "#003300", "#990000", "#ff7f0e") 

# Generate the PCoA results manually
pcoa_result <- vegan::metaMDS(D, eig = TRUE, k = 2)  # k = 2 for 2D

# Get the site coordinates for plotting
site_coords <- as.data.frame(pcoa_result$points)

# Add drainage, site, and year for labeling
site_coords$label <- rownames(comm_best_sites)  # Use rownames as labels (drainage, site, year)

# Generate the plot using trajectoryPCoA
trajectoryPCoA(D, sites = comm_best_sites$site, surveys = comm_best_sites$year, 
               traj.colors = custom_palette, lwd = 2)

# Add the labels to the plot at the PCoA coordinates
text(site_coords$MDS1, site_coords$MDS2, labels = site_coords$label, pos = 3, cex = 0.8)



# Capture the plot output
trajectory_plot_1 <- recordPlot()

# Save the plot as a PNG file
png(filename = "figures/full_assem_trajectory_plot.png", width = 8, height = 8, res = 300, units = "in")
# trajectory_plot_1
dev.off()
