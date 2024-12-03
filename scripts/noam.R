#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Noam Altman-Kurosaki
#### Last Updated: November 7th, 2024
librarian::shelf(vegan, lme4, dplyr, tidyr, lattice, lavaan, ggplot2)
source("00_test-data.R")

# function for z standardization
z_standard <- function(x){
  (x - mean(x))/sd(x)
}

# coefficient of variation
CV<-function(x){
  return(sd(x,na.rm=T)/mean(x,na.rm=T))
}

# stability
stability <- function(x){
  1/CV(x)
}

# create fake data for Nick example
## I ADAPTED THIS CODE FROM CHAT GPT!!! USE AT YOUR OWN RISK!!!

simulate_habitat <- function(habitat_id, years, producer_range, consumer_range, producer_dist, consumer_dist, 
                             lambda_min, lambda_max, meanlog, sdlog, shape1, shape2, num_transects) {
  # Generate species pools
  num_producers <- sample(producer_range, 1)
  num_consumers <- sample(consumer_range, 1)
  producer_species_pool <- paste0("P", 1:num_producers, "_H", habitat_id)
  consumer_species_pool <- paste0("C", 1:num_consumers, "_H", habitat_id)
  
  # Initialize empty list to store data for each year and transect
  habitat_data <- list()
  
  for (year in years) {
    for (transect_id in 1:num_transects) {
      # Randomly select present species for the transect and year
      num_producers_present <- sample(1:num_producers, 1)
      num_consumers_present <- sample(1:num_consumers, 1)
      producers_present <- sample(producer_species_pool, num_producers_present)
      consumers_present <- sample(consumer_species_pool, num_consumers_present)
      
      # Simulate producers
      producers <- expand.grid(
        year = year,
        habitat = paste0("Habitat_", habitat_id),
        trophic_level = "Producer",
        measurement = producer_dist,
        species = producers_present,
        transect_ID = paste0("Habitat_", habitat_id, "-T", transect_id)
      )
      
      # Generate abundance for producers
      if (producer_dist == "percent_cover") {
        repeat {
          # Generate random percent cover values
          producers$abundance <- rbeta(nrow(producers), shape1 = shape1, shape2 = shape2)
          # Check if total percent cover is <= 1
          if (sum(producers$abundance) <= 1) break
        }
      } else if (producer_dist == "count") {
        producers$abundance <- rpois(nrow(producers), lambda = sample(seq(lambda_min, lambda_max, 0.1), 1))
      } else if (producer_dist == "biomass") {
        if (is.na(meanlog) || is.na(sdlog)) {
          stop("meanlog and sdlog must be specified for biomass distribution.")
        }
        producers$abundance <- rlnorm(nrow(producers), meanlog = meanlog, sdlog = sdlog)
      }
      
      # Simulate consumers
      consumers <- expand.grid(
        year = year,
        habitat = paste0("Habitat_", habitat_id),
        trophic_level = "Consumer",
        measurement = consumer_dist,
        species = consumers_present,
        transect_ID = paste0("Habitat_", habitat_id, "-T", transect_id)
      )
      consumers$abundance <- if (consumer_dist == "count") {
        rpois(nrow(consumers), lambda = sample(seq(lambda_min, lambda_max, 0.1), 1))
      } else if (consumer_dist == "biomass") {
        if (is.na(meanlog) || is.na(sdlog)) {
          stop("meanlog and sdlog must be specified for biomass distribution.")
        }
        rlnorm(nrow(consumers), meanlog = meanlog, sdlog = sdlog)
      } 
      
      # Combine producers and consumers for the transect
      habitat_data[[paste(year, transect_id)]] <- bind_rows(producers, consumers)
    }
  }
  
  # Combine all transects and years into one dataframe
  bind_rows(habitat_data)
}




habitat_details <- list(
  list(habitat_id = 1, years = 2006:2018, producer_range = 1:5, consumer_range = 2:3, 
       producer_dist = "percent_cover", consumer_dist = "count", shape1 = 1, shape2 = 5,
       lambda_min = 10, lambda_max = 20, meanlog = NA, sdlog = NA, num_transects = 5),
  list(habitat_id = 2, years = 1980:2024, producer_range = 8:16, consumer_range = 5:8, 
       producer_dist = "count", consumer_dist = "biomass",  shape1 = NA, shape2 = NA,
       lambda_min = 5, lambda_max = 15, meanlog = 1, sdlog = 0.5, num_transects = 6),
  list(habitat_id = 3, years = 1960:1991, producer_range = 40:60, consumer_range = 5:23, 
       producer_dist = "biomass", consumer_dist = "count",  shape1 = NA, shape2 = NA,
       lambda_min = 15, lambda_max = 25, meanlog = 2, sdlog = 0.7, num_transects = 10),
  list(habitat_id = 4, years = 2010:2024, producer_range = 1:8, consumer_range = 1:15, 
       producer_dist = "percent_cover", consumer_dist = "biomass",  shape1 = 2, shape2 = 5,
       lambda_min = 8, lambda_max = 12, meanlog = 1.5, sdlog = 0.4, num_transects = 8)
)

habitat_data <- bind_rows(
  lapply(habitat_details, function(h) do.call(simulate_habitat, h))
)

head(habitat_data)

# Simulate data for all habitats
set.seed(132)
sim_data <- bind_rows(
  lapply(habitat_details, function(h) do.call(simulate_habitat, h))
)

sim_data_wide <- sim_data %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0))

long_meta_cols <- c("year", "habitat", "transect_ID", "trophic_level", "measurement")

sim_diversity <- 
  data.frame(
    year = sim_data_wide$year,
    habitat = sim_data_wide$habitat,
    transect_ID = sim_data_wide$transect_ID,
    measurement = sim_data_wide$measurement,
    trophic_level = sim_data_wide$trophic_level,
    richness = rowSums(sim_data_wide[, -which(names(sim_data_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(sim_data_wide[, -which(names(sim_data_wide) %in% long_meta_cols)])
  )


sim_diversity_z <- sim_diversity %>%
  group_by(year, habitat, trophic_level) %>%
  mutate(
    richness_z = z_standard(richness),
    abundance_z = z_standard(abundance)
  ) %>%
  ungroup()

abundance_z_ranges <- sim_diversity_z %>%
  group_by(year, habitat, measurement, trophic_level) %>%
  summarize(
    min_abundance_z = min(abundance_z, na.rm = TRUE),
    max_abundance_z = max(abundance_z, na.rm = TRUE),
    range_abundance_z = max_abundance_z - min_abundance_z
  ) %>%
  ungroup()

## Visualize how z-standardizing affects abundance across measurement types
ggplot(data = sim_diversity_z, aes(abundance_z)) +
  geom_histogram() +
  facet_grid(measurement ~ trophic_level)

ggplot(data = abundance_z_ranges, aes(range_abundance_z)) +
  geom_histogram() +
  facet_grid(measurement ~ trophic_level)

ggplot(data = abundance_z_ranges, aes(min_abundance_z)) +
  geom_histogram() +
  facet_grid(measurement ~ trophic_level)

ggplot(data = abundance_z_ranges, aes(max_abundance_z)) +
  geom_histogram() +
  facet_grid(measurement ~ trophic_level)


#### ----

# create new metadata column to aggregate to appropriate replicate level
data_wide$environment <- paste0(data_wide$site, "-", data_wide$habitat, "_", data_wide$plot)

# list of meta data columns
data_meta <- c("year", "site", "habitat", "plot", "subplot", "uniqueID", "guild", "unitAbund", "scaleAbund", "environment")

# list of species columns
data_spec <- names(data_wide)[-which(names(data_wide) %in% data_meta)]

# aggregate to site level
data_condensed <- data_wide %>%
  dplyr::group_by(year, site, habitat, plot, environment, guild, unitAbund) %>%
  dplyr::summarise(across(all_of(data_spec), mean, na.rm = TRUE))

# calculate richness
alpha_diversity <- 
  data.frame(
    year = data_condensed$year,
    site = data_condensed$site,
    environment = data_condensed$environment,
    unitAbund = data_condensed$unitAbund,
    guild = data_condensed$guild,
    richness = rowSums(data_condensed[, which(names(data_condensed) %in% data_spec)] > 0),
    abundance = rowSums(data_condensed[, which(names(data_condensed) %in% data_spec)])
  )

diversity_stability <- alpha_diversity %>%
  dplyr::group_by(site, environment, guild) %>%
  dplyr::summarise(
    richness_mean = mean(richness),
    abundance_mean = mean(abundance),
    stability = stability(abundance)
  )

# z score richness for comparisons
diversity_stability$richness_trans <- z_standard(diversity_stability$richness_mean)

# with different measures of abundance, not sure how to include in sem
diversity_stability$abundance_trans <- z_standard(diversity_stability$abundance_mean)

# log transform for sem to linearize
diversity_stability$stability_trans <- log(diversity_stability$stability + 1)

# build SEM outputs
sem_results <- list()

# loop through guilds

# for each guild
for(i in unique(diversity_stability$guild)){ 
  
  # subset relevant data
  data <- subset(diversity_stability, guild == i)
  
  # define model list
  model_list <- list(
    lmer(stability_trans ~ abundance_trans + richness_trans + (1|site), data = data),
    lmer(abundance_trans ~ richness_trans + (1|site), data = data)
  )
  
  # fit sem
  sem_fit <- psem(model_list)
  
  # store results
  sem_results[[i]] <- summary(sem_fit)
  
  
}
