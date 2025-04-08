rm(list = ls())
librarian::shelf(googledrive, dplyr, tidyr, summarytools, here, codyn)


#### z_standard() - function for z standardization ####
z_standard <- function(x){
  (x - mean(x))/sd(x)
}

#### CV() - coefficient of variation ####
CV<-function(x){
  return(sd(x,na.rm=T)/mean(x,na.rm=T))
}

#### stability() - inverse CV ####
stability <- function(x){
  1/CV(x)
}

#### filter_data() - function to streamline and clean datasets for synthesis ####
filter_data <- function(site_name, # site name as string
                        producer_data, # producer df
                        consumer_data, # consumer df
                        mean_sum, # c("mean", "sum") indicates whether plots should be averaged or summed when aggregating
                        minimize = FALSE, # subset for shortest possible time series based on spatio-temporally co-located plots
                        output_folder = NULL, # string for output folder if writing csv (e.g. "data/CDR")
                        write_csv = FALSE) {
  print("Starting function execution...")
  
  # remove not confident IDs
  producer_data <- subset(producer_data, id_confidence == 1)
  consumer_data <- subset(consumer_data, id_confidence == 1)
  
  # Aggregate data at the year-plot level
  print("Aggregating data...")
  if (mean_sum == "mean") {
    producer_agg <- producer_data %>%
      group_by(site, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      summarise(abundance = mean(abundance, na.rm = TRUE), .groups = "drop")
    
    consumer_agg <- consumer_data %>%
      group_by(site, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      summarise(abundance = mean(abundance, na.rm = TRUE), .groups = "drop")
  } else {
    producer_agg <- producer_data %>%
      group_by(site, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop")
    
    consumer_agg <- consumer_data %>%
      group_by(site, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop")
  }
  
  # pivot data to wide format
  print("Pivoting data to wide format...")
  producer_wide <- producer_agg %>%
    pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))
  
  consumer_wide <- consumer_agg %>%
    pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))
  
  # subset timpoints and plots that sampled consumers and producers in the same place/year 
  print("Checking valid plot-year combinations...")
  valid_plot_years <- inner_join(producer_wide %>% select(plot, year),
                                 consumer_wide %>% select(plot, year),
                                 by = c("plot", "year"))
  
  print("Filtering valid data...")
  producer_wide_sub <- producer_wide %>%
    inner_join(valid_plot_years, by = c("plot", "year")) %>%
    mutate(site = site_name)
  
  consumer_wide_sub <- consumer_wide %>%
    inner_join(valid_plot_years, by = c("plot", "year")) %>%
    mutate(site = site_name)
  
  # Minimize — remove plots with < 10 years of data and then standardize remaining plots by shortest time series possible
  if (minimize) {
    # Remove plots with <10 years of data
    plot_year_counts <- producer_wide_sub %>%
      group_by(plot) %>%
      summarise(n_years = n_distinct(year), .groups = "drop") %>%
      filter(n_years >= 10)
    
    valid_plots <- plot_year_counts$plot
    
    producer_wide_sub <- producer_wide_sub %>%
      filter(plot %in% valid_plots)
    
    consumer_wide_sub <- consumer_wide_sub %>%
      filter(plot %in% valid_plots)
    
    # Determine overlapping year range
    year_ranges <- producer_wide_sub %>%
      group_by(plot) %>%
      summarise(min_year = min(year), max_year = max(year), .groups = "drop")
    
    min_range_start <- max(year_ranges$min_year)
    max_range_end <- min(year_ranges$max_year)
    
    producer_wide_sub <- producer_wide_sub %>%
      filter(year >= min_range_start, year <= max_range_end)
    
    consumer_wide_sub <- consumer_wide_sub %>%
      filter(year >= min_range_start, year <= max_range_end)
  }
  
  # assign created dataframes to global environment
  print("Creating final object names...")
  producer_object_name <- paste0(site_name, "_producers_wide_sub")
  consumer_object_name <- paste0(site_name, "_consumers_wide_sub")
  
  print("Assigning objects to global environment...")
  assign(producer_object_name, producer_wide_sub, envir = .GlobalEnv)
  assign(consumer_object_name, consumer_wide_sub, envir = .GlobalEnv)
  
  # write csv
  if (write_csv) {
    if (!is.null(output_folder)) {
      if (!dir.exists(output_folder)) {
        message("Output path does not exist: ", output_folder)
      } else {
        write.csv(row.names = F, producer_wide_sub, here::here(output_folder, paste0(producer_object_name, ".csv")))
        write.csv(row.names = F, consumer_wide_sub, here::here(output_folder, paste0(consumer_object_name, ".csv")))
      }
    } else {
      # fallback to working directory
      write.csv(row.names = F, producer_wide_sub, paste0(producer_object_name, ".csv"))
      write.csv(row.names = F, consumer_wide_sub, paste0(consumer_object_name, ".csv"))
    }
  
  print("Function execution complete. Returning results...")
  return(setNames(
    list(producer_wide_sub, consumer_wide_sub),
    c(producer_object_name, consumer_object_name)
  ))
  }
}

# #### TEST ####
# knz_prod <-  read.csv(here::here("data/KNZ", "knz_producer.csv"))
# knz_con <- read.csv(here::here("data/KNZ", "knz_consumer.csv"))
# 
# filter_data(site_name = "knz", producer_data = knz_prod, consumer_data = knz_con, mean_sum = "mean", write_csv = FALSE)
# 
# functions to extract ranges of data (from mcr algal working group github)
# custom functions by Noam - could probably be made more efficient
# function to extract the ranges of the data for plotting



####  create aggregate diversity stability dataframe ####
calculate_agg_stability <- function(producer_data, # synthesized producer data after filtering
                                    consumer_data, # synthesized consumer data after filtering
                                    ecosystem_type, # vector c("Terrestrial", "Marine") indictaing ecosystem type
                                    output_folder = NULL, # string for output folder if writing csv (e.g. "data/CDR")
                                    write_csv = FALSE # option to automatically write csv
) {
  # Universal metadata columns for subsetting
  meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", "unit_abundance", "scale_abundance", "plot", "year")
  
  # calculate diversity for producers and consumers
  producer_diversity <- 
    data.frame(
      site = producer_data$site,
      taxa_type = producer_data$taxa_type,
      ecosystem = producer_data$ecosystem,
      habitat_broad = producer_data$habitat_broad,
      habitat_fine = producer_data$habitat_fine,
      biome = producer_data$biome,
      guild = producer_data$guild,
      plot = producer_data$plot,
      year = producer_data$year,
      richness = rowSums(producer_data[, -which(names(producer_data) %in% meta_cols)] > 0),
      abundance = rowSums(producer_data[, -which(names(producer_data) %in% meta_cols)]),
      shannon = vegan::diversity(producer_data[, -which(names(producer_data) %in% meta_cols)], "shannon")
    )
  
  consumer_diversity <- 
    data.frame(
      site = consumer_data$site,
      taxa_type = consumer_data$taxa_type,
      ecosystem = consumer_data$ecosystem,
      habitat_broad = consumer_data$habitat_broad,
      habitat_fine = consumer_data$habitat_fine,
      biome = consumer_data$biome,
      guild = consumer_data$guild,
      plot = consumer_data$plot,
      year = consumer_data$year,
      richness = rowSums(consumer_data[, -which(names(consumer_data) %in% meta_cols)] > 0),
      abundance = rowSums(consumer_data[, -which(names(consumer_data) %in% meta_cols)]),
      shannon = vegan::diversity(consumer_data[, -which(names(consumer_data) %in% meta_cols)], "shannon")
    )
  
  # create dss dataframes for producers and consumers
  producer_dss <- producer_diversity %>%
    dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
    dplyr::summarise(
      prod_richness = mean(richness), 
      prod_shannon = mean(shannon), 
      prod_abundance = mean(abundance),
      prod_cv = CV(abundance),
      prod_stability = stability(abundance)) 
  
  consumer_dss <- consumer_diversity %>%
    dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
    dplyr::summarise(
      con_richness = mean(richness), 
      con_shannon = mean(shannon), 
      con_abundance = mean(abundance),
      con_cv = CV(abundance),
      con_stability = stability(abundance)) 
  
  # calculate multitrophic stability
  # combine producers and consumers
  multitrophic_data <- merge(producer_data[,-which(names(producer_data) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                             consumer_data[,-which(names(consumer_data) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                             by = c("site", "ecosystem", "habitat_broad", "habitat_fine", "biome", "plot", "year"))
  
  # aggregate to calculate multritophic abundance and diversity
  multitrophic_diversity <- 
    data.frame(
      site = multitrophic_data$site,
      taxa_type = rep("multitroph", nrow(multitrophic_data)),
      ecosystem = multitrophic_data$ecosystem,
      habitat_broad = multitrophic_data$habitat_broad,
      habitat_fine = multitrophic_data$habitat_fine,
      biome = multitrophic_data$biome,
      guild = rep("multitroph", nrow(multitrophic_data)),
      plot = multitrophic_data$plot,
      year = multitrophic_data$year,
      richness = rowSums(multitrophic_data[, -which(names(multitrophic_data) %in% meta_cols)] > 0),
      abundance = rowSums(multitrophic_data[, -which(names(multitrophic_data) %in% meta_cols)]),
      shannon = vegan::diversity(multitrophic_data[, -which(names(multitrophic_data) %in% meta_cols)], "shannon")
    )
  
  # calculate multitrophic diversity and stability
  multitrophic_dss <- multitrophic_diversity %>%
    dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
    dplyr::summarise(
      multitroph_richness = mean(richness), 
      multitroph_shannon = mean(shannon), 
      multitroph_abundance = mean(abundance),
      multitroph_cv = CV(abundance),
      multitroph_stability = stability(abundance))
  
  # combine dfs for SEM
  aggregate_dss <- ungroup(
    left_join(producer_dss[,-which(names(producer_dss) %in% c("taxa_type", "guild"))],
              consumer_dss[,-which(names(consumer_dss) %in% c("taxa_type", "guild"))], 
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem")) %>%
      left_join(multitrophic_dss[,-which(names(multitrophic_dss) %in% c("taxa_type", "guild"))],
                by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
  )
  
  # create object names
  producer_object_name <- paste0(ecosystem_type, "_producer_dss")
  consumer_object_name <- paste0(ecosystem_type, "_consumer_dss")
  multitrophic_object_name <- paste0(ecosystem_type, "_multitrophic_dss")
  
  # assign to global environment
  assign(producer_object_name, producer_dss, envir = .GlobalEnv)
  assign(consumer_object_name, consumer_dss, envir = .GlobalEnv)
  assign(multitrophic_object_name, multitrophic_dss, envir = .GlobalEnv)
  

  # write csv
  if (write_csv) {
    if (!is.null(output_folder)) {
      if (!dir.exists(output_folder)) {
        message("Output path does not exist: ", output_folder)
      } else {
        write.csv(row.names = F, producer_dss, here::here(output_folder, paste0(producer_object_name, ".csv")))
        write.csv(row.names = F, consumer_dss, here::here(output_folder, paste0(consumer_object_name, ".csv")))
        write.csv(row.names = F, multitrophic_dss, here::here(output_folder, paste0(multitrophic_object_name, ".csv")))
      }
    } else {
      # fallback to working directory
      write.csv(row.names = F, producer_dss, paste0(producer_object_name, ".csv"))
      write.csv(row.names = F, consumer_dss, paste0(consumer_object_name, ".csv"))
      write.csv(row.names = F, multitrophic_dss, paste0(multitrophic_object_name, ".csv"))
    }
  }
}

extract_ranges <- function(df, # a dataframe
                           group, # grouping variable (e.g. habitat)
                           columns # columns you want to summarize (e.g. richness, cover, synchrony)
){
  df %>% # in your dataframe
    dplyr::group_by(across(all_of(group))) %>% 
    dplyr::summarise(across(all_of(columns), # summarize the columns in your group
                            list(min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)), # by taking the min/max of them
                            .names = "{.fn}_{.col}")) # and assigning them to the columns name "min/max_value"
}

# function for filtering ranges of emtrends to only fit range of actual data
filter_ranges <- function(trend, # emtrends df created from emmip
                          range_obj, # output df from extract_ranges() custom function
                          group, # group over which to summarize (e.g. habitat)
                          value # value to filter range (e.g. richness)
){
  trend %>% # take emtrend
    left_join(range_obj, by = group) %>% # join with range object
    filter(.data[[value]] >= .data[[paste0("min_", value)]] & # filter so emtrend only covers range of actual values
             .data[[value]] <= .data[[paste0("max_", value)]])
}
