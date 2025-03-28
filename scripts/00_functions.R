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
filter_data <- function(site_name, producer_data, consumer_data, mean_sum, write_csv = FALSE) {
  print("Starting function execution...")
  
  # Aggregate data at the year-plot level
  print("Aggregating data...")
  if (mean_sum == "mean") {
    producer_agg <- producer_data %>%
      group_by(plot, year, taxon_name) %>%
      summarise(abundance = mean(abundance, na.rm = TRUE), .groups = "drop")
    
    consumer_agg <- consumer_data %>%
      group_by(plot, year, taxon_name) %>%
      summarise(abundance = mean(abundance, na.rm = TRUE), .groups = "drop")
  } else {
    producer_agg <- producer_data %>%
      group_by(plot, year, taxon_name) %>%
      summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop")
    
    consumer_agg <- consumer_data %>%
      group_by(plot, year, taxon_name) %>%
      summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop")
  }
  
  print("Pivoting data to wide format...")
  producer_wide <- producer_agg %>%
    pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))
  
  consumer_wide <- consumer_agg %>%
    pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))
  
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
  
  print("Creating final object names...")
  producer_object_name <- paste0(site_name, "_producers_wide_sub")
  consumer_object_name <- paste0(site_name, "_consumers_wide_sub")
  
  print("Assigning objects to global environment...")
  assign(producer_object_name, producer_wide_sub, envir = .GlobalEnv)
  assign(consumer_object_name, consumer_wide_sub, envir = .GlobalEnv)
  
  if (write_csv) {
    print("Writing to CSV...")
    write_csv(producer_wide_sub, paste0(producer_object_name, ".csv"))
    write_csv(consumer_wide_sub, paste0(consumer_object_name, ".csv"))
  }
  
  print("Function execution complete. Returning results...")
  return(setNames(
    list(producer_wide_sub, consumer_wide_sub),
    c(producer_object_name, consumer_object_name)
  ))
}

#### TEST ####
knz_prod <-  read.csv(here::here("data/KNZ", "knz_producer.csv"))
knz_con <- read.csv(here::here("data/KNZ", "knz_consumer.csv"))

filter_data(site_name = "knz", producer_data = knz_prod, consumer_data = knz_con, mean_sum = "mean", write_csv = FALSE)

# functions to extract ranges of data (from mcr algal working group github)
# custom functions by Noam - could probably be made more efficient
# function to extract the ranges of the data for plotting
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
