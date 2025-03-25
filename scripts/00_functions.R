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
filter_data <- function( # site_name, # vector of the site name you are filtering
                        producer_data, # producer dataset you're filtering (e.g. KNZ_producer.csv)
                        consumer_data, # consumer dataset you're fileterning (e.g. KNZ_consumer.csv)
                        mean_sum # do you aggregate subplots by taking the mean or the sum - vector c("mean", "sum")
                        ){

 # aggregate according to year-plot level
 # based on mean_sum for both producers and consumers
  
  if(mean_sum == "mean"){
    producer_agg <- producer_data %>%
      dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      dplyr::summarise(abundance = mean(abundance, na.rm = T))
    consumer_agg <- consumer_data %>%
      dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      dplyr::summarise(abundance = mean(abundance, na.rm = T))
  }else{
    producer_agg <- producer_data %>%
      dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      dplyr::summarise(abundance = sum(abundance, na.rm = T))
    
    consumer_agg <- consumer_data %>%
      dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
      dplyr::summarise(abundance = sum(abundance, na.rm = T))
  }
  
  # pivot wider
  producer_wide <- producer_agg %>%
    pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))
  producer_names <- names(producer_wide)
  
  consumer_wide <- consumer_agg %>%
    pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))
  consumer_names <- names(consumer_wide)
  
  # use merge to identify when producers and cosumers are spatio-temporally co-located
  # CHECK MASTER WIDE RESULTS WITH SUMMARY TABLES
  master_wide <- merge(producer_wide, consumer_wide, by = c("plot", "year"))
  
  # get subset of producers and consumers based on spatio-temporally co-located plots from master_wide
  producer_wide_sub <- master_wide[,which(names(master_wide) %in% producer_names)]
  consumer_wide_sub <- master_wide[,which(names(master_wide) %in% consumer_names)]
  


}

#### TEST ####
knz_prod <-  read.csv(here::here("data/KNZ", "knz_producer.csv"))
knz_con <- read.csv(here::here("data/KNZ", "knz_consumer.csv"))

x <- filter_data(producer_data = knz_prod, consumer_data = knz_con, mean_sum = "mean")

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
