library(librarian)
shelf(tidyverse)

control_confounder_spatial <- function(data) {
  # df is a data frame of calculated dss metrics
  
  # Adopt Laura Dee's comment: 
  # step 1: which metrics to use? and whether we need to do log transformation?
  # producer and consumer richness: no log transformation
  # producer stability: log transformation
  # consumer stability: no log transformation
  
  # step 2: do z-standardization to get standard effect sizes
  # this is needed
  
  # step 3: control for spatial confounding factors 
  
  # add new columns, do log transformation
  data$prod_stability_log <- log(data$prod_stability)
  data$herb_stability_log <- log(data$herb_stability)  
  data$con_stability_log <- log(data$con_stability)
  data$prod_richness_log <- log(data$prod_richness + 1)
  data$herb_richness_log <- log(data$herb_richness + 1)
  data$con_richness_log <- log(data$con_richness + 1)
  
  # z-score to get standardized effect size
  data$prod_richness_zscore <- scale(data$prod_richness)
  data$herb_richness_zscore <- scale(data$herb_richness)
  data$con_richness_zscore <- scale(data$con_richness)
  data$prod_stability_zscore <- scale(data$prod_stability)
  data$herb_stability_zscore <- scale(data$herb_stability)  
  data$con_stability_zscore <- scale(data$con_stability)
  #
  data$prod_richness_log_zscore <- scale(data$prod_richness_log)
  data$herb_richness_log_zscore <- scale(data$herb_richness_log)  
  data$con_richness_log_zscore <- scale(data$con_richness_log)
  data$prod_stability_log_zscore <- scale(data$prod_stability_log)
  data$herb_stability_log_zscore <- scale(data$herb_stability_log)
  data$con_stability_log_zscore <- scale(data$con_stability_log)
  
  # calculate mean diversity and stability of each site
  data_site_mean <- data %>% group_by(site_new) %>% 
    summarise(prod_richness_zscore_site_mean = mean(prod_richness_zscore), 
              herb_richness_zscore_site_mean = mean(herb_richness_zscore), 
              con_richness_zscore_site_mean = mean(con_richness_zscore),
              prod_stability_zscore_site_mean = mean(prod_stability_zscore), 
              herb_stability_zscore_site_mean = mean(herb_stability_zscore), 
              con_stability_zscore_site_mean = mean(con_stability_zscore),
              
              prod_richness_log_zscore_site_mean = mean(prod_richness_log_zscore), 
              herb_richness_log_zscore_site_mean = mean(herb_richness_log_zscore), 
              con_richness_log_zscore_site_mean = mean(con_richness_log_zscore),            
              prod_stability_log_zscore_site_mean = mean(prod_stability_log_zscore),
              herb_stability_log_zscore_site_mean = mean(herb_stability_log_zscore),
              con_stability_log_zscore_site_mean = mean(con_stability_log_zscore))
  
  if (! "prod_richness_zscore_site_mean" %in% names(data)) {
    data <- data %>% left_join(data_site_mean, by="site_new")
  }
  
  # calculate deviation
  data$prod_richness_zscore_devi <- data$prod_richness_zscore - data$prod_richness_zscore_site_mean
  data$herb_richness_zscore_devi <- data$herb_richness_zscore - data$herb_richness_zscore_site_mean
  data$con_richness_zscore_devi <- data$con_richness_zscore - data$con_richness_zscore_site_mean
  data$prod_stability_zscore_devi <- data$prod_stability_zscore - data$prod_stability_zscore_site_mean
  data$herb_stability_zscore_devi <- data$herb_stability_zscore - data$herb_stability_zscore_site_mean
  data$con_stability_zscore_devi <- data$con_stability_zscore - data$con_stability_zscore_site_mean
  #
  data$prod_richness_log_zscore_devi <- data$prod_richness_log_zscore - data$prod_richness_log_zscore_site_mean
  data$herb_richness_log_zscore_devi <- data$herb_richness_log_zscore - data$herb_richness_log_zscore_site_mean
  data$con_richness_log_zscore_devi <- data$con_richness_log_zscore - data$con_richness_log_zscore_site_mean
  data$prod_stability_log_zscore_devi <- data$prod_stability_log_zscore - data$prod_stability_log_zscore_site_mean
  data$herb_stability_log_zscore_devi <- data$herb_stability_log_zscore - data$herb_stability_log_zscore_site_mean
  data$con_stability_log_zscore_devi <- data$con_stability_log_zscore - data$con_stability_log_zscore_site_mean  
  
  return(data)
}

#-------------------------------------------------------------------------------
control_confounder_temporal <- function(data) {
  # df is a data frame of calculated dss metrics
  
  # Adopt Laura Dee's comment: 
  # step 1: which metrics to use? and whether we need to do log transformation?
  # producer and consumer richness: no log transformation
  # producer stability: log transformation
  # consumer stability: no log transformation
  
  # step 2: do z-standardization to get standard effect sizes
  # this is needed
  
  # step 3: control for spatial confounding factors 
  
  # add new columns, do log transformation
  data$prod_stability_log <- log(data$prod_stability)
  data$herb_stability_log <- log(data$herb_stability)  
  data$con_stability_log <- log(data$con_stability)
  data$prod_richness_log <- log(data$prod_richness + 1)
  data$herb_richness_log <- log(data$herb_richness + 1)
  data$con_richness_log <- log(data$con_richness + 1)
  
  # z-score to get standardized effect size
  data$prod_richness_zscore <- scale(data$prod_richness)
  data$herb_richness_zscore <- scale(data$herb_richness)
  data$con_richness_zscore <- scale(data$con_richness)
  data$prod_stability_zscore <- scale(data$prod_stability)
  data$herb_stability_zscore <- scale(data$herb_stability)  
  data$con_stability_zscore <- scale(data$con_stability)
  #
  data$prod_richness_log_zscore <- scale(data$prod_richness_log)
  data$herb_richness_log_zscore <- scale(data$herb_richness_log)  
  data$con_richness_log_zscore <- scale(data$con_richness_log)
  data$prod_stability_log_zscore <- scale(data$prod_stability_log)
  data$herb_stability_log_zscore <- scale(data$herb_stability_log)
  data$con_stability_log_zscore <- scale(data$con_stability_log)
  
  # calculate mean diversity and stability of each plot
  data_plot_mean <- data %>% group_by(plot_new) %>% 
    summarise(prod_richness_zscore_plot_mean = mean(prod_richness_zscore), 
              herb_richness_zscore_plot_mean = mean(herb_richness_zscore), 
              con_richness_zscore_plot_mean = mean(con_richness_zscore),
              prod_stability_zscore_plot_mean = mean(prod_stability_zscore), 
              herb_stability_zscore_plot_mean = mean(herb_stability_zscore), 
              con_stability_zscore_plot_mean = mean(con_stability_zscore),
              
              prod_richness_log_zscore_plot_mean = mean(prod_richness_log_zscore), 
              herb_richness_log_zscore_plot_mean = mean(herb_richness_log_zscore), 
              con_richness_log_zscore_plot_mean = mean(con_richness_log_zscore),            
              prod_stability_log_zscore_plot_mean = mean(prod_stability_log_zscore),
              herb_stability_log_zscore_plot_mean = mean(herb_stability_log_zscore),
              con_stability_log_zscore_plot_mean = mean(con_stability_log_zscore))
  
  if (! "prod_richness_zscore_plot_mean" %in% names(data)) {
    data <- data %>% left_join(data_plot_mean, by="plot_new")
  }
  
  # calculate mean diversity and stability of each site_period
  data_site_period_mean <- data %>% group_by(site_period) %>% 
    summarise(prod_richness_zscore_site_period_mean = mean(prod_richness_zscore), 
              herb_richness_zscore_site_period_mean = mean(herb_richness_zscore), 
              con_richness_zscore_site_period_mean = mean(con_richness_zscore),
              prod_stability_zscore_site_period_mean = mean(prod_stability_zscore), 
              herb_stability_zscore_site_period_mean = mean(herb_stability_zscore), 
              con_stability_zscore_site_period_mean = mean(con_stability_zscore),
              
              prod_richness_log_zscore_site_period_mean = mean(prod_richness_log_zscore), 
              herb_richness_log_zscore_site_period_mean = mean(herb_richness_log_zscore), 
              con_richness_log_zscore_site_period_mean = mean(con_richness_log_zscore),            
              prod_stability_log_zscore_site_period_mean = mean(prod_stability_log_zscore),
              herb_stability_log_zscore_site_period_mean = mean(herb_stability_log_zscore),
              con_stability_log_zscore_site_period_mean = mean(con_stability_log_zscore))
  
  if (! "prod_richness_zscore_site_period_mean" %in% names(data)) {
    data <- data %>% left_join(data_site_period_mean, by="site_period")
  }  
  
  
  # calculate deviation of plot mean
  data$prod_richness_zscore_plot_mean_devi <- data$prod_richness_zscore - data$prod_richness_zscore_plot_mean
  data$herb_richness_zscore_plot_mean_devi <- data$herb_richness_zscore - data$herb_richness_zscore_plot_mean
  data$con_richness_zscore_plot_mean_devi <- data$con_richness_zscore - data$con_richness_zscore_plot_mean
  data$prod_stability_zscore_plot_mean_devi <- data$prod_stability_zscore - data$prod_stability_zscore_plot_mean
  data$herb_stability_zscore_plot_mean_devi <- data$herb_stability_zscore - data$herb_stability_zscore_plot_mean
  data$con_stability_zscore_plot_mean_devi <- data$con_stability_zscore - data$con_stability_zscore_plot_mean
  #
  data$prod_richness_log_zscore_plot_mean_devi <- data$prod_richness_log_zscore - data$prod_richness_log_zscore_plot_mean
  data$herb_richness_log_zscore_plot_mean_devi <- data$herb_richness_log_zscore - data$herb_richness_log_zscore_plot_mean
  data$con_richness_log_zscore_plot_mean_devi <- data$con_richness_log_zscore - data$con_richness_log_zscore_plot_mean
  data$prod_stability_log_zscore_plot_mean_devi <- data$prod_stability_log_zscore - data$prod_stability_log_zscore_plot_mean
  data$herb_stability_log_zscore_plot_mean_devi <- data$herb_stability_log_zscore - data$herb_stability_log_zscore_plot_mean
  data$con_stability_log_zscore_plot_mean_devi <- data$con_stability_log_zscore - data$con_stability_log_zscore_plot_mean  
  #
  data$prod_richness_log_zscore_site_period_mean_devi <- data$prod_richness_log_zscore - data$prod_richness_log_zscore_site_period_mean
  data$herb_richness_log_zscore_site_period_mean_devi <- data$herb_richness_log_zscore - data$herb_richness_log_zscore_site_period_mean
  data$con_richness_log_zscore_site_period_mean_devi <- data$con_richness_log_zscore - data$con_richness_log_zscore_site_period_mean
  data$prod_stability_log_zscore_site_period_mean_devi <- data$prod_stability_log_zscore - data$prod_stability_log_zscore_site_period_mean
  data$herb_stability_log_zscore_site_period_mean_devi <- data$herb_stability_log_zscore - data$herb_stability_log_zscore_site_period_mean
  data$con_stability_log_zscore_site_period_mean_devi <- data$con_stability_log_zscore - data$con_stability_log_zscore_site_period_mean  
  
  return(data)
}


