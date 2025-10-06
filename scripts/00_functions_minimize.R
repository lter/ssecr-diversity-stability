# rm(list = ls())
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
  # Junna added
  if (minimize) {
    # create plot year table
    valid_plot_years$year <- as.factor(valid_plot_years$year)
    valid_plot_years_check <- table(valid_plot_years)
#    browser()
    
    # identify years when each site is sampled
    # sites sampled per year
    nsites_1year <- colSums(valid_plot_years_check) 
    overlap_years <- colnames(valid_plot_years_check)[nsites_1year == nrow(valid_plot_years_check)]
    while (length(overlap_years) < 10 & nrow(valid_plot_years_check) >=5) {
      # find the year column we need to move the least number of sites
      nonoverlap_year <- order(nsites_1year, decreasing = T)[length(overlap_years) + 1]
      
      # remove sites with least number of overlapping years    
      valid_plot_years_check <- valid_plot_years_check[-which(valid_plot_years_check[, nonoverlap_year] == 0), ]
      
      # recalculate overlapping years
      nsites_1year <- colSums(valid_plot_years_check)
      overlap_years <- colnames(valid_plot_years_check)[nsites_1year == nrow(valid_plot_years_check)]
    }
    
    if (nrow(valid_plot_years_check) < 5) {
      print('Warning: we do not have enough plots with > 10 overlapping years!!')
    } else {
      print(cat("Number of effective plots: ", nrow(valid_plot_years_check), '; plot names: ', paste(rownames(valid_plot_years_check), collapse = ", "), "\n"))
      print(cat("Number of sampling years: ", length(overlap_years), '; sampling years: ', paste(overlap_years, collapse = ", "), "\n"))
    }
    
    producer_wide_sub <- producer_wide %>%
      filter(year %in% as.numeric(overlap_years)) %>%
      filter(plot %in% rownames(valid_plot_years_check))
    
    consumer_wide_sub <- consumer_wide %>%
      filter(year %in% as.numeric(overlap_years)) %>%
      filter(plot %in% rownames(valid_plot_years_check))      
  } else {
    producer_wide_sub <- producer_wide %>%
      inner_join(valid_plot_years, by = c("plot", "year")) %>%
      mutate(site = site_name)

    consumer_wide_sub <- consumer_wide %>%
      inner_join(valid_plot_years, by = c("plot", "year")) %>%
      mutate(site = site_name)
  }
  # Junna's add ends here.
  
  # # Minimize — remove plots with < 10 years of data and then standardize remaining plots by shortest time series possible
  # if (minimize) {
  #   # Remove plots with <10 years of data
  #   plot_year_counts <- producer_wide_sub %>%
  #     group_by(plot) %>%
  #     summarise(n_years = n_distinct(year), .groups = "drop") %>%
  #     filter(n_years >= 10)
  #   
  #   valid_plots <- plot_year_counts$plot
  #   
  #   producer_wide_sub <- producer_wide_sub %>%
  #     filter(plot %in% valid_plots)
  #   
  #   consumer_wide_sub <- consumer_wide_sub %>%
  #     filter(plot %in% valid_plots)
  #   
  #   # Determine overlapping year range
  #   year_ranges <- producer_wide_sub %>%
  #     group_by(plot) %>%
  #     summarise(min_year = min(year), max_year = max(year), .groups = "drop")
  #   
  #   min_range_start <- max(year_ranges$min_year)
  #   max_range_end <- min(year_ranges$max_year)
  #   
  #   producer_wide_sub <- producer_wide_sub %>%
  #     filter(year >= min_range_start, year <= max_range_end)
  #   
  #   consumer_wide_sub <- consumer_wide_sub %>%
  #     filter(year >= min_range_start, year <= max_range_end)
  # }
  
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
                                    site_name, # site name as string
                                    output_folder = NULL, # string for output folder if writing csv (e.g. "data/CDR")
                                    write_csv = FALSE # option to automatically write csv
) {
  # Universal metadata columns for subsetting
  meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", "unit_abundance", "scale_abundance", "plot", "year")
  
  # calculate diversity for producers and consumers
  producer_diversity <- 
    data.frame(
      site = producer_data$site,
      #     taxa_type = producer_data$taxa_type,
      taxa_type = rep("producer", nrow(producer_data)),
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
      #     taxa_type = producer_data$taxa_type,
      taxa_type = rep("consumer", nrow(consumer_data)),
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
  # the way to calculate abundance of multi-tropic levels may have problems. 
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
  producer_object_name <- paste0(site_name, "_producer_dss")
  consumer_object_name <- paste0(site_name, "_consumer_dss")
  multitrophic_object_name <- paste0(site_name, "_multitrophic_dss")
  aggregate_object_name <- paste0(site_name, "_aggregate_dss")
  
  # assign to global environment
  assign(producer_object_name, producer_dss, envir = .GlobalEnv)
  assign(consumer_object_name, consumer_dss, envir = .GlobalEnv)
  assign(multitrophic_object_name, multitrophic_dss, envir = .GlobalEnv)
  assign(aggregate_object_name, aggregate_dss, envir = .GlobalEnv)
  
  
  # write csv
  if (write_csv) {
    if (!is.null(output_folder)) {
      if (!dir.exists(output_folder)) {
        message("Output path does not exist: ", output_folder)
      } else {
        write.csv(row.names = F, producer_dss, here::here(output_folder, paste0(producer_object_name, ".csv")))
        write.csv(row.names = F, consumer_dss, here::here(output_folder, paste0(consumer_object_name, ".csv")))
        write.csv(row.names = F, multitrophic_dss, here::here(output_folder, paste0(multitrophic_object_name, ".csv")))
        write.csv(row.names = F, aggregate_dss, here::here(output_folder, paste0(aggregate_object_name, ".csv")))
      }
    } else {
      # fallback to working directory
      write.csv(row.names = F, producer_dss, paste0(producer_object_name, ".csv"))
      write.csv(row.names = F, consumer_dss, paste0(consumer_object_name, ".csv"))
      write.csv(row.names = F, multitrophic_dss, paste0(multitrophic_object_name, ".csv"))
      write.csv(row.names = F, aggregate_dss, paste0(aggregate_object_name, ".csv"))
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

#### create preliminary models plots for  stability
model_stability <- function(df, # stability df
                            ecosystem_type, # string c("terrestrial", "marine")
                            stability_metric, # metric used to measure stability c("aggregate", "compositional")
                            diversity_metric, # metric used to measure diversity
                            prod_diversity_col, # column name that contains producer diversity
                            con_diversity_col, # column name that contains consumer stability
                            prod_stability_col, # column name that contains producer diversity
                            con_stability_col, # column name that contains consumer stability
                            multi_stability_col, # column name that contains multitrophic stability
                            transformation = c("none", "log", "z")) { # what kind of transformation do you want?
  # check transformation
  transformation <- match.arg(transformation)
  
  # create temporary vector with the relevant numeric variables - add multitrophic stability if present
  all_cols <- c(prod_diversity_col, con_diversity_col, prod_stability_col, con_stability_col)
  if (!is.null(multi_stability_col)) {
    all_cols <- c(all_cols, multi_stability_col)
  }
  
  # Transformation logic
  if (transformation == "log") {
    df <- df %>%
      mutate(across(all_of(all_cols), ~ log(.x + 1)))
    
  } else if (transformation == "z") {
    df <- df %>%
      group_by(site) %>%
      mutate(across(all_of(all_cols), ~ as.numeric(z_standard(.x)))) %>%
      ungroup()
  }
  
  # set names of relevant dataframes based on diversity and stability metrics
  prod_site_results_name <- paste0("site_results_", ecosystem_type, "_prod_", stability_metric, "_", diversity_metric)
  con_site_results_name <- paste0("site_results_", ecosystem_type, "_con_", stability_metric, "_", diversity_metric)
  multitrophic_site_results_name <- paste0("site_results_", ecosystem_type, "_multi_", stability_metric, "_", diversity_metric)
  combined_lmer_results_name <- paste0("lmer_results_", ecosystem_type, "_", stability_metric, "_", diversity_metric)
  
  
  #### SITE LEVEL ANALYSES #####
  
  ##### PRODUCER STABILITY #####
  prod_stability_results_list <- list()
  
  for (s in unique(df$site)) {
    site_data <- subset(df, site == s)
    
    
    #  CREATE A FORMULA STRING AND THEN APPLY IS TO THE FORMULA OBJECTI WITH as.formula()
    producer_model_formula <- as.formula(paste0(
      prod_stability_col, " ~ ",
      prod_diversity_col, " + ",
      con_diversity_col)
    )
    
    
    # Fit the model
    producer_model <- lm(producer_model_formula, data = site_data)
    
    # Get coefficients
    coefs <- coef(producer_model)
    
    # Get ANOVA results
    anova_results <- Anova(producer_model)
    prod_f_statistic <- anova_results$`F value`[1]
    con_f_statistic <- anova_results$`F value`[2]
    prod_p_value <- anova_results$`Pr(>F)`[1]
    con_p_value <- anova_results$`Pr(>F)`[2]
    
    # Dynamically get correct coefficient names
    coef_names <- names(coefs)
    prod_name <-  prod_diversity_col
    con_name  <-  con_diversity_col
    stability_name <- stability_metric
    
    # Store results
    prod_stability_results_list[[s]] <- data.frame(
      model_type <- paste0("producer_", stability_name),
      site = s,
      intercept = unname(coefs["(Intercept)"]),
      prod_diversity_coef = unname(coefs[prod_name]),
      con_diversity_coef = unname(coefs[con_name]),
      prod_f_statistic = prod_f_statistic,
      con_f_statistic = con_f_statistic,
      prod_p_value = prod_p_value,
      con_p_value = con_p_value,
      stringsAsFactors = FALSE
    )
  }
  
  
  ##### CONSUMER STABILITY #####
  con_stability_results_list <- list()
  
  for (s in unique(df$site)) {
    site_data <- subset(df, site == s)
    
    
    # model formula
    consumer_model_formula <- as.formula(paste0(
      con_stability_col, " ~ ",
      prod_diversity_col, " + ",
      con_diversity_col
    ))
    
    
    # Fit the model
    consumer_model <- lm(consumer_model_formula, data = site_data)
    
    # Get coefficients
    consumer_coefs <- coef(consumer_model)
    
    # Get ANOVA results
    anova_results <- Anova(consumer_model)
    prod_f_statistic <- anova_results$`F value`[1]
    con_f_statistic <- anova_results$`F value`[2]
    prod_p_value <- anova_results$`Pr(>F)`[1]
    con_p_value <- anova_results$`Pr(>F)`[2]
    
    # Dynamically get correct coefficient names
    consumer_coef_names <- names(consumer_coefs)
    prod_name <-  prod_diversity_col
    con_name  <-  con_diversity_col
    stability_name <- stability_metric
    
    # Store results
    con_stability_results_list[[s]] <- data.frame(
      model_type <- paste0("consumer_", stability_name),
      site = s,
      intercept = unname(consumer_coefs["(Intercept)"]),
      prod_diversity_coef = unname(consumer_coefs[prod_name]),
      con_diversity_coef = unname(consumer_coefs[con_name]),
      prod_f_statistic = prod_f_statistic,
      con_f_statistic = con_f_statistic,
      prod_p_value = prod_p_value,
      con_p_value = con_p_value,
      stringsAsFactors = FALSE
    )
  }
  
  ##### MULTITROPHIC STABILITY #####
  if (!is.null(multi_stability_col)) {
    multitrophic_stability_results_list <- list()
    
    for (s in unique(df$site)) {
      site_data <- subset(df, site == s)
      
      # model formula 
      multitrophic_model_formula <- as.formula(paste0(
        multi_stability_col, " ~ ",
        prod_stability_col, " + ",
        con_stability_col
      ))
      
      
      # Fit the model
      multitrophic_model <- lm(multitrophic_model_formula, data = site_data)
      
      # Get coefficients
      coefs <- coef(multitrophic_model)
      
      # Get ANOVA results
      anova_results <- Anova(multitrophic_model)
      prod_f_statistic <- anova_results$`F value`[1]
      con_f_statistic <- anova_results$`F value`[2]
      prod_p_value <- anova_results$`Pr(>F)`[1]
      con_p_value <- anova_results$`Pr(>F)`[2]
      
      # Dynamically get correct coefficient names
      coef_names <- names(coefs)
      prod_name <-  prod_stability_col
      con_name  <-  con_stability_col
      stability_name <- stability_metric
      
      # Store results
      multitrophic_stability_results_list[[s]] <- data.frame(
        model_type <- paste0("multitrophic_", stability_name),
        site = s,
        intercept = unname(coefs["(Intercept)"]),
        prod_stability_coef = unname(coefs[prod_name]),
        con_stability_coef = unname(coefs[con_name]),
        prod_f_statistic = prod_f_statistic,
        con_f_statistic = con_f_statistic,
        prod_p_value = prod_p_value,
        con_p_value = con_p_value,
        stringsAsFactors = FALSE
      )
    }
  }
  
  prod_stability_results <- do.call(rbind, prod_stability_results_list)
  con_stability_results <- do.call(rbind, con_stability_results_list)
  
  
  # assign objects to global environment
  assign(prod_site_results_name, prod_stability_results, envir = .GlobalEnv)
  assign(con_site_results_name, con_stability_results, envir = .GlobalEnv)
  
  # do the same for multitrophic stability if the column is present
  if (!is.null(multi_stability_col)) {
    multitrophic_stability_results <- do.call(rbind, multitrophic_stability_results_list)
    assign(multitrophic_site_results_name, multitrophic_stability_results, envir = .GlobalEnv)
  }
  
  #### SITE AS RANDOM EFFECT ####
  
  
  # model formulas
  producer_combined_model_formula <- as.formula(paste0(
    prod_stability_col, " ~ ",
    "0 + ",                               # Junna added 5/20/2025, no need for intercept
    prod_diversity_col, " + ",
    con_diversity_col, " + ",
    "(0 + ", prod_diversity_col, " | site) + ",
    "(0 + ", con_diversity_col, " | site)"
  ))
  
  consumer_combined_model_formula <- as.formula(paste0(
    con_stability_col, " ~ ",
    "0 + ",                               # Junna added 5/20/2025, no need for intercept
    prod_diversity_col, " + ",
    con_diversity_col, " + ",
    "(0 + ", prod_diversity_col, " | site) + ",
    "(0 + ", con_diversity_col, " | site)"
  ))
  
  
  
  # Fit the models - LOOP METHOD
  # Store models in a named list
  combined_models <- list(
    producer = lmer(producer_combined_model_formula, data = df),
    consumer = lmer(consumer_combined_model_formula, data = df)
  )
  if (!is.null(multi_stability_col)) {
    multitrophic_combined_model_formula <-as.formula(paste0(
      multi_stability_col, " ~ ",
      prod_stability_col, " + ",
      con_stability_col, " + ",
      "(1 + ", prod_stability_col, " | site) + ",
      "(1 + ", con_stability_col, " | site)"
    ))
    combined_models$multitrophic <- lmer(multitrophic_combined_model_formula, data = df)
  }
  
  # Initialize list to collect results
  combined_model_results_list <- list()
  
  # Loop through models to extract statistics
  for (model_name in names(combined_models)) {
    model <- combined_models[[model_name]]
    anova_res <- Anova(model)
    coefs <- fixef(model)  # Get fixed effects
    r2 <- performance::r2(model)
    
    # Create a row of results for this model
    combined_model_results_list[[model_name]] <- data.frame(
      model = model_name,
      intercept = unname(coefs["(Intercept)"]),
      prod_coef = unname(coefs[2]),
      con_coef = unname(coefs[3]),
      prod_chisq = anova_res$Chisq[1],
      con_chisq = anova_res$Chisq[2],
      prod_p_value = anova_res$`Pr(>Chisq)`[1],
      con_p_value = anova_res$`Pr(>Chisq)`[2],
      R2_marginal = unname(r2$R2_marginal),
      R2_conditional = unname(r2$R2_conditional),
      stringsAsFactors = FALSE
    )
  }
  
  # Combine into a final dataframe
  combined_model_results <- do.call(rbind, combined_model_results_list)
  rownames(combined_model_results) <- combined_model_results$model
  assign(combined_lmer_results_name, combined_model_results, envir = .GlobalEnv)
  
  ##### CREATE PLOTS #####
  # NOTE: USED CHATGPT TO STREAMLINE THIS PROCESS
  # NOTE: COULD PROBABLY DO THIS EARLIER AND REMOVE THE CONDITIONAL FROM THE MODEL STRUCTURE STEP?
  
  
  # BASIC PLOTS
  # NOTE: I WANT TO CREATE A SECOND WRAPPER THAT EXPLICITLY DRAWS TRENDLINES FROM MODEL COEFFICIENTS
  basic_plot <- function(df,
                         xcol,
                         ycol
  ){
    ggplot() +
      geom_point(data = df, aes_string(x = xcol, y = ycol, colour = "site")) +
      stat_smooth(data = df, aes_string(x = xcol, y = ycol, colour = "site"),
                  method = "lm", se = FALSE) +
      stat_smooth(data = df, aes_string(x = xcol, y = ycol),
                  method = "lm", se = TRUE, colour = "black") +
      theme_classic()
  }
  
  ## PRODUCE PLOTS
  producer_stability_plot_name <- paste0("plot_",ecosystem_type,"_", stability_metric, "_prod_", diversity_metric,  "_stability")
  consumer_stability_plot_name <- paste0("plot_", ecosystem_type,"_", stability_metric, "_con_", diversity_metric,  "_stability")
  multitrophic_stability_plot_name <- paste0("plot_", ecosystem_type,"_", stability_metric, "_multi_", diversity_metric,  "_stability")
  correlation_plot_name <- paste0("plot_", ecosystem_type,"_", stability_metric, "_stability_", diversity_metric,  "_correlations")
  
  # Producer stability
  prod_stab_prod_div_plot <- basic_plot(df, xcol = prod_diversity_col, ycol = prod_stability_col)
  prod_stab_con_div_plot <- basic_plot(df, xcol = con_diversity_col, ycol = prod_stability_col)
  producer_stability_plot <- ggpubr::ggarrange(prod_stab_prod_div_plot, prod_stab_con_div_plot,
                                               nrow = 1, ncol = 2, common.legend = TRUE)
  
  con_stab_prod_div_plot <- basic_plot(df, xcol = prod_diversity_col, ycol = con_stability_col)
  con_stab_con_div_plot <- basic_plot(df, xcol = con_diversity_col, ycol = con_stability_col)
  consumer_stability_plot <- ggpubr::ggarrange(con_stab_prod_div_plot, con_stab_con_div_plot,
                                               nrow = 1, ncol = 2, common.legend = TRUE)
  
  diversity_corr_plot <- basic_plot(df, xcol = prod_diversity_col, ycol = con_diversity_col)
  stability_corr_plot <- basic_plot(df, xcol = prod_stability_col, ycol = con_stability_col)
  correlation_plot <- ggpubr::ggarrange(diversity_corr_plot, stability_corr_plot,
                                        nrow = 1, ncol = 2, common.legend = TRUE)
  
  # assign plots to environment
  assign(producer_stability_plot_name, producer_stability_plot, envir = .GlobalEnv)
  assign(consumer_stability_plot_name, consumer_stability_plot, envir = .GlobalEnv)
  assign(correlation_plot_name, correlation_plot, envir = .GlobalEnv)
  
  # handle multitrophic plot if variable is present
  if (!is.null(multi_stability_col)) {
    multi_stab_prod_div_plot <- basic_plot(df, xcol = prod_stability_col, ycol = multi_stability_col)
    multi_stab_con_div_plot <- basic_plot(df, xcol = con_stability_col, ycol = multi_stability_col)
    multitrophic_stability_plot <- ggpubr::ggarrange(multi_stab_prod_div_plot, multi_stab_con_div_plot,
                                                     nrow = 1, ncol = 2, common.legend = TRUE)
    assign(multitrophic_stability_plot_name, multitrophic_stability_plot, envir = .GlobalEnv)
  }
  
  
  
  
  
  ##### PIECEWISE SEM #####
  # names for environment
  piecewise_results_name <- paste0("sem_results_", ecosystem_type, "_", diversity_metric, "_", stability_metric)
  sem_plot_name <- paste0("basic_sem_plot_", ecosystem_type, "_", diversity_metric, "_", stability_metric)
  
  
  # Define correlated errors
  cor1 <- eval(parse(text = paste0(prod_stability_col, " %~~% ", con_stability_col)))
  cor2 <- eval(parse(text = paste0(prod_diversity_col, " %~~% ", con_diversity_col)))
  
  # build the list of psem() arguments
  psem_args <- list(
    lmer(producer_combined_model_formula, data = df),
    lmer(consumer_combined_model_formula, data = df),
    cor1,
    cor2
  )
  
  # add multitrophic model only if it exists
  if (!is.null(multi_stability_col)) {
    multitrophic_combined_model <- lmer(multitrophic_combined_model_formula, data = df)
    psem_args <- append(psem_args, list(multitrophic_combined_model))
  }
  
  # add the data argument last 
  psem_args <- append(psem_args, list(data = df))
  
  # run the SEM
  modelList <- do.call(psem, psem_args)
  piecewise_summary <- summary(modelList)
  basic_sem_plot <- plot(modelList)
  
  # Assign results
  assign(piecewise_results_name, piecewise_summary, envir = .GlobalEnv)
  assign(sem_plot_name, basic_sem_plot, envir = .GlobalEnv)
  
  
  # Assign results
  assign(piecewise_results_name, piecewise_summary, envir = .GlobalEnv)
  assign(sem_plot_name, basic_sem_plot, envir = .GlobalEnv)
  
  
  assign(piecewise_results_name, piecewise_summary, envir = .GlobalEnv)
  assign(sem_plot_name, basic_sem_plot, envir = .GlobalEnv)
}
