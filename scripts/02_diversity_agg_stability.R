rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)


#  create aggregate diversity stability dataframe
calculate_agg_stability <- function(producer_data, # synthesized producer data after filtering
                                    consumer_data, # synthesized consumer data after filtering
                                    ecosystem_type, # vector c("Terrestrial", "Marine") indictaing ecosystem type
                                    write_csv = FALSE # option to automatically write csv
) {
  
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
      richness = rowSums(producer_data[, -which(names(producer_data) %in% long_meta_cols)] > 0),
      abundance = rowSums(producer_data[, -which(names(producer_data) %in% long_meta_cols)]),
      shannon = vegan::diversity(producer_data[, -which(names(producer_data) %in% long_meta_cols)], "shannon")
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
      richness = rowSums(consumer_data[, -which(names(consumer_data) %in% long_meta_cols)] > 0),
      abundance = rowSums(consumer_data[, -which(names(consumer_data) %in% long_meta_cols)]),
      shannon = vegan::diversity(consumer_data[, -which(names(consumer_data) %in% long_meta_cols)], "shannon")
    )
  
  # create dss dataframes for producers and consumers
  producer_dss <- producer_diversity %>%
    dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
    dplyr::summarise(
      prod_richness = mean(richness), 
      prod_shannon = mean(shannon), 
      prod_abundance = mean(abundance),
      prod_cv = CV(abundance),
      prod_stability = stability(abundance)) %>%
    dplyr::left_join(cdr_biodiv_prod_synch, by = "plot")
  
  consumer_dss <- consumer_diversity %>%
    dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
    dplyr::summarise(
      con_richness = mean(richness), 
      con_shannon = mean(shannon), 
      con_abundance = mean(abundance),
      con_cv = CV(abundance),
      con_stability = stability(abundance)) %>%
    dplyr::left_join(cdr_biodiv_con_synch, by = "plot")
  
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
      richness = rowSums(multitrophic_data[, -which(names(multitrophic_data) %in% long_meta_cols)] > 0),
      abundance = rowSums(multitrophic_data[, -which(names(multitrophic_data) %in% long_meta_cols)]),
      shannon = vegan::diversity(multitrophic_data[, -which(names(multitrophic_data) %in% long_meta_cols)], "shannon")
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
  
  if (write_csv) {
    print("Writing to CSV...")
    write_csv(producer_dss, paste0(producer_object_name, ".csv"))
    write_csv(consumer_dss, paste0(consumer_object_name, ".csv"))
    write_csv(multitrophic_dss, paste0(multitrophic_object_name, ".csv"))
    
  }
}
