rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan, piecewiseSEM, semPlot, ggplot2, codyn, ggpubr)


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

# dss metadata
dss_meta <- c("site", "ecosystem", "habitat_broad", "habitat_fine", "biome", "plot")

#### KNZ ####
# read data
knz_prod <-  read.csv(here::here("data/KNZ", "knz_producer.csv"))
knz_con <- read.csv(here::here("data/KNZ", "knz_consumer.csv"))

# consumers are read in as integers due to ID codes. convert all spp to factors
knz_con$taxon_name <- as.factor(knz_con$taxon_name)
knz_prod$taxon_name <- as.factor(knz_prod$taxon_name)

# aggregate to plot level - NOTE: dividing by 10 to get areal abundance per 10m2 plot. Not converting to proportional cover
knz_prod_mean <- knz_prod %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance)/10) 

knz_con_mean <- knz_con %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance))

# calculate synchrony
knz_prod_synch <- codyn::synchrony(knz_prod_mean,
                                   abundance.var = "abundance",
                                   species.var = "taxon_name",
                                   time.var = "year",
                                   metric = "Loreau",
                                   replicate.var = "plot")
colnames(knz_prod_synch) <- c("plot", "prod_synchrony")
knz_con_synch <- codyn::synchrony(knz_con_mean,
                                  abundance.var = "abundance",
                                  species.var = "taxon_name",
                                  time.var = "year",
                                  metric = "Loreau",
                                  replicate.var = "plot")
colnames(knz_con_synch) <- c("plot", "con_synchrony")
# not all plots are replicated between producer and consumer datasets
# remove plots in producers that are not represented in consumer dataset and vice-versa
knz_prod_synch_sub <- knz_prod_synch[which(knz_prod_synch$plot %in% knz_con_synch$plot),]
knz_con_synch_sub <- knz_con_synch[which(knz_con_synch$plot %in% knz_prod_synch_sub$plot),]

knz_prod_mean <- knz_prod_mean[which(knz_prod_mean$plot %in% knz_prod_synch_sub$plot),]
knz_con_mean <- knz_con_mean[which(knz_con_mean$plot %in% knz_prod_synch_sub$plot),]

# pivot wider for diversity
knz_prod_wide <- knz_prod_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

knz_con_wide <- knz_con_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

# specify meta columns for diversity calculations
long_meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome",
                    "guild", "plot", "year", "taxon_name", "unit_abundance", "scale_abundance")

# calculate diversity
knz_prod_diversity <- 
  data.frame(
    site = knz_prod_wide$site,
    taxa_type = knz_prod_wide$taxa_type,
    ecosystem = knz_prod_wide$ecosystem,
    habitat_broad = knz_prod_wide$habitat_broad,
    habitat_fine = knz_prod_wide$habitat_fine,
    biome = knz_prod_wide$biome,
    guild = knz_prod_wide$guild,
    plot = knz_prod_wide$plot,
    year = knz_prod_wide$year,
    richness = rowSums(knz_prod_wide[, -which(names(knz_prod_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(knz_prod_wide[, -which(names(knz_prod_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(knz_prod_wide[, -which(names(knz_prod_wide) %in% long_meta_cols)], "shannon")
  )

knz_con_diversity <- 
  data.frame(
    site = knz_con_wide$site,
    taxa_type = knz_con_wide$taxa_type,
    ecosystem = knz_con_wide$ecosystem,
    habitat_broad = knz_con_wide$habitat_broad,
    habitat_fine = knz_con_wide$habitat_fine,
    biome = knz_con_wide$biome,
    guild = knz_con_wide$guild,
    plot = knz_con_wide$plot,
    year = knz_con_wide$year,
    richness = rowSums(knz_con_wide[, -which(names(knz_con_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(knz_con_wide[, -which(names(knz_con_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(knz_con_wide[, -which(names(knz_con_wide) %in% long_meta_cols)], "shannon")
  )

# create dss dataframe
knz_prod_dss <- knz_prod_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    prod_richness = mean(richness), 
    prod_shannon = mean(shannon), 
    prod_abundance = mean(abundance),
    prod_cv = CV(abundance),
    prod_stability = stability(abundance)) %>%
  dplyr::left_join(knz_prod_synch_sub, by = "plot")

knz_con_dss <- knz_con_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    con_richness = mean(richness), 
    con_shannon = mean(shannon), 
    con_abundance = mean(abundance),
    con_cv = CV(abundance),
    con_stability = stability(abundance)) %>%
  dplyr::left_join(knz_con_synch_sub, by = "plot")


# calculate multitrophic stability
# combine producers and consumers
knz_multitroph <- merge(knz_prod_wide[,-which(names(knz_prod_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        knz_con_wide[,-which(names(knz_con_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        by = c("site", "ecosystem", "habitat_broad", "habitat_fine", "biome", "plot", "year"))


# aggregate to calculate multritophic abundance and diversity
knz_multitroph_diversity <- 
  data.frame(
    site = knz_multitroph$site,
    taxa_type = rep("multitroph", nrow(knz_multitroph)),
    ecosystem = knz_multitroph$ecosystem,
    habitat_broad = knz_multitroph$habitat_broad,
    habitat_fine = knz_multitroph$habitat_fine,
    biome = knz_multitroph$biome,
    guild = rep("multitroph", nrow(knz_multitroph)),
    plot = knz_multitroph$plot,
    year = knz_multitroph$year,
    richness = rowSums(knz_multitroph[, -which(names(knz_multitroph) %in% long_meta_cols)] > 0),
    abundance = rowSums(knz_multitroph[, -which(names(knz_multitroph) %in% long_meta_cols)]),
    shannon = vegan::diversity(knz_multitroph[, -which(names(knz_multitroph) %in% long_meta_cols)], "shannon")
  )

# calculate diversity and stability
knz_multitroph_dss <- knz_multitroph_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    multitroph_richness = mean(richness), 
    multitroph_shannon = mean(shannon), 
    multitroph_abundance = mean(abundance),
    multitroph_cv = CV(abundance),
    multitroph_stability = stability(abundance))

# combine dfs for SEM
knz_comb <- ungroup(
  left_join(knz_prod_dss[,-which(names(knz_prod_dss) %in% c("taxa_type", "guild"))],
            knz_con_dss[,-which(names(knz_con_dss) %in% c("taxa_type", "guild"))], 
            by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem")) %>%
    left_join(knz_multitroph_dss,
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
)


#### SBC ####
# read data
sbc_all <- read.csv(here::here("data/SBC", "sbc_all.csv"))
# subset confident ID and remove missing transects (-9999)
sbc_all <- subset(sbc_all, abundance != -99999)
sbc_all <- subset(sbc_all, id_confidence == 1)
sbc_all_new <- sbc_all[ , !(names(sbc_all) %in% "id_confidence")]

# subset producers and consumers
sbc_prod <-  subset(sbc_all_new, guild == "algae")
# NOTE: consumers contains both fish and inverts, which may be problematic
sbc_con <- subset(sbc_all_new, guild != "algae") 

# consumers are read in as integers due to ID codes. convert all spp to factors
sbc_con$taxon_name <- as.factor(sbc_con$taxon_name)
sbc_prod$taxon_name <- as.factor(sbc_prod$taxon_name)

# aggregate to plot level
sbc_prod_mean <- sbc_prod %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance)) 

sbc_con_mean <- sbc_con %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance))

# calculate synchrony
sbc_prod_synch <- codyn::synchrony(sbc_prod_mean,
                                   abundance.var = "abundance",
                                   species.var = "taxon_name",
                                   time.var = "year",
                                   metric = "Loreau",
                                   replicate.var = "plot")
colnames(sbc_prod_synch) <- c("plot", "prod_synchrony")
sbc_con_synch <- codyn::synchrony(sbc_con_mean,
                                  abundance.var = "abundance",
                                  species.var = "taxon_name",
                                  time.var = "year",
                                  metric = "Loreau",
                                  replicate.var = "plot")
colnames(sbc_con_synch) <- c("plot", "con_synchrony")

# pivot wider for diversity
sbc_prod_wide <- sbc_prod_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

sbc_con_wide <- sbc_con_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

# specify meta columns for diversity calculations
long_meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome",
                    "guild", "plot", "year", "taxon_name", "unit_abundance", "scale_abundance")

# calculate diversity
sbc_prod_diversity <- 
  data.frame(
    site = sbc_prod_wide$site,
    taxa_type = sbc_prod_wide$taxa_type,
    ecosystem = sbc_prod_wide$ecosystem,
    habitat_broad = sbc_prod_wide$habitat_broad,
    habitat_fine = sbc_prod_wide$habitat_fine,
    biome = sbc_prod_wide$biome,
    guild = sbc_prod_wide$guild,
    plot = sbc_prod_wide$plot,
    year = sbc_prod_wide$year,
    richness = rowSums(sbc_prod_wide[, -which(names(sbc_prod_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(sbc_prod_wide[, -which(names(sbc_prod_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(sbc_prod_wide[, -which(names(sbc_prod_wide) %in% long_meta_cols)], "shannon")
  )


sbc_con_diversity <- 
  data.frame(
    site = sbc_con_wide$site,
    taxa_type = sbc_con_wide$taxa_type,
    ecosystem = sbc_con_wide$ecosystem,
    habitat_broad = sbc_con_wide$habitat_broad,
    habitat_fine = sbc_con_wide$habitat_fine,
    biome = sbc_con_wide$biome,
    guild = sbc_con_wide$guild,
    plot = sbc_con_wide$plot,
    year = sbc_con_wide$year,
    richness = rowSums(sbc_con_wide[, -which(names(sbc_con_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(sbc_con_wide[, -which(names(sbc_con_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(sbc_con_wide[, -which(names(sbc_con_wide) %in% long_meta_cols)], "shannon")
  )

# create dss dataframe - NOTE: invsimpson created inf values for dss here... need to look into further
# reverted back to shannon for now
sbc_prod_dss <- sbc_prod_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    prod_richness = mean(richness), 
    prod_shannon = mean(shannon), 
    prod_abundance = mean(abundance),
    prod_cv = CV(abundance),
    prod_stability = stability(abundance)) %>%
  dplyr::left_join(sbc_prod_synch, by = "plot")

sbc_con_dss <- sbc_con_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    con_richness = mean(richness), 
    con_shannon = mean(shannon), 
    con_abundance = mean(abundance),
    con_cv = CV(abundance),
    con_stability = stability(abundance)) %>%
  dplyr::left_join(sbc_con_synch, by = "plot")


# calculate multitrophic stability
# combine producers and consumers
sbc_multitroph <- merge(sbc_prod_wide[,-which(names(sbc_prod_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        sbc_con_wide[,-which(names(sbc_con_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        by = c("site", "ecosystem", "habitat_broad", "habitat_fine", "biome", "plot", "year"))


# aggregate to calculate multritophic abundance and diversity
sbc_multitroph_diversity <- 
  data.frame(
    site = sbc_multitroph$site,
    taxa_type = rep("multitroph", nrow(sbc_multitroph)),
    ecosystem = sbc_multitroph$ecosystem,
    habitat_broad = sbc_multitroph$habitat_broad,
    habitat_fine = sbc_multitroph$habitat_fine,
    biome = sbc_multitroph$biome,
    guild = rep("multitroph", nrow(sbc_multitroph)),
    plot = sbc_multitroph$plot,
    year = sbc_multitroph$year,
    richness = rowSums(sbc_multitroph[, -which(names(sbc_multitroph) %in% long_meta_cols)] > 0),
    abundance = rowSums(sbc_multitroph[, -which(names(sbc_multitroph) %in% long_meta_cols)]),
    shannon = vegan::diversity(sbc_multitroph[, -which(names(sbc_multitroph) %in% long_meta_cols)], "shannon")
  )

# calculate diversity and stability
sbc_multitroph_dss <- sbc_multitroph_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    multitroph_richness = mean(richness), 
    multitroph_shannon = mean(shannon), 
    multitroph_abundance = mean(abundance),
    multitroph_cv = CV(abundance),
    multitroph_stability = stability(abundance))

# combine dfs for SEM
sbc_comb <- ungroup(
  left_join(sbc_prod_dss[,-which(names(sbc_prod_dss) %in% c("taxa_type", "guild"))],
            sbc_con_dss[,-which(names(sbc_con_dss) %in% c("taxa_type", "guild"))], 
            by = dss_meta) %>%
    left_join(sbc_multitroph_dss,
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
)
