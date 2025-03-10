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
    left_join(knz_multitroph_dss[,-which(names(knz_multitroph_dss) %in% c("taxa_type", "guild"))],
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
)

# write.csv(here::here(file = "data/KNZ", "knz_agg_dss.csv"), x = knz_comb, row.names = F)

#### GCE ####
# read data
gce_prod <-  read.csv(here::here("data/GCE", "gce_producers.csv"), row.names = 1)
gce_con <- read.csv(here::here("data/GCE", "gce_consumers.csv"), row.names = 1)

# correct column name
colnames(gce_prod)[colnames(gce_prod) == "scale_abundnace"] <- "scale_abundance"
colnames(gce_con)[colnames(gce_con) == "scale_abundnace"] <- "scale_abundance"

# subset confident ids
gce_prod <- subset(gce_prod, id_confidence == 1)
gce_con <- subset(gce_con, id_confidence == 1)

# consumers are read in as integers due to ID codes. convert all spp to factors
gce_con$taxon_name <- as.factor(gce_con$taxon_name)
gce_prod$taxon_name <- as.factor(gce_prod$taxon_name)

# aggregate to plot level 
gce_prod_mean <- gce_prod %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance)) 

gce_con_mean <- gce_con %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance))

# calculate synchrony
# NOTE - GCE does not have high producer or consumer richness - only 9 species of producers and 5 species of consumers
# may make DSS relationships problematic and also makes it so synchrony cannot be calculated
# will replace with NAs for now
# gce_prod_synch <- codyn::synchrony(gce_prod_mean,
#                                    abundance.var = "abundance",
#                                    species.var = "taxon_name",
#                                    time.var = "year",
#                                    metric = "Loreau",
#                                    replicate.var = "plot")
# colnames(gce_prod_synch) <- c("plot", "prod_synchrony")

# gce_con_synch <- codyn::synchrony(gce_con_mean,
#                                   abundance.var = "abundance",
#                                   species.var = "taxon_name",
#                                   time.var = "year",
#                                   metric = "Loreau",
#                                   replicate.var = "plot")
# colnames(gce_con_synch) <- c("plot", "con_synchrony")

gce_prod_synch <- data.frame(plot = unique(gce_prod_mean$plot),
                             prod_synchrony = rep(NA, length(unique(gce_prod_mean$plot))))

gce_con_synch <- data.frame(plot = unique(gce_con_mean$plot),
                             con_synchrony = rep(NA, length(unique(gce_con_mean$plot))))

# pivot wider for diversity
gce_prod_wide <- gce_prod_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

gce_con_wide <- gce_con_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

# specify meta columns for diversity calculations
long_meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome",
                    "guild", "plot", "year", "taxon_name", "unit_abundance", "scale_abundance")

# calculate diversity
gce_prod_diversity <- 
  data.frame(
    site = gce_prod_wide$site,
    taxa_type = gce_prod_wide$taxa_type,
    ecosystem = gce_prod_wide$ecosystem,
    habitat_broad = gce_prod_wide$habitat_broad,
    habitat_fine = gce_prod_wide$habitat_fine,
    biome = gce_prod_wide$biome,
    guild = gce_prod_wide$guild,
    plot = gce_prod_wide$plot,
    year = gce_prod_wide$year,
    richness = rowSums(gce_prod_wide[, -which(names(gce_prod_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(gce_prod_wide[, -which(names(gce_prod_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(gce_prod_wide[, -which(names(gce_prod_wide) %in% long_meta_cols)], "shannon")
  )

gce_con_diversity <- 
  data.frame(
    site = gce_con_wide$site,
    taxa_type = gce_con_wide$taxa_type,
    ecosystem = gce_con_wide$ecosystem,
    habitat_broad = gce_con_wide$habitat_broad,
    habitat_fine = gce_con_wide$habitat_fine,
    biome = gce_con_wide$biome,
    guild = gce_con_wide$guild,
    plot = gce_con_wide$plot,
    year = gce_con_wide$year,
    richness = rowSums(gce_con_wide[, -which(names(gce_con_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(gce_con_wide[, -which(names(gce_con_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(gce_con_wide[, -which(names(gce_con_wide) %in% long_meta_cols)], "shannon")
  )

# create dss dataframe
gce_prod_dss <- gce_prod_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    prod_richness = mean(richness, na.rm = T), 
    prod_shannon = mean(shannon, na.rm = T), 
    prod_abundance = mean(abundance, na.rm = T),
    prod_cv = CV(abundance),
    prod_stability = stability(abundance)) %>%
  dplyr::left_join(gce_prod_synch, by = "plot")

gce_con_dss <- gce_con_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    con_richness = mean(richness, na.rm = T), 
    con_shannon = mean(shannon, na.rm = T), 
    con_abundance = mean(abundance, na.rm = T),
    con_cv = CV(abundance),
    con_stability = stability(abundance)) %>%
  dplyr::left_join(gce_con_synch, by = "plot")


# calculate multitrophic stability
# combine producers and consumers
gce_multitroph <- merge(gce_prod_wide[,-which(names(gce_prod_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        gce_con_wide[,-which(names(gce_con_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        by = c("site", "ecosystem", "habitat_broad", "habitat_fine", "biome", "plot", "year"))


# aggregate to calculate multritophic abundance and diversity
gce_multitroph_diversity <- 
  data.frame(
    site = gce_multitroph$site,
    taxa_type = rep("multitroph", nrow(gce_multitroph)),
    ecosystem = gce_multitroph$ecosystem,
    habitat_broad = gce_multitroph$habitat_broad,
    habitat_fine = gce_multitroph$habitat_fine,
    biome = gce_multitroph$biome,
    guild = rep("multitroph", nrow(gce_multitroph)),
    plot = gce_multitroph$plot,
    year = gce_multitroph$year,
    richness = rowSums(gce_multitroph[, -which(names(gce_multitroph) %in% long_meta_cols)] > 0),
    abundance = rowSums(gce_multitroph[, -which(names(gce_multitroph) %in% long_meta_cols)]),
    shannon = vegan::diversity(gce_multitroph[, -which(names(gce_multitroph) %in% long_meta_cols)], "shannon")
  )

# calculate diversity and stability
gce_multitroph_dss <- gce_multitroph_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    multitroph_richness = mean(richness, na.rm = T), 
    multitroph_shannon = mean(shannon, na.rm = T), 
    multitroph_abundance = mean(abundance, na.rm = T),
    multitroph_cv = CV(abundance),
    multitroph_stability = stability(abundance))

# combine dfs for SEM
gce_comb <- ungroup(
  left_join(gce_prod_dss[,-which(names(gce_prod_dss) %in% c("taxa_type", "guild"))],
            gce_con_dss[,-which(names(gce_con_dss) %in% c("taxa_type", "guild"))], 
            by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem")) %>%
    left_join(gce_multitroph_dss[,-which(names(gce_multitroph_dss) %in% c("taxa_type", "guild"))],
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
)
# replace NaNs with NAs to avoid issues down the line
gce_comb <- gce_comb %>% mutate_all(~ifelse(is.nan(.), NA, .))
# write.csv(here::here(file = "data/GCE", "gce_agg_dss.csv"), x = gce_comb, row.names = F)




#### kbs ####
# read data
kbs_prod <-  read.csv(here::here("data/KBS", "kbs_producer.csv"))
kbs_con <- read.csv(here::here("data/KBS", "kbs_consumer.csv"))

# subset confident ids - I am not subsetting kbs_con for specific trophic level at this point Feb 23 2025
kbs_prod <- subset(kbs_prod, id_confidence == 1)
kbs_con <- subset(kbs_con, id_confidence == 1)

# consumers are read in as integers due to ID codes. convert all spp to factors
kbs_con$taxon_name <- as.factor(kbs_con$taxon_name)
kbs_prod$taxon_name <- as.factor(kbs_prod$taxon_name)

# aggregate to plot level 
kbs_prod_mean <- kbs_prod %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance)) 

kbs_con_mean <- kbs_con %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance))

# calculate synchrony
kbs_prod_synch <- codyn::synchrony(kbs_prod_mean,
                                   abundance.var = "abundance",
                                   species.var = "taxon_name",
                                   time.var = "year",
                                   metric = "Loreau",
                                   replicate.var = "plot")
colnames(kbs_prod_synch) <- c("plot", "prod_synchrony")

kbs_con_synch <- codyn::synchrony(kbs_con_mean,
                                  abundance.var = "abundance",
                                  species.var = "taxon_name",
                                  time.var = "year",
                                  metric = "Loreau",
                                  replicate.var = "plot")
colnames(kbs_con_synch) <- c("plot", "con_synchrony")


# pivot wider for diversity
kbs_prod_wide <- kbs_prod_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

kbs_con_wide <- kbs_con_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

# specify meta columns for diversity calculations
long_meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome",
                    "guild", "plot", "year", "taxon_name", "unit_abundance", "scale_abundance")

# calculate diversity
kbs_prod_diversity <- 
  data.frame(
    site = kbs_prod_wide$site,
    taxa_type = kbs_prod_wide$taxa_type,
    ecosystem = kbs_prod_wide$ecosystem,
    habitat_broad = kbs_prod_wide$habitat_broad,
    habitat_fine = kbs_prod_wide$habitat_fine,
    biome = kbs_prod_wide$biome,
    guild = kbs_prod_wide$guild,
    plot = kbs_prod_wide$plot,
    year = kbs_prod_wide$year,
    richness = rowSums(kbs_prod_wide[, -which(names(kbs_prod_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(kbs_prod_wide[, -which(names(kbs_prod_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(kbs_prod_wide[, -which(names(kbs_prod_wide) %in% long_meta_cols)], "shannon")
  )

kbs_con_diversity <- 
  data.frame(
    site = kbs_con_wide$site,
    taxa_type = kbs_con_wide$taxa_type,
    ecosystem = kbs_con_wide$ecosystem,
    habitat_broad = kbs_con_wide$habitat_broad,
    habitat_fine = kbs_con_wide$habitat_fine,
    biome = kbs_con_wide$biome,
    guild = kbs_con_wide$guild,
    plot = kbs_con_wide$plot,
    year = kbs_con_wide$year,
    richness = rowSums(kbs_con_wide[, -which(names(kbs_con_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(kbs_con_wide[, -which(names(kbs_con_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(kbs_con_wide[, -which(names(kbs_con_wide) %in% long_meta_cols)], "shannon")
  )

# create dss dataframe
kbs_prod_dss <- kbs_prod_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    prod_richness = mean(richness, na.rm = T), 
    prod_shannon = mean(shannon, na.rm = T), 
    prod_abundance = mean(abundance, na.rm = T),
    prod_cv = CV(abundance),
    prod_stability = stability(abundance)) %>%
  dplyr::left_join(kbs_prod_synch, by = "plot")

kbs_con_dss <- kbs_con_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    con_richness = mean(richness, na.rm = T), 
    con_shannon = mean(shannon, na.rm = T), 
    con_abundance = mean(abundance, na.rm = T),
    con_cv = CV(abundance),
    con_stability = stability(abundance)) %>%
  dplyr::left_join(kbs_con_synch, by = "plot")


# calculate multitrophic stability
# combine producers and consumers
kbs_multitroph <- merge(kbs_prod_wide[,-which(names(kbs_prod_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        kbs_con_wide[,-which(names(kbs_con_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        by = c("site", "ecosystem", "habitat_broad", "habitat_fine", "biome", "plot", "year"))


# aggregate to calculate multritophic abundance and diversity
kbs_multitroph_diversity <- 
  data.frame(
    site = kbs_multitroph$site,
    taxa_type = rep("multitroph", nrow(kbs_multitroph)),
    ecosystem = kbs_multitroph$ecosystem,
    habitat_broad = kbs_multitroph$habitat_broad,
    habitat_fine = kbs_multitroph$habitat_fine,
    biome = kbs_multitroph$biome,
    guild = rep("multitroph", nrow(kbs_multitroph)),
    plot = kbs_multitroph$plot,
    year = kbs_multitroph$year,
    richness = rowSums(kbs_multitroph[, -which(names(kbs_multitroph) %in% long_meta_cols)] > 0),
    abundance = rowSums(kbs_multitroph[, -which(names(kbs_multitroph) %in% long_meta_cols)]),
    shannon = vegan::diversity(kbs_multitroph[, -which(names(kbs_multitroph) %in% long_meta_cols)], "shannon")
  )

# calculate diversity and stability
kbs_multitroph_dss <- kbs_multitroph_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    multitroph_richness = mean(richness, na.rm = T), 
    multitroph_shannon = mean(shannon, na.rm = T), 
    multitroph_abundance = mean(abundance, na.rm = T),
    multitroph_cv = CV(abundance),
    multitroph_stability = stability(abundance))

# combine dfs for SEM
kbs_comb <- ungroup(
  left_join(kbs_prod_dss[,-which(names(kbs_prod_dss) %in% c("taxa_type", "guild"))],
            kbs_con_dss[,-which(names(kbs_con_dss) %in% c("taxa_type", "guild"))], 
            by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem")) %>%
    left_join(kbs_multitroph_dss[,-which(names(kbs_multitroph_dss) %in% c("taxa_type", "guild"))],
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
)

# write.csv(here::here(file = "data/KBS", "kbs_agg_dss.csv"), x = kbs_comb, row.names = F)

#### CDR biodiversity exp data ####
# read data and subset confident IDs
cdr_biodiv_prod <-  read.csv(here::here("data/CDR_biodiv", "cdr_producer.csv"))
cdr_biodiv_prod <- subset(cdr_biodiv_prod, id_confidence == 1)

cdr_biodiv_con <- read.csv(here::here("data/CDR_biodiv", "cdr_consumer.csv"))
cdr_biodiv_con <- subset(cdr_biodiv_con, id_confidence == 1)
# cdr_biodiv_con abundances being read in as characters still. Convert to numeric
cdr_biodiv_con$abundance <- as.numeric(cdr_biodiv_con$abundance)



# consumers are read in as integers due to ID codes. convert all spp to factors
cdr_biodiv_con$taxon_name <- as.factor(cdr_biodiv_con$taxon_name)
cdr_biodiv_prod$taxon_name <- as.factor(cdr_biodiv_prod$taxon_name)

# aggregate to plot level 
cdr_biodiv_prod_mean <- cdr_biodiv_prod %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance)) 

cdr_biodiv_con_mean <- cdr_biodiv_con %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, taxon_name, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance))

# calculate synchrony 
cdr_biodiv_prod_synch <- codyn::synchrony(cdr_biodiv_prod_mean,
                                   abundance.var = "abundance",
                                   species.var = "taxon_name",
                                   time.var = "year",
                                   metric = "Loreau",
                                   replicate.var = "plot")
colnames(cdr_biodiv_prod_synch) <- c("plot", "prod_synchrony")

# NOTE some plots contain only a single consumer species at some timepoijts
# need to subset and replace with NA
# alternative would be to relace with 1s (a species is perfectly synchronous to self) but doesn't see appropriate given 
# that it's only a few years at a time
# See code for craven et al. 2018: https://github.com/idiv-biodiversity/StabilityII/blob/master/TS_extendedSEM.R

problematic_plots <- c()  # Empty vector to store problem plots

for (p in unique(cdr_biodiv_con_mean$plot)) {
  cat("Processing plot:", p, "\n")  # Show progress
  
  plot_data <- cdr_biodiv_con_mean %>% filter(plot == p)  # Subset data for one plot
  
  result <- tryCatch({
    codyn::synchrony(plot_data,
                     abundance.var = "abundance",
                     species.var = "taxon_name",
                     time.var = "year",
                     metric = "Loreau",
                     replicate.var = "plot")
    TRUE  # If success, return TRUE
  }, error = function(e) {
    cat("Error in plot:", p, "\nMessage:", e$message, "\n")  # Print error message
    problematic_plots <<- c(problematic_plots, p)  # Save the problem plot
    FALSE  # Return FALSE to indicate failure
  })
}

cat("Problematic plots:", problematic_plots, "\n") # problematic plot is 248 - almost always only had one species present

# remove and replace with NAs at the end

cdr_biodiv_con_filtered <- subset(cdr_biodiv_con_mean, plot != 248)


cdr_biodiv_con_synch <- codyn::synchrony(cdr_biodiv_con_filtered,
                                  abundance.var = "abundance",
                                  species.var = "taxon_name",
                                  time.var = "year",
                                  metric = "Loreau",
                                  replicate.var = "plot")
colnames(cdr_biodiv_con_synch) <- c("plot", "con_synchrony")

# append 248 back in with synchrony = NA
plot_248_synch <- data.frame(
  plot = 248,
  con_synchrony = NA
)

cdr_biodiv_con_synch <- rbind(cdr_biodiv_con_synch, plot_248_synch)


# pivot wider for diversity
cdr_biodiv_prod_wide <- cdr_biodiv_prod_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

cdr_biodiv_con_wide <- cdr_biodiv_con_mean %>%
  pivot_wider(names_from = taxon_name, values_from = abundance, values_fill = list(abundance = 0))

# specify meta columns for diversity calculations
long_meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome",
                    "guild", "plot", "year", "taxon_name", "unit_abundance", "scale_abundance")

# calculate diversity
cdr_biodiv_prod_diversity <- 
  data.frame(
    site = cdr_biodiv_prod_wide$site,
    taxa_type = cdr_biodiv_prod_wide$taxa_type,
    ecosystem = cdr_biodiv_prod_wide$ecosystem,
    habitat_broad = cdr_biodiv_prod_wide$habitat_broad,
    habitat_fine = cdr_biodiv_prod_wide$habitat_fine,
    biome = cdr_biodiv_prod_wide$biome,
    guild = cdr_biodiv_prod_wide$guild,
    plot = cdr_biodiv_prod_wide$plot,
    year = cdr_biodiv_prod_wide$year,
    richness = rowSums(cdr_biodiv_prod_wide[, -which(names(cdr_biodiv_prod_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(cdr_biodiv_prod_wide[, -which(names(cdr_biodiv_prod_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(cdr_biodiv_prod_wide[, -which(names(cdr_biodiv_prod_wide) %in% long_meta_cols)], "shannon")
  )

cdr_biodiv_con_diversity <- 
  data.frame(
    site = cdr_biodiv_con_wide$site,
    taxa_type = cdr_biodiv_con_wide$taxa_type,
    ecosystem = cdr_biodiv_con_wide$ecosystem,
    habitat_broad = cdr_biodiv_con_wide$habitat_broad,
    habitat_fine = cdr_biodiv_con_wide$habitat_fine,
    biome = cdr_biodiv_con_wide$biome,
    guild = cdr_biodiv_con_wide$guild,
    plot = cdr_biodiv_con_wide$plot,
    year = cdr_biodiv_con_wide$year,
    richness = rowSums(cdr_biodiv_con_wide[, -which(names(cdr_biodiv_con_wide) %in% long_meta_cols)] > 0),
    abundance = rowSums(cdr_biodiv_con_wide[, -which(names(cdr_biodiv_con_wide) %in% long_meta_cols)]),
    shannon = vegan::diversity(cdr_biodiv_con_wide[, -which(names(cdr_biodiv_con_wide) %in% long_meta_cols)], "shannon")
  )

# create dss dataframe
cdr_biodiv_prod_dss <- cdr_biodiv_prod_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    prod_richness = mean(richness), 
    prod_shannon = mean(shannon), 
    prod_abundance = mean(abundance),
    prod_cv = CV(abundance),
    prod_stability = stability(abundance)) %>%
  dplyr::left_join(cdr_biodiv_prod_synch, by = "plot")

cdr_biodiv_con_dss <- cdr_biodiv_con_diversity %>%
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
cdr_biodiv_multitroph <- merge(cdr_biodiv_prod_wide[,-which(names(cdr_biodiv_prod_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        cdr_biodiv_con_wide[,-which(names(cdr_biodiv_con_wide) %in% c("guild", "taxa_type", "unit_abundance", "scale_abundance"))],
                        by = c("site", "ecosystem", "habitat_broad", "habitat_fine", "biome", "plot", "year"))


# aggregate to calculate multritophic abundance and diversity
cdr_biodiv_multitroph_diversity <- 
  data.frame(
    site = cdr_biodiv_multitroph$site,
    taxa_type = rep("multitroph", nrow(cdr_biodiv_multitroph)),
    ecosystem = cdr_biodiv_multitroph$ecosystem,
    habitat_broad = cdr_biodiv_multitroph$habitat_broad,
    habitat_fine = cdr_biodiv_multitroph$habitat_fine,
    biome = cdr_biodiv_multitroph$biome,
    guild = rep("multitroph", nrow(cdr_biodiv_multitroph)),
    plot = cdr_biodiv_multitroph$plot,
    year = cdr_biodiv_multitroph$year,
    richness = rowSums(cdr_biodiv_multitroph[, -which(names(cdr_biodiv_multitroph) %in% long_meta_cols)] > 0),
    abundance = rowSums(cdr_biodiv_multitroph[, -which(names(cdr_biodiv_multitroph) %in% long_meta_cols)]),
    shannon = vegan::diversity(cdr_biodiv_multitroph[, -which(names(cdr_biodiv_multitroph) %in% long_meta_cols)], "shannon")
  )

# calculate diversity and stability
cdr_biodiv_multitroph_dss <- cdr_biodiv_multitroph_diversity %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot) %>%
  dplyr::summarise(
    multitroph_richness = mean(richness), 
    multitroph_shannon = mean(shannon), 
    multitroph_abundance = mean(abundance),
    multitroph_cv = CV(abundance),
    multitroph_stability = stability(abundance))

# combine dfs for SEM
cdr_biodiv_comb <- ungroup(
  left_join(cdr_biodiv_prod_dss[,-which(names(cdr_biodiv_prod_dss) %in% c("taxa_type", "guild"))],
            cdr_biodiv_con_dss[,-which(names(cdr_biodiv_con_dss) %in% c("taxa_type", "guild"))], 
            by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem")) %>%
    left_join(cdr_biodiv_multitroph_dss[,-which(names(cdr_biodiv_multitroph_dss) %in% c("taxa_type", "guild"))],
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
)

# write.csv(here::here(file = "data/CDR_biodiv", "cdr_agg_dss.csv"), x = cdr_biodiv_comb, row.names = F)


#### Combine terrestrial datasets ####
terr_comb <- rbind(knz_comb, kbs_comb, gce_comb, cdr_biodiv_comb)
# write.csv(here::here(file = "data/synthesized_data", "temp_terrestrial_agg_dss.csv"), x = terr_comb, row.names = F)


#### SBC ####
# NOTE: NEED TO MAKE GROUP DECISIONS ON HOW TO DEAL WITH CONSUMER DATASET OR ELSE IT WON'T MERGE WITH THE PRODUCERS
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
            by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem")) %>%
    left_join(sbc_multitroph_dss[,-which(names(sbc_multitroph_dss) %in% c("taxa_type", "guild"))],
              by = c("site", "plot", "habitat_broad", "habitat_fine", "biome", "ecosystem"))
)
