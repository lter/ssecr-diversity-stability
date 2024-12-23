# Noam Altman-Kurosaki
# Konza multitrophic analyses

librarian::shelf(vegan, lme4, car, dplyr, tidyr, lattice, lavaan, piecewiseSEM, ggplot2, codyn)


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

# read data
knz_prod <- read.csv(here::here("data", "knz_producer.csv"))
knz_con <- read.csv(here::here("data", "knz_consumer.csv"))

# consumers are read in as integers due to ID codes. convert all spp to factors
knz_con$species <- as.factor(knz_con$species)
knz_prod$species <- as.factor(knz_prod$species)

# aggregate to plot level - NOTE: dividing by 10 to get areal abundance per 10m2 plot. Not converting to proportional cover
knz_prod_mean <- knz_prod %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, species, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance)/10) 

knz_con_mean <- knz_con %>%
  dplyr::group_by(site, taxa_type, ecosystem, habitat_broad, habitat_fine, biome, guild, plot, year, species, unit_abundance, scale_abundance) %>%
  dplyr::summarise(abundance = mean(abundance))

# calculate synchrony
knz_prod_synch <- codyn::synchrony(knz_prod_mean,
                                   abundance.var = "abundance",
                                   species.var = "species",
                                   time.var = "year",
                                   metric = "Loreau",
                                   replicate.var = "plot")
colnames(knz_prod_synch) <- c("plot", "prod_synchrony")
knz_con_synch <- codyn::synchrony(knz_con_mean,
                                   abundance.var = "abundance",
                                   species.var = "species",
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
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0))

knz_con_wide <- knz_con_mean %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0))

# specify meta columns for diversity calculations
long_meta_cols <- c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome",
                    "guild", "plot", "year", "species", "unit_abundance", "scale_abundance")

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

# visualize relationships
pairs(knz_prod_dss[, -which(names(knz_prod_dss) %in% long_meta_cols)])
pairs(knz_con_dss[, -which(names(knz_con_dss) %in% long_meta_cols)])

#### basic glms ####
# when I have a synthesized dataset I would use site as a random effect both here and below in the SEM approach

# producers
knz_prod_divstab_mod <- glm(prod_stability ~ prod_shannon, family = Gamma("log"), data = knz_prod_dss)
plot(knz_prod_divstab_mod) # some patterning in the residuals - tested with transformations. Seems due to low sample size
car::Anova(knz_prod_divstab_mod) # X2 = 4.9, P = 0.03

knz_prod_richstab_mod <- glm(prod_stability ~ prod_richness, family = Gamma("log"), data = knz_prod_dss)
plot(knz_prod_richstab_mod) # some patterning in the residuals but a little better than shannon
car::Anova(knz_prod_richstab_mod) # X2 = 4.2, P = 0.04

knz_prod_synchstab_mod <- glm(prod_stability ~ prod_synchrony, family = Gamma("log"), data = knz_prod_dss)
plot(knz_prod_synchstab_mod) # strong patterning in residuals - due to low range and near uniformity of synchrony?
car::Anova(knz_prod_synchstab_mod) # X2 = 1.2606, P = 0.2615

# consumers
knz_con_divstab_mod <- glm(con_stability ~ con_shannon, family = Gamma("log"), data = knz_con_dss)
plot(knz_con_divstab_mod) # some patterning in the residuals - tested with transformations. Seems due to low sample size
car::Anova(knz_con_divstab_mod) # X2 = 0.49, P = 0.48

knz_con_richstab_mod <- glm(con_stability ~ con_richness, family = Gamma("log"), data = knz_con_dss)
plot(knz_con_richstab_mod) # looks good
car::Anova(knz_con_richstab_mod) # X2 = 1.5, P = 0.22

knz_con_synchstab_mod <- glm(con_stability ~ con_synchrony, family = Gamma("log"), data = knz_con_dss)
plot(knz_con_synchstab_mod) # strong patterning in residuals but not as bad as producer dataset
car::Anova(knz_con_synchstab_mod) # X2 = 30.6, P = 5.8E-06


##### structural equation modeling approach #####
### calculate multitrophic stability
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

mod_list <- list(
  lm(log(prod_stability, 2) ~ prod_shannon + prod_synchrony + con_shannon,  data = knz_comb),
  lm(log(con_stability, 2) ~ con_shannon + con_synchrony + prod_shannon,  data = knz_comb),
  lm(log(multitroph_stability,2) ~ prod_stability + con_stability,  data = knz_comb)
)

# check residuals of all the models
# lapply(mod_list, plot) # some look a little wonky - talk to nick about troubleshooting this
# might have to standardize/use transformed vars, then use lme/lms instead of Gamma

# run sem - need to collapse the separate models into a single multiple regression - look into this further
knz_sem <- psem(
  lm(log(prod_stability, 2) ~ prod_shannon + prod_synchrony + con_shannon,  data = knz_comb),
  lm(log(con_stability, 2) ~ con_shannon + con_synchrony + prod_shannon,  data = knz_comb),
  lm(log(multitroph_stability,2) ~ prod_stability + con_stability,  data = knz_comb),
  data = knz_comb)
semOutput(knz_sem)
