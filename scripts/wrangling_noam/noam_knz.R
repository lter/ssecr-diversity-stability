# Noam Altman-Kurosaki
# Konza multitrophic analyses
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

# log transform/rescale variables for lavaan - NOTE: might be able to do this with glmms with piecewiseSEM in the future
knz_comb$prod_stability_log <- log(knz_comb$prod_stability, 2)
knz_comb$con_stability_log <- log(knz_comb$con_stability, 2)
knz_comb$multitroph_stability_log <- log(knz_comb$multitroph_stability, 2)


# visualize relationships - zoom in here
pairs(knz_comb[, -which(names(knz_comb) %in% long_meta_cols)])


#### basic lms ####
# when I have a synthesized dataset I would use site as a random effect both here and below in the SEM approach

# producers
knz_prod_dss_mod <- lm(prod_stability_log ~ prod_shannon + prod_synchrony + con_shannon, data = knz_comb)
plot(knz_prod_dss_mod) # some patterning in the residuals but not the worst
car::Anova(knz_prod_dss_mod) # X2 = 4.9, P = 0.03
# Sum Sq Df F value    Pr(>F)    
# prod_shannon   0.00048  1  0.0275 0.8716272    
# prod_synchrony 0.40602  1 23.2396 0.0007014 ***
# con_shannon    0.02782  1  1.5926 0.2355946    
# Residuals      0.17471 10    


knz_prod_richstab_mod <- lm(prod_stability_log ~ prod_richness + prod_synchrony + con_shannon, data = knz_comb)
plot(knz_prod_richstab_mod) # slightly stronger patterning than shannon
car::Anova(knz_prod_richstab_mod)
# Sum Sq Df F value    Pr(>F)    
# prod_richness  0.00002  1  0.0014 0.9706503    
# prod_synchrony 0.41442  1 23.6587 0.0006575 ***
#   con_shannon    0.05124  1  2.9255 0.1179816    
# Residuals      0.17516 10     


# consumers
knz_con_dss_mod <- lm(con_stability_log ~ con_shannon + con_synchrony + prod_shannon, data = knz_comb)
plot(knz_con_dss_mod) # some patterning in the residuals - similar to producers
car::Anova(knz_con_dss_mod) 
# Sum Sq Df F value    Pr(>F)    
# con_shannon   0.00000  1  0.0001 0.9939831    
# con_synchrony 0.86956  1 23.3355 0.0006911 ***
# prod_shannon  0.00580  1  0.1557 0.7014250    
# Residuals     0.37263 10     

knz_con_richstab_mod <- lm(con_stability_log ~ con_richness + con_synchrony + prod_shannon, data = knz_comb)
plot(knz_con_richstab_mod) # similar to above
car::Anova(knz_con_richstab_mod)
# Sum Sq Df F value    Pr(>F)    
# con_richness  0.02320  1  0.6639 0.4341697    
# con_synchrony 0.74775  1 21.3986 0.0009421 ***
# prod_shannon  0.00188  1  0.0538 0.8212512    
# Residuals     0.34944 10                   

# multitrophic
knz_multitroph_dss_mod <- lm(multitroph_stability_log ~ con_stability_log + prod_stability_log, data = knz_comb)
plot(knz_multitroph_dss_mod) # looks pretty good
car::Anova(knz_multitroph_dss_mod)
# Sum Sq Df F value    Pr(>F)    
# con_stability_log  1.60926  1 76.3394 2.798e-06 ***
#   prod_stability_log 0.03110  1  1.4755    0.2499    
# Residuals          0.23188 11    

# correlations
cor(knz_comb$prod_shannon, knz_comb$con_shannon) # 0.97
cor(knz_comb$prod_stability_log, knz_comb$con_stability_log) # -0.27

# basic plots
(prod_divstab_plot <- ggplot(data = knz_comb, aes(x = prod_shannon, y = prod_stability_log)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_classic()
  )

(prod_synchstab_plot <- ggplot(data = knz_comb, aes(x = prod_synchrony, y = prod_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(prod_constab_plot <- ggplot(data = knz_comb, aes(x = con_shannon, y = prod_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(prod_stab_panel <- ggpubr::ggarrange(prod_divstab_plot, prod_synchstab_plot, prod_constab_plot,
                                      nrow = 1, ncol = 3)
)

(con_divstab_plot <- ggplot(data = knz_comb, aes(x = con_shannon, y = con_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(con_synchstab_plot <- ggplot(data = knz_comb, aes(x = con_synchrony, y = con_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(con_prodstab_plot <- ggplot(data = knz_comb, aes(x = prod_shannon, y = con_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(con_stab_panel <- ggpubr::ggarrange(con_divstab_plot, con_synchstab_plot, con_prodstab_plot,
                                      nrow = 1, ncol = 3)
)

(mult_prodstab_plot <- ggplot(data = knz_comb, aes(x = prod_stability_log, y = multitroph_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(mult_constab_plot <- ggplot(data = knz_comb, aes(x = con_stability_log, y = multitroph_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(mult_stab_panel <- ggpubr::ggarrange(mult_prodstab_plot, mult_constab_plot,
                                     nrow = 1, ncol = 2)
)

(div_cor_plot <- ggplot(data = knz_comb, aes(x = prod_shannon, y = con_shannon)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(stab_cor_plot <- ggplot(data = knz_comb, aes(x = prod_stability_log, y = con_stability_log)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()
)

(cor_stab_panel <- ggpubr::ggarrange(div_cor_plot, stab_cor_plot,
                                      nrow = 1, ncol = 2)
)


#### correlations between aggregate and communitu stability ####

knz_prod_com_stab <- read.csv(here::here("data/KNZ", "producer_lengths.csv"))
knz_con_com_stab <- read.csv(here::here("data/KNZ", "consumer_lengths.csv"))

# specify relevant columns for stability
community_columns <- c("site", "Trajectory")


# subset and rename colummns for merge
knz_prod_com_stab <- knz_prod_com_stab[,which(names(knz_prod_com_stab) %in% community_columns)] %>%
  rename(plot = site,
         com_stab = Trajectory) 

knz_con_com_stab <- knz_con_com_stab[,which(names(knz_con_com_stab) %in% community_columns)] %>%
  rename(plot = site,
         com_stab = Trajectory)

# merge with aggregate stability metrics
knz_prod_agg_mv_dss <- merge(knz_prod_dss, knz_prod_com_stab, by = "plot")
knz_con_agg_mv_dss <- merge(knz_con_dss, knz_con_com_stab, by = "plot")

cor.test(knz_prod_agg_mv_dss$prod_stability, knz_prod_agg_mv_dss$com_stab, method = "kendall") # tau = 0.14, p = 0.51
cor.test(knz_con_agg_mv_dss$con_stability, knz_con_agg_mv_dss$com_stab, method = "kendall") # tau = -0.38, p = 0.062

# plot
(prod_agg_mv_plot <- ggplot(data = knz_prod_agg_mv_dss, aes(x = prod_stability, y = com_stab)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic()  +
    geom_text(aes(x = 7.7, y = 8, label = "tau = 0.14"), size = 8, hjust = 1)
)

(con_agg_mv_plot <- ggplot(data = knz_con_agg_mv_dss, aes(x = con_stability, y = com_stab)) +
    geom_point() +
    stat_smooth(method = "lm") +
    theme_classic() + 
    geom_text(aes(x = 1.2, y = 4, label = "tau = -0.38"), size = 8, hjust = 0)
)

(agg_mv_stab_panel <- ggpubr::ggarrange(prod_agg_mv_plot, con_agg_mv_plot,
                                     nrow = 1, ncol = 2)
)

##### structural equation modeling approach #####

# specify models for lavaan
mod_list <-
  '
# regregssions
prod_synchrony ~ prod_shannon
con_synchrony ~ con_shannon
prod_stability_log ~ prod_shannon + prod_synchrony + con_shannon
con_stability_log ~ con_shannon + con_synchrony + prod_shannon
multitroph_stability_log ~ con_stability_log + prod_stability_log 

# covariances
prod_shannon ~~ con_shannon
con_stability_log ~~ prod_stability_log
'

knz_fit <- lavaan::sem(mod_list, estimator = "ML", data = knz_comb)

summary(knz_fit)

knz_model_fit<-data.frame(model = lavInspect(knz_fit, "fit")[c("chisq","df", "pvalue", "rmsea","cfi","tli")])
knz_model_fit

knz_model_fit_df<-as.data.frame(knz_model_fit)
modindices(knz_fit)
varTable(knz_fit)

knz_r2 <- lavaan::inspect(knz_fit, "r2")
knz_r2_labels <- paste0("R² = ", round(knz_r2, 2)) # converting to character vector

sem_plot <- semPaths(
  knz_fit, 
  what = "col",        # create unweighted path
  whatLabels = "std",  # Show standardized coefficients on paths
  layout = "tree2",    # Tree layout
  curvePivot = TRUE,   # Curve pivot for covariances
  nCharNodes = 0,      # Full variable names
  sizeMan = 12,        # Size of manifest variables
  sizeLat = 12,        # Size of latent variables
  asize = 3,           # Arrowhead size
  residuals = FALSE,
  edge.label.cex = 1,  # Adjust size of edge labels
  edge.label.color = "black", # Ensure edge labels are visible
  as.expression = TRUE
)

# Extract path information
edge_info <- lavaan::parameterEstimates(knz_fit, standardized = TRUE)
edge_significance <- edge_info$pvalue
edge_values <- edge_info$std.all

# Extract standardized coefficients and significance
edge_info <- lavaan::parameterEstimates(knz_fit, standardized = TRUE)

# Define edge attributes based on significance and standardized coefficients
edge_colors <- ifelse(edge_info$std.all > 0, "blue", "red")  # Blue for positive, red for negative
edge_lty <- ifelse(edge_info$pvalue < 0.05, 1, 2)            # Solid for significant, dashed for non-significant

# Modify graph attributes
sem_plot$graphAttributes$Edges$color <- edge_colors  # Set edge colors
sem_plot$graphAttributes$Edges$lty <- edge_lty     # Set line types

# Replot with updated properties
plot(sem_plot)

## add R2 to main response variables - EXTRACTED THIS CODE FROM CHAT GPT BECAUSE IT'S CRAZY UNINTUITIVE IN SEMPATH
# Extract node positions
node_positions <- sem_plot$layout
node_labels <- sem_plot$graphAttributes$Nodes$labels

# Loop through variables and add R² labels
for (i in seq_along(knz_r2_labels)) {
  var_name <- names(knz_r2)[i]
  node_index <- which(node_labels == var_name)
  
  # Add R² annotation slightly below each node
  text(
    x = node_positions[node_index, 1],       # X-coordinate of the node
    y = node_positions[node_index, 2] - 0.1, # Slight offset below node
    labels = knz_r2_labels[i],                  # R² label
    cex = 0.8                               # Label size
  )
}


#### PSEM SCREW AROUND - IGNORE
psem_mod <- psem(knz_prod_dss_mod,  knz_con_dss_mod, knz_multitroph_dss_mod)
summary(psem_mod)


