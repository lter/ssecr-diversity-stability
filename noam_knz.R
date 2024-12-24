# Noam Altman-Kurosaki
# Konza multitrophic analyses

librarian::shelf(vegan, lme4, car, dplyr, tidyr, lattice, lavaan, piecewiseSEM, semPlot, ggplot2, codyn)


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

# log transform/rescale variables for lavaan - NOTE: might be able to do this with glmms with piecewiseSEM in the future
knz_comb$prod_stability_log <- log(knz_comb$prod_stability, 2)
knz_comb$con_stability_log <- log(knz_comb$con_stability, 2)
knz_comb$multitroph_stability_log <- log(knz_comb$multitroph_stability, 2)

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
  what = "std",        # Show standardized estimates
  whatLabels = "std",  # Show standardized coefficients on paths
  edge.label.cex = 1,  # Adjust size of edge labels
  style = "lisrel",    # Classic LISREL-style diagram
  layout = "tree2",     # Tree layout
  curvePivot = TRUE,   # Curve pivot for covariances
  nCharNodes = 0,      # Full variable names
  sizeMan = 12,         # Size of manifest variables
  sizeLat = 12,        # Size of latent variables
  asize = 2,           # Arrowhead size
  residScale = 10,    # Scale residuals
  as.expression = TRUE
)



knz_model_fit_df_plot<-knz_model_fit_df %>% mutate(variables = row.names(.)) %>% filter(variables %in% c("chisq","df","pvalue","rmsea"))

param_summary <- standardizedSolution(knz_fit, type="std.all") # standardize solution
param_summary_sub<-param_summary %>% filter(!rhs == lhs, !lhs == rhs) # remove equivalent comparisons

# add variables to specify directionality
param_summary_sub$from <- param_summary_sub$rhs
param_summary_sub$to <- param_summary_sub$lhs

# extract R2
SEM_r2<-parameterEstimates(knz_fit,rsquare = TRUE) %>% filter(op == "r2")

# Rename variables
knz_model_fit_df_plot$model <- sprintf("%.2f", knz_model_fit_df_plot$model)
combined_annotation <- paste(knz_model_fit_df_plot$variables, "=", knz_model_fit_df_plot$model, collapse = "; \n")





# param_summary_sub <- param_summary_sub %>% 
#   mutate(from = fct_recode(from,
#                            "consumer synchrony" = "con_synchrony",
#                            "producer synchrony" = "prod_synchrony",
#                            "producer diversity" = "prod_shannon_log",
#                            "consumer diversity" = "con_shannon_log",
#                            "producer stability" = "prod_stability_log",
#                            "consumer stability" = "con_stability_log"),
#          to = fct_recode(to,
#                          "consumer synchrony" = "con_synchrony",
#                          "producer synchrony" = "prod_synchrony",
#                          "multitrophic stability" = "multitroph_stability_log",
#                          "producer stability" = "prod_stability_log",
#                          "consumer stability" = "con_stability_log",))
# 


# add significance codes
param_summary_sub<-param_summary_sub %>% mutate(direction = ifelse(est.std < 0, "-", "+"),
                                                sig = ifelse(pvalue < 0.05, "yes","no"))

SEM_table<-param_summary_sub[c("to","from","est.std","se","pvalue")]
# write.csv(SEM_table,"./Tables/SEM_trop.csv", row.names = F)

hs_graph <- as_tbl_graph(param_summary_sub, directed = T)

coord_positions <- data.frame(name = c("algal diversity","herbivore diversity",
                                       "herbivore synchrony","algal synchrony",
                                       "multitrophic stability"),
                              y = c(15,15,
                                    10,10,
                                    5),
                              x = c(2,3,
                                    3,2,
                                    2.5))

trop_coord_ordered<-data.frame(name = row.names(data.frame(hs_graph_trop[4])))
trop_coord<-merge(trop_coord_ordered,coord_positions, sort = F)

sem_path_tropical<-
  ggraph(hs_graph_trop,trop_coord) + 
  theme_bw() +
  removeGrid() +
  geom_edge_link(aes(edge_color = ifelse(!is.na(sig) & sig == "yes", est.std, NA), # changed from est
                     alpha = ifelse(!is.na(sig) & sig == "yes", 1,0.5),
                     label = ifelse(!is.na(sig) & sig == "yes", round(est.std,2), NA)),
                 arrow = arrow(length = unit(3, 'mm'),angle = 20,
                               ends = "last",
                               type = "open"),
                 start_cap = circle(12, 'mm'),
                 end_cap = square(25, 'mm'),
                 width = 3,
                 angle_calc = 'along',
                 label_colour = "black",
                 label_size = 2.6) +
  labs(edge_color = "Coef. estimate", linetype = "none", fill = "variable", title = "Tropical") +
  geom_node_point(aes(fill = name), fill = "black",size = 30, pch =21) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.6,0.07),
        legend.direction = "horizontal",
        legend.key.width=unit(0.8, 'cm'),
        legend.background = element_blank()) +
  guides(alpha = "none", fill = "none") +
  scale_edge_linetype(guide = "none") +
  scale_edge_alpha(guide = 'none') +
  scale_edge_colour_gradientn(colors = c(low = "blue",mid = "white",high = "red"), 
                              values = scales::rescale(c(-0.5, -0.15,0, 0.15,0.5)),
                              limits = c(-0.5, 0.5), oob = scales::squish) +
  geom_node_text(aes(label = stringr::str_wrap(name, 8)), color = "white") +
  lims(x = c(1.5,3.5),
       y = c(2,17))


