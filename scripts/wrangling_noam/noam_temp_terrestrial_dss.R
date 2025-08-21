rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans, DHARMa)



##### setup ####
source(here::here("scripts", "00_functions.r"))

###### read comb data #####
comb <- read.csv(here::here(file = "data/synthesized_data", "combined_agg_stability.csv"))

#### add mean-center transformation ####
comb <- comb %>%
  group_by(site) %>%
  mutate(
    n_habitats = n_distinct(habitat_fine)
  ) %>%
  ungroup() %>%
  group_by(site, habitat_fine) %>%
  mutate(
    prod_mean_cent = ifelse(
      n_habitats > 1,
      prod_richness - mean(prod_richness, na.rm = TRUE),
      prod_richness - mean(prod_richness, na.rm = TRUE) # will be replaced later
    ),
    con_mean_cent = ifelse(
      n_habitats > 1,
      con_richness - mean(con_richness, na.rm = TRUE),
      con_richness - mean(con_richness, na.rm = TRUE) # will be replaced later
    )
  ) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(
    prod_mean_cent = ifelse(
      n_habitats == 1,
      prod_richness - mean(prod_richness, na.rm = TRUE),
      prod_mean_cent
    ),
    con_mean_cent = ifelse(
      n_habitats == 1,
      con_richness - mean(con_richness, na.rm = TRUE),
      con_mean_cent
    )
  ) %>%
  ungroup() %>%
  select(-n_habitats)


##### z-transform predictors #####
comb <- comb %>%
  group_by(site) %>%
  mutate(
    prod_richness_z = as.numeric(z_standard(prod_richness)),
    con_richness_z = as.numeric(z_standard(con_richness)),
    prod_shannon_z = as.numeric(z_standard(prod_shannon)),
    con_shannon_z = as.numeric(z_standard(con_shannon))
  ) %>%
  ungroup()



##### separate terrestrial and aquatic #####
terr_comb <- subset(comb, ecosystem == "terrestrial")
aquatic_comb <- subset(comb, ecosystem == "aquatic")

#### test mean centered component models ###
terr_prod_stab_mod <- lmer(prod_stability  ~ prod_mean_cent + con_mean_cent + (1|site),  data = terr_comb)
summary(terr_prod_stab_mod)
# Estimate Std. Error t value
# (Intercept)     5.00923    1.42709   3.510
# prod_mean_cent -0.11826    0.03612  -3.274
# con_mean_cent   0.10230    0.24158   0.423
plot(DHARMa::simulateResiduals(terr_prod_stab_mod)) # significant quantile deviations
Anova(terr_prod_stab_mod) 
# prod_mean_cent 10.7223  1   0.001059 **
# con_mean_cent   0.1793  1   0.671956    
performance::r2(terr_prod_stab_mod) # 0.18, 0.735

terr_con_stab_mod <- lmer(con_stability  ~ prod_mean_cent + con_mean_cent + (1|site),  data = terr_comb)
summary(terr_con_stab_mod)
# Estimate Std. Error t value
# (Intercept)     1.704154   0.074429  22.896
# prod_mean_cent  0.018102   0.007891   2.294
# con_mean_cent  -0.079746   0.052782  -1.511
plot(DHARMa::simulateResiduals(terr_con_stab_mod)) # quantile deviations but not as bad
Anova(terr_con_stab_mod) 
# prod_mean_cent 5.2629  1    0.02178 *
# con_mean_cent  2.2827  1    0.13082    
performance::r2(terr_con_stab_mod) # 0.144, 0.169


aquatic_prod_stab_mod <- lmer(prod_stability  ~ prod_mean_cent + con_mean_cent + (1|site),  data = aquatic_comb)
summary(aquatic_prod_stab_mod)
# Estimate Std. Error t value
# (Intercept)     6.81148    2.66280   2.558
# prod_mean_cent  0.34455    0.22086   1.560
# con_mean_cent   0.01314    0.05526   0.238
plot(DHARMa::simulateResiduals(aquatic_prod_stab_mod)) # significant quantile deviations in simulated residuals, KS test sig
plot(residuals(aquatic_prod_stab_mod), predict(aquatic_prod_stab_mod)) # lots of clumping from one outlying site
Anova(aquatic_prod_stab_mod) 
# Chisq Df Pr(>Chisq)
# prod_mean_cent 2.4338  1     0.1187
# con_mean_cent  0.0566  1     0.8120
performance::r2(aquatic_prod_stab_mod) # 0.006, 0.563


aquatic_con_stab_mod <- lmer(con_stability  ~ prod_mean_cent + con_mean_cent + (1|site),  data = aquatic_comb)
summary(aquatic_con_stab_mod)
# Estimate Std. Error t value
# (Intercept)    1.766865   0.206738   8.546
# prod_mean_cent 0.024330   0.030300   0.803
# con_mean_cent  0.007780   0.007581   1.026
plot(DHARMa::simulateResiduals(aquatic_con_stab_mod)) # same issues as prod stability but not as bad
plot(residuals(aquatic_con_stab_mod), predict(aquatic_con_stab_mod)) # two clusters
Anova(aquatic_con_stab_mod) 
# prod_mean_cent 0.6448  1     0.4220
# con_mean_cent  1.0533  1     0.3047
performance::r2(aquatic_con_stab_mod) # 0.007, 0.276

(terr_prod_prod_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_mean_cent, y = prod_stability , colour = site)) +
    stat_smooth(data = terr_comb, aes(x =  prod_mean_cent, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x =  prod_mean_cent, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(terr_prod_con_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_mean_cent, y = prod_stability , colour = site)) +
    stat_smooth(data = terr_comb, aes(x =  con_mean_cent, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x =  con_mean_cent, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(terr_prod_rich_stab_panel <- ggpubr::ggarrange(terr_prod_prod_plot, terr_prod_con_plot,
                                                nrow = 1, ncol = 2)
)
# ggsave(plot = terr_prod_rich_stab_panel, filename =  here::here("figures/GLM", "terr_prod_stab_richness_mean_cent.png"), height = 7, width = 14)



(terr_con_prod_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_mean_cent, y = con_stability , colour = site)) +
    stat_smooth(data = terr_comb, aes(x =  prod_mean_cent, y = con_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x =  prod_mean_cent, y = con_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(terr_con_con_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_mean_cent, y = con_stability , colour = site)) +
    stat_smooth(data = terr_comb, aes(x =  con_mean_cent, y = con_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x =  con_mean_cent, y = con_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(terr_con_rich_stab_panel <- ggpubr::ggarrange(terr_con_prod_plot, terr_con_con_plot,
                                               nrow = 1, ncol = 2)
)
# ggsave(plot = terr_con_rich_stab_panel, filename =  here::here("figures/GLM", "terr_con_stab_richness_mean_cent.png"), height = 7, width = 14)





(aquatic_prod_prod_plot <- 
    ggplot() +
    geom_point(data = aquatic_comb, aes(x = prod_mean_cent, y = prod_stability , colour = site)) +
    stat_smooth(data = aquatic_comb, aes(x =  prod_mean_cent, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = aquatic_comb, aes(x =  prod_mean_cent, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(aquatic_prod_con_plot <- 
    ggplot() +
    geom_point(data = aquatic_comb, aes(x = con_mean_cent, y = prod_stability , colour = site)) +
    stat_smooth(data = aquatic_comb, aes(x =  con_mean_cent, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = aquatic_comb, aes(x =  con_mean_cent, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(aquatic_prod_rich_stab_panel <- ggpubr::ggarrange(aquatic_prod_prod_plot, aquatic_prod_con_plot,
                                                nrow = 1, ncol = 2)
)
# ggsave(plot = aquatic_prod_rich_stab_panel, filename =  here::here("figures/GLM", "aquatic_prod_stab_richness_mean_cent.png"), height = 7, width = 14)



(aquatic_con_prod_plot <- 
    ggplot() +
    geom_point(data = aquatic_comb, aes(x = prod_mean_cent, y = con_stability , colour = site)) +
    stat_smooth(data = aquatic_comb, aes(x =  prod_mean_cent, y = con_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = aquatic_comb, aes(x =  prod_mean_cent, y = con_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(aquatic_con_con_plot <- 
    ggplot() +
    geom_point(data = aquatic_comb, aes(x = con_mean_cent, y = con_stability , colour = site)) +
    stat_smooth(data = aquatic_comb, aes(x =  con_mean_cent, y = con_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = aquatic_comb, aes(x =  con_mean_cent, y = con_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(aquatic_con_rich_stab_panel <- ggpubr::ggarrange(aquatic_con_prod_plot, aquatic_con_con_plot,
                                               nrow = 1, ncol = 2)
)
# ggsave(plot = aquatic_con_rich_stab_panel, filename =  here::here("figures/GLM", "aquatic_con_stab_richness_mean_cent.png"), height = 7, width = 14)



###############################################################################################
### z-standardized component models ####
##### terrestrial richness #####
terr_prod_stab_mod <- lmer(prod_stability  ~ prod_richness_z + con_richness_z + (1|site),  data = terr_comb)
summary(terr_prod_stab_mod)
# Estimate Std. Error t value
# (Intercept)       5.0179     1.4278   3.515
# prod_richness_z  -0.8057     0.4479  -1.799
# con_richness_z   -0.3351     0.4479  -0.748
plot(DHARMa::simulateResiduals(terr_prod_stab_mod)) # all good
Anova(terr_prod_stab_mod) 
# prod_richness_z 0.07206 .
# con_richness_z  0.45437   
performance::r2(terr_prod_stab_mod) # 0.097, 0.643

producer_combined_model_formula <- as.formula(paste0(
  "prod_stability", " ~ ",
  "prod_richness_z", " + ",
  "con_richness_z", " + ",
  "(1 | site) + ",
  "(1 | site)"
))
lmer(producer_combined_model_formula, data = terr_comb)

terr_con_stab_mod <- lmer(con_stability  ~ prod_richness_z + con_richness_z + (1|site),  data = terr_comb)
summary(terr_con_stab_mod)
# Estimate Std. Error t value
# (Intercept)      1.70660    0.07397  23.072
# prod_richness_z  0.10184    0.09009   1.130
# con_richness_z  -0.04294    0.09009  -0.477
plot(DHARMa::simulateResiduals(terr_con_stab_mod)) # all good
Anova(terr_con_stab_mod) 
# prod_richness_z  0.2583 
# con_richness_z   0.6336  
performance::r2(terr_con_stab_mod) # 0.041, 0.054

##### terrestrial shannon #####
terr_prod_stab_mod <- lmer(prod_stability  ~ prod_shannon_z + con_shannon_z + (1|site),  data = terr_comb)
summary(terr_prod_stab_mod)
# Estimate Std. Error t value
# (Intercept)      5.0196     1.4279   3.515
# prod_shannon_z  -0.5518     0.5416  -1.019
# con_shannon_z   -0.4722     0.5416  -0.872
plot(DHARMa::simulateResiduals(terr_prod_stab_mod)) # some quantile deviations
plot(residuals(terr_prod_stab_mod), predict(terr_prod_stab_mod)) # possible negative trend
Anova(terr_prod_stab_mod) 
# Chisq Df Pr(>Chisq)
# prod_shannon_z    0.3082
# con_shannon_z     0.3832
performance::r2(terr_prod_stab_mod) # 0.081, 0.625


terr_con_stab_mod <- lmer(con_stability  ~ prod_shannon_z + con_shannon_z + (1|site),  data = terr_comb) 
summary(terr_con_stab_mod)
# Estimate Std. Error t value
# (Intercept)     1.70534    0.07420  22.983
# prod_shannon_z -0.03838    0.10274  -0.374
# con_shannon_z   0.15289    0.10274   1.488
plot(DHARMa::simulateResiduals(terr_con_stab_mod)) #  quantile deviations
plot(residuals(terr_con_stab_mod), predict(terr_con_stab_mod)) # not terrible
Anova(terr_con_stab_mod) 
# prod_shannon_z  0.7087  
# con_shannon_z   0.1367
performance::r2(terr_con_stab_mod) # 0.095, 0.114

##### aquatic richness #####
aquatic_prod_stab_mod <- lmer(prod_stability  ~ prod_richness_z + con_richness_z + (1|site),  data = aquatic_comb)
summary(aquatic_prod_stab_mod)
# Estimate Std. Error t value
# (Intercept)       6.8072     2.6614   2.558
# prod_richness_z   1.6782     0.4196   4.000
# con_richness_z   -0.2978     0.4196  -0.710
plot(DHARMa::simulateResiduals(aquatic_prod_stab_mod)) # significant quantile deviations in simulated residuals, KS test sig
plot(residuals(aquatic_prod_stab_mod), predict(aquatic_prod_stab_mod)) # lots of clumping from one outlying site
Anova(aquatic_prod_stab_mod) 

# prod_richness_z 6.347e-05 ***
# con_richness_z  0.4779
performance::r2(aquatic_prod_stab_mod) # 0.35, 0.592


aquatic_con_stab_mod <- lmer(con_stability  ~ prod_richness_z + con_richness_z + (1|site),  data = aquatic_comb)
summary(aquatic_con_stab_mod)
# Estimate Std. Error t value
# (Intercept)      1.76428    0.20681   8.531
# prod_richness_z  0.11134    0.05697   1.954
# con_richness_z   0.18344    0.05697   3.220
plot(DHARMa::simulateResiduals(aquatic_con_stab_mod)) # same issues as prod stability
plot(residuals(aquatic_con_stab_mod), predict(aquatic_con_stab_mod)) # possibly heteroskedastic
Anova(aquatic_con_stab_mod) 
# prod_richness_z  0.050649 .
# con_richness_z  0.001281 **
performance::r2(aquatic_con_stab_mod) # 0.068, 0.340

##### aquatic shannon #####
aquatic_prod_stab_mod <- lmer(prod_stability  ~ prod_shannon_z + con_shannon_z + (1|site),  data = aquatic_comb)
summary(aquatic_prod_stab_mod)
# Estimate Std. Error t value
# (Intercept)      6.8115     2.6628   2.558
# prod_shannon_z   0.1670     0.4151   0.402
# con_shannon_z    0.6435     0.4151   1.550
plot(DHARMa::simulateResiduals(aquatic_prod_stab_mod)) # similar issues
plot(residuals(aquatic_prod_stab_mod), predict(aquatic_prod_stab_mod)) # same as richness
Anova(aquatic_prod_stab_mod) 
# Chisq Df Pr(>Chisq)
# prod_shannon_z   0.6875
# con_shannon_z    0.1211
performance::r2(aquatic_prod_stab_mod) # 0.006, 0.562


aquatic_con_stab_mod <- lmer(con_stability  ~ prod_shannon_z + con_shannon_z + (1|site),  data = aquatic_comb)
summary(aquatic_con_stab_mod)
# (Intercept)     1.76423    0.20682   8.530
# prod_shannon_z  0.06723    0.05434   1.237
# con_shannon_z   0.23499    0.05434   4.324
plot(DHARMa::simulateResiduals(aquatic_con_stab_mod)) # still same issues 
plot(residuals(aquatic_con_stab_mod), predict(aquatic_con_stab_mod)) # same as above
Anova(aquatic_con_stab_mod) 
# prod_shannon_z  0.216  
# con_shannon_z   1.531e-05 ***
performance::r2(aquatic_con_stab_mod) # 0.069, 0.341

#### exploratory plots ####
##### terrestrial richness #####
(terr_prod_prod_plot <- 
  ggplot() +
  geom_point(data = terr_comb, aes(x = prod_richness_z, y = prod_stability , colour = site)) +
  stat_smooth(data = terr_comb, aes(x =  prod_richness_z, y = prod_stability, colour = site),
              method = "glm", se = F) +
  stat_smooth(data = terr_comb, aes(x =  prod_richness_z, y = prod_stability),
              method = "glm", se = T, colour = "black") +
  theme_classic() 
)

(terr_prod_con_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_richness_z, y = prod_stability , colour = site)) +
    stat_smooth(data = terr_comb, aes(x =  con_richness_z, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x =  con_richness_z, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(terr_prod_rich_stab_panel <- ggpubr::ggarrange(terr_prod_prod_plot, terr_prod_con_plot,
                                     nrow = 1, ncol = 2)
)
# ggsave(plot = terr_prod_rich_stab_panel, filename =  here::here("figures/GLM", "terr_prod_stab_richness_z.png"), height = 7, width = 14)

(terr_con_prod_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_richness_z, y = prod_stability , colour = site)) +
    stat_smooth(data = terr_comb, aes(x =  prod_richness_z, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x =  prod_richness_z, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(terr_con_con_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_richness_z, y = prod_stability , colour = site)) +
    stat_smooth(data = terr_comb, aes(x =  con_richness_z, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x =  con_richness_z, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(terr_con_rich_stab_panel <- ggpubr::ggarrange(terr_prod_prod_plot, terr_prod_con_plot,
                                                nrow = 1, ncol = 2)
)
# ggsave(plot = terr_prod_rich_stab_panel, filename =  here::here("figures/GLM", "terr_prod_stab_richness_z.png"), height = 7, width = 14)


##### aquatic richness #####
(aquatic_prod_prod_plot <- 
    ggplot() +
    geom_point(data = aquatic_comb, aes(x = prod_richness_z, y = prod_stability , colour = site)) +
    stat_smooth(data = aquatic_comb, aes(x =  prod_richness_z, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = aquatic_comb, aes(x =  prod_richness_z, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(aquatic_prod_con_plot <- 
    ggplot() +
    geom_point(data = aquatic_comb, aes(x = con_richness_z, y = prod_stability , colour = site)) +
    stat_smooth(data = aquatic_comb, aes(x =  con_richness_z, y = prod_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = aquatic_comb, aes(x =  con_richness_z, y = prod_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(aquatic_prod_rich_stab_panel <- ggpubr::ggarrange(aquatic_prod_prod_plot, aquatic_prod_con_plot,
                                                nrow = 1, ncol = 2)
)
# ggsave(plot = aquatic_prod_rich_stab_panel, filename =  here::here("figures/GLM", "aquatic_prod_stab_richness_z.png"), height = 7, width = 14)

#### Test function ####
model_stability(df = aquatic_comb,
                ecosystem_type = "aquatic",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")
