rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)



# retrieve functions from 00_functions.r
source(here::here("scripts", "00_functions.r"))

# read temp terrestrial comb data

terr_comb <- read.csv(here::here(file = "data/synthesized_data", "terrestrial_agg_dss.csv"))
# test function
model_stability(df = terr_comb,
                ecosystem_type = "terrestrial",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = "multitroph_stability",
                z_standard = FALSE)
# terrestrial_richness_aggregate_sem_results
### SCRATCH PAPER BELOW
# create individual models for each site

#### Producer stability ####
# Initialize an empty dataframe to store results
prod_stability_results <- data.frame(
  site = character(),
  intercept = numeric(),
  prod_richness_coef = numeric(),
  con_richness_coef = numeric(),
  x2 = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site
for (s in unique(terr_comb$site)) {
  # Subset data for the current site
  site_data <- subset(terr_comb, site == s)
  
  # Fit linear model
  model <- lm(prod_stability ~ prod_richness + con_richness, , data = site_data)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Get ANOVA results
  anova_results <- Anova(model) 
  f_statistic <- anova_results$`LR Chisq`[2]  # Take F-statistic for the model
  p_value <- anova_results$`Pr(>Chisq)`[2]        # Take p-value
  
  # Store results
  prod_stability_results <- rbind(prod_stability_results, data.frame(
    site = s,
    intercept = unname(coefs["(Intercept)"]),
    prod_shannon_coef = unname(coefs["prod_richness"]),
    con_shannon_coef = unname(coefs["con_richness"]),
    f_statistic = f_statistic,
    p_value = p_value,
    stringsAsFactors = FALSE
  ))
}



# run as lmer with site as random effect
comb_prod_mod <- lmer(prod_stability  ~ z_standard(prod_richness) + z_standard(con_richness) + (1|site),  data = terr_comb)
summary(comb_prod_mod)
Anova(comb_prod_mod) # producer X2 = 0.5, P = 0.47, consumer X2 = 14.7, P = 0.0001
x <- Anova(comb_prod_mod)
x$Chisq
x$`Pr(>Chisq)`
y <- performance::r2(comb_prod_mod) # 0.06, 0.79
unname(y$R2_conditional)
y$R2_marginal

# basic plots
(prod_stab_prod_div_plot <- 
  ggplot() +
  geom_point(data = terr_comb, aes(x = z_standard(prod_richness), y = prod_stability , colour = site)) +
  stat_smooth(data = terr_comb, aes(x = z_standard(prod_richness), y = prod_stability, colour = site),
              method = "glm", se = F) +
  stat_smooth(data = terr_comb, aes(x = z_standard(prod_richness), y = prod_stability),
              method = "glm", se = T, colour = "black") +
  theme_classic() 
)

(prod_stab_con_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_richness, y = log(prod_stability + 1), colour = site)) +
    stat_smooth(data = terr_comb, aes(x = con_richness, y = log(prod_stability + 1), colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = con_richness, y = log(prod_stability + 1)),
                method = "lm", se = T, colour = "black") +
    theme_classic() 
)

(prod_stab_panel <- ggpubr::ggarrange(prod_stab_prod_div_plot, prod_stab_con_div_plot,
                                     nrow = 1, ncol = 2)
)


#### CONSUMER STABILITY ####
# repeat the above but for consumer stability
con_stability_results <- data.frame(
  site = character(),
  intercept = numeric(),
  prod_richness_coef = numeric(),
  con_richness_coef = numeric(),
  x2 = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site
for (s in unique(terr_comb$site)) {
  # Subset data for the current site
  site_data <- subset(terr_comb, site == s)
  
  # Fit linear model
  model <- lm(con_stability ~ prod_richness + con_richness, , data = site_data)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Get ANOVA results
  anova_results <- Anova(model) 
  f_statistic <- anova_results$`LR Chisq`[2]  # Take F-statistic for the model
  p_value <- anova_results$`Pr(>Chisq)`[2]        # Take p-value
  
  # Store results
  con_stability_results <- rbind(con_stability_results, data.frame(
    site = s,
    intercept = unname(coefs["(Intercept)"]),
    prod_shannon_coef = unname(coefs["prod_richness"]),
    con_shannon_coef = unname(coefs["con_richness"]),
    f_statistic = f_statistic,
    p_value = p_value,
    stringsAsFactors = FALSE
  ))
}



# run as lmer with site as random effect
comb_con_mod <- lmer(con_stability  ~ z_standard(prod_richness) + z_standard(con_richness) + (1|site), , data = terr_comb)
summary(comb_con_mod)
Anova(comb_con_mod) # prod richness X2 = 3,1, P = 0.08; con richness X2 = 003, P = 0.96
performance::r2(comb_con_mod) # 0.05, 0.53

# basic plots
(con_stab_prod_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = z_standard(prod_richness), y = con_stability, colour = site)) +
    stat_smooth(data = terr_comb, aes(x = z_standard(prod_richness), y = con_stability, colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = z_standard(prod_richness), y = con_stability),
                method = "lm", se = T, colour = "black") +
    theme_classic() 
)

(con_stab_con_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_richness, y = con_stability, colour = site)) +
    stat_smooth(data = terr_comb, aes(x = con_richness, y = con_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x = con_richness, y = con_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(con_stab_panel <- ggpubr::ggarrange(con_stab_prod_div_plot, con_stab_con_div_plot,
                                      nrow = 1, ncol = 2)
)



#### MULTITROPHIC STABILITY ####
# repeat the above but for consumer stability
multitrophic_stability_results <- data.frame(
  site = character(),
  intercept = numeric(),
  prod_stability_coef = numeric(),
  con_stability_coef = numeric(),
  x2 = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site
for (s in unique(terr_comb$site)) {
  # Subset data for the current site
  site_data <- subset(terr_comb, site == s)
  
  # Fit linear model
  model <- lm(multitroph_stability ~ prod_stability + con_stability, , data = site_data)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Get ANOVA results
  anova_results <- Anova(model) 
  f_statistic <- anova_results$`LR Chisq`[2]  # Take F-statistic for the model
  p_value <- anova_results$`Pr(>Chisq)`[2]        # Take p-value
  
  # Store results
  multitrophic_stability_results <- rbind(multitrophic_stability_results, data.frame(
    site = s,
    intercept = unname(coefs["(Intercept)"]),
    prod_stability_coef = unname(coefs["prod_richness"]),
    con_stability_coef = unname(coefs["con_richness"]),
    f_statistic = f_statistic,
    p_value = p_value,
    stringsAsFactors = FALSE
  ))
}



# run as lmer with site as random effect
comb_multitroph_mod <- lmer(multitroph_stability ~ prod_stability + con_stability + (1|site), , data = terr_comb)
summary(comb_multitroph_mod)
Anova(comb_multitroph_mod)
performance::r2(comb_multitroph_mod) 

# basic plots
(multitroph_prod_stab_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_stability, y = multitroph_stability, colour = site)) +
    stat_smooth(data = terr_comb, aes(x = prod_stability, y = multitroph_stability, colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = prod_stability, y = multitroph_stability),
                method = "lm", se = T, colour = "black") +
    theme_classic() 
)

(multitroph_con_stab_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_stability, y = multitroph_stability, colour = site)) +
    stat_smooth(data = terr_comb, aes(x = con_stability, y = multitroph_stability, colour = site),
                method = "glm", se = F) +
    stat_smooth(data = terr_comb, aes(x = con_stability, y = multitroph_stability),
                method = "glm", se = T, colour = "black") +
    theme_classic() 
)

(con_stab_panel <- ggpubr::ggarrange(con_stab_prod_div_plot, con_stab_con_div_plot,
                                     nrow = 1, ncol = 2)
)



#### Correlations ####
# diversity correlations
diversity_correlations <- terr_comb %>%
  group_by(site) %>%
  summarise(
    kendall_tau = cor(prod_shannon, con_shannon, method = "kendall"),
    p_value = cor.test(prod_shannon, con_shannon, method = "kendall")$p.value,
    .groups = "drop"
  )

cor.test(terr_comb$prod_shannon, terr_comb$con_shannon, method = "kendall") # tau = 0.06, P = 0.22

# stability correlations
stability_correlations <- terr_comb %>%
  group_by(site) %>%
  summarise(
    kendall_tau = cor(prod_stability, con_stability, method = "kendall"),
    p_value = cor.test(prod_stability, con_stability, method = "kendall")$p.value,
    .groups = "drop"
  )

cor.test(terr_comb$prod_stability, terr_comb$con_stability, method = "kendall") # tau = 0.08, P = 0.09

# basic plots
(con_prod_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_shannon, y = con_shannon, colour = site)) +
    stat_smooth(data = terr_comb, aes(x = prod_shannon, y = con_shannon, colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = prod_shannon, y = con_shannon),
                method = "lm", se = T, colour = "black") +
    theme_classic() 
)


(con_prod_stab_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_stability, y = con_stability, colour = site)) +
    stat_smooth(data = terr_comb, aes(x = prod_stability, y = con_stability, colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = prod_stability, y = con_stability),
                method = "lm", se = T, colour = "black") +
    theme_classic() 
)

(terr_corr_panel <- ggpubr::ggarrange(con_prod_div_plot, con_prod_stab_plot,
                                      nrow = 1, ncol = 2)
)

modelList <- psem(comb_prod_mod, comb_con_mod, comb_multitroph_mod,
                  prod_stability %~~% con_stability,
                  prod_richness %~~% con_richness,
                  terr_comb)
z <- summary(modelList)
z
plot(modelList)
