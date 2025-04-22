rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)



# functions to extract ranges of data (from mcr algal working group github)
# custom functions by Noam - could probably be made more efficient
# function to extract the ranges of the data for plotting
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


# read temp terrestrial comb data

terr_comb <- read.csv(here::here(file = "data/synthesized_data", "terrestrial_agg_dss.csv"))

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
  model <- glm(prod_stability ~ prod_richness + con_richness, family = Gamma("log"), data = site_data)
  
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
comb_prod_mod <- glmer(prod_stability  ~ prod_richness + con_richness + (1|site), family = Gamma("log"), data = terr_comb)
summary(comb_prod_mod)
Anova(comb_prod_mod) # producer X2 = 0.5, P = 0.47, consumer X2 = 14.7, P = 0.0001
performance::r2(comb_prod_mod) # 0.06, 0.79

# basic plots
(prod_stab_prod_div_plot <- 
  ggplot() +
  geom_point(data = terr_comb, aes(x = prod_richness, y = prod_stability , colour = site)) +
  stat_smooth(data = terr_comb, aes(x = prod_richness, y = prod_stability, colour = site),
              method = "glm", se = F) +
  stat_smooth(data = terr_comb, aes(x = prod_richness, y = prod_stability),
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
  model <- glm(con_stability ~ prod_richness + con_richness, family = Gamma("log"), data = site_data)
  
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
comb_con_mod <- glmer(con_stability  ~ prod_richness + con_richness + (1|site), family = Gamma("log"), data = terr_comb)
summary(comb_con_mod)
Anova(comb_con_mod) # prod richness X2 = 3,1, P = 0.08; con richness X2 = 003, P = 0.96
performance::r2(comb_con_mod) # 0.05, 0.53

# basic plots
(con_stab_prod_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_richness, y = log(con_stability + 1), colour = site)) +
    stat_smooth(data = terr_comb, aes(x = prod_richness, y = log(con_stability + 1), colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = prod_richness, y = log(con_stability + 1)),
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
  model <- glm(multitroph_stability ~ prod_stability + con_stability, family = Gamma("log"), data = site_data)
  
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
comb_multitroph_mod <- glmer(multitroph_stability ~ prod_stability + con_stability + (1|site), family = Gamma("log"), data = terr_comb)
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
summary(modelList)
