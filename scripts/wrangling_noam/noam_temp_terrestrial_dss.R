rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)


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

terr_comb <- read.csv(here::here(file = "data/synthesized_data", "temp_terrestrial_agg_dss.csv"))

# create individual models for each site

#### Producer stability ####
# Initialize an empty dataframe to store results
prod_stability_results <- data.frame(
  site = character(),
  intercept = numeric(),
  prod_shannon_coef = numeric(),
  con_shannon_coef = numeric(),
  f_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site
for (s in unique(terr_comb$site)) {
  # Subset data for the current site
  site_data <- subset(terr_comb, site == s)
  
  # Fit linear model
  model <- lm(log(prod_stability + 1) ~ prod_shannon + con_shannon, data = site_data)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Get ANOVA results
  anova_results <- Anova(model) 
  f_statistic <- anova_results$`F value`[2]  # Take F-statistic for the model
  p_value <- anova_results$`Pr(>F)`[2]        # Take p-value
  
  # Store results
  prod_stability_results <- rbind(prod_stability_results, data.frame(
    site = s,
    intercept = coefs["(Intercept)"],
    prod_shannon_coef = coefs["prod_shannon"],
    con_shannon_coef = coefs["con_shannon"],
    f_statistic = f_statistic,
    p_value = p_value,
    stringsAsFactors = FALSE
  ))
}



# run as lmer with site as random effect
comb_prod_mod <- lmer(log(prod_stability + 1) ~ prod_shannon + con_shannon + (1|site), data = terr_comb)
summary(comb_prod_mod)
Anova(comb_prod_mod) # producer X2 = 15.7, P < 0.0001, consumer X2 = 0.14, P = 0.71
performance::r2(comb_prod_mod) # 0.12, 0.61

# basic plots
(prod_stab_prod_div_plot <- 
  ggplot() +
  geom_point(data = terr_comb, aes(x = prod_shannon, y = log(prod_stability + 1), colour = site)) +
  stat_smooth(data = terr_comb, aes(x = prod_shannon, y = log(prod_stability + 1), colour = site),
              method = "lm", se = F) +
  stat_smooth(data = terr_comb, aes(x = prod_shannon, y = log(prod_stability + 1)),
              method = "lm", se = T, colour = "black") +
  theme_classic() 
)

(prod_stab_con_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_shannon, y = log(prod_stability + 1), colour = site)) +
    stat_smooth(data = terr_comb, aes(x = con_shannon, y = log(prod_stability + 1), colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = con_shannon, y = log(prod_stability + 1)),
                method = "lm", se = T, colour = "black") +
    theme_classic() 
)

(prod_stab_panel <- ggpubr::ggarrange(prod_stab_prod_div_plot, prod_stab_con_div_plot,
                                     nrow = 1, ncol = 2)
)

# Initialize an empty dataframe to store results
prod_stability_results <- data.frame(
  site = character(),
  intercept = numeric(),
  prod_shannon_coef = numeric(),
  con_shannon_coef = numeric(),
  f_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site
for (s in unique(terr_comb$site)) {
  # Subset data for the current site
  site_data <- subset(terr_comb, site == s)
  
  # Fit linear model
  model <- lm(log(prod_stability + 1) ~ prod_shannon + con_shannon, data = site_data)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Get ANOVA results
  anova_results <- Anova(model) 
  f_statistic <- anova_results$`F value`[2]  # Take F-statistic for the model
  p_value <- anova_results$`Pr(>F)`[2]        # Take p-value
  
  # Store results
  prod_stability_results <- rbind(prod_stability_results, data.frame(
    site = s,
    intercept = coefs["(Intercept)"],
    prod_shannon_coef = coefs["prod_shannon"],
    con_shannon_coef = coefs["con_shannon"],
    f_statistic = f_statistic,
    p_value = p_value,
    stringsAsFactors = FALSE
  ))
}



# run as lmer with site as random effect
comb_prod_mod <- lmer(log(prod_stability + 1) ~ prod_shannon + con_shannon + (1|site), data = terr_comb)
summary(comb_prod_mod)
Anova(comb_prod_mod) # producer X2 = 15.7, P < 0.0001, consumer X2 = 0.14, P = 0.71
performance::r2(comb_prod_mod) # 0.12, 0.61

# basic plots
(prod_stab_prod_div_plot <- 
  ggplot() +
  geom_point(data = terr_comb, aes(x = prod_shannon, y = log(prod_stability + 1), colour = site)) +
  stat_smooth(data = terr_comb, aes(x = prod_shannon, y = log(prod_stability + 1), colour = site),
              method = "lm", se = F) +
  stat_smooth(data = terr_comb, aes(x = prod_shannon, y = log(prod_stability + 1)),
              method = "lm", se = T, colour = "black") +
  theme_classic() 
)

(prod_stab_con_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_shannon, y = log(prod_stability + 1), colour = site)) +
    stat_smooth(data = terr_comb, aes(x = con_shannon, y = log(prod_stability + 1), colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = con_shannon, y = log(prod_stability + 1)),
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
  prod_shannon_coef = numeric(),
  con_shannon_coef = numeric(),
  f_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each site
for (s in unique(terr_comb$site)) {
  # Subset data for the current site
  site_data <- subset(terr_comb, site == s)
  
  # Fit linear model
  model <- lm(log(con_stability + 1) ~ prod_shannon + con_shannon, data = site_data)
  
  # Extract coefficients
  coefs <- coef(model)
  
  # Get ANOVA results
  anova_results <- Anova(model) 
  f_statistic <- anova_results$`F value`[2]  # Take F-statistic for the model
  p_value <- anova_results$`Pr(>F)`[2]        # Take p-value
  
  # Store results
  con_stability_results <- rbind(con_stability_results, data.frame(
    site = s,
    intercept = coefs["(Intercept)"],
    prod_shannon_coef = coefs["prod_shannon"],
    con_shannon_coef = coefs["con_shannon"],
    f_statistic = f_statistic,
    p_value = p_value,
    stringsAsFactors = FALSE
  ))
}



# run as lmer with site as random effect
comb_con_mod <- lmer(log(con_stability + 1) ~ prod_shannon + con_shannon + (1|site), data = terr_comb)
summary(comb_con_mod)
Anova(comb_con_mod) # prod shannon X2 = 2.9, P = 0.08; con shannon X2 = 2.2, P = 0.13
performance::r2(comb_con_mod) # 0.13, 0.45

# basic plots
(con_stab_prod_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = prod_shannon, y = log(con_stability + 1), colour = site)) +
    stat_smooth(data = terr_comb, aes(x = prod_shannon, y = log(con_stability + 1), colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = prod_shannon, y = log(con_stability + 1)),
                method = "lm", se = T, colour = "black") +
    theme_classic() 
)

(con_stab_con_div_plot <- 
    ggplot() +
    geom_point(data = terr_comb, aes(x = con_shannon, y = log(con_stability + 1), colour = site)) +
    stat_smooth(data = terr_comb, aes(x = con_shannon, y = log(con_stability + 1), colour = site),
                method = "lm", se = F) +
    stat_smooth(data = terr_comb, aes(x = con_shannon, y = log(con_stability + 1)),
                method = "lm", se = T, colour = "black") +
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

cor.test(terr_comb$prod_shannon, terr_comb$con_shannon, method = "kendall") # tau = 0.29, P < 0.0001

# stability correlations
stability_correlations <- terr_comb %>%
  group_by(site) %>%
  summarise(
    kendall_tau = cor(prod_stability, con_stability, method = "kendall"),
    p_value = cor.test(prod_stability, con_stability, method = "kendall")$p.value,
    .groups = "drop"
  )

cor.test(terr_comb$prod_stability, terr_comb$con_stability, method = "kendall") # tau = 0.06, P = 0.16

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
