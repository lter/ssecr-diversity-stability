rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)



# retrieve functions from 00_functions.r
source(here::here("scripts", "00_functions.r"))

#### MASTER ####
master <- read.csv(here::here(file = "data/synthesized_data", "master_stability.csv"))
model_stability(df = master,
                ecosystem_type = "combined",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")
# view figures
basic_sem_plot_combined_richness_aggregate
plot_combined_aggregate_prod_richness_stability
plot_combined_aggregate_con_richness_stability
# plot_combined_aggregate_multi_richness_stability
plot_combined_aggregate_stability_richness_correlations

write.csv(row.names = F, lmer_results_combined_aggregate_richness,
          here::here("data/summary_tables", "lmer_results_combined_aggregate_richness.csv"))

##### TERRESTRIAL ####
terr <- subset(master, ecosystem == "terrestrial")

# test function
model_stability(df = terr,
                ecosystem_type = "terrestrial",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")
# view figures
basic_sem_plot_terrestrial_richness_aggregate
plot_terrestrial_aggregate_prod_richness_stability
plot_terrestrial_aggregate_con_richness_stability
# plot_terrestrial_aggregate_multi_richness_stability
plot_terrestrial_aggregate_stability_richness_correlations

# view within-site coefficients and p-values (i.e. what results would be if modeled independently)
# lm(producer_stability ~ producer_richness + consumer_richness)
# View(site_results_terrestrial_prod_aggregate_richness)
write.csv(row.names = F, site_results_terrestrial_prod_aggregate_richness,
          here::here("data/summary_tables", "site_results_terrestrial_prod_aggregate_richness.csv"))

# consumer_stability ~ producer_richness + consumer_richness
# View(site_results_terrestrial_con_aggregate_richness)
write.csv(row.names = F, site_results_terrestrial_con_aggregate_richness,
          here::here("data/summary_tables", "site_results_terrestrial_con_aggregate_richness.csv"))

# multitrophic_stability ~ producer_stability + consumer_stability
# View(site_results_terrestrial_multi_aggregate_richness)
# write.csv(row.names = F, site_results_terrestrial_multi_aggregate_richness,
#           here::here("data/summary_tables", "site_results_terrestrial_multi_aggregate_richness.csv"))

# view combined results (i.e. site as a random effect)
# for producer and consumer stability, producer and consumer richness are the predictor variables
# for multitrophic stability, producer and consumer stability are the predictor variables
# View(lmer_results_terrestrial_aggregate_richness)
write.csv(row.names = F, lmer_results_terrestrial_aggregate_richness,
          here::here("data/summary_tables", "lmer_results_terrestrial_aggregate_richness.csv"))

# view sem results
sem_results_terrestrial_richness_aggregate

#### aquatic ####
# read temp terrestrial comb data
mar <- subset(master, ecosystem == "aquatic")

# test function
model_stability(df = mar,
                ecosystem_type = "aquatic",
                stability_metric = "aggregate",
                diversity_metric = "richness",
                prod_diversity_col = "prod_richness",
                con_diversity_col = "con_richness",
                prod_stability_col = "prod_stability",
                con_stability_col = "con_stability",
                multi_stability_col = NULL,
                transformation = "z")
# view figures
basic_sem_plot_aquatic_richness_aggregate
plot_aquatic_aggregate_prod_richness_stability
plot_aquatic_aggregate_con_richness_stability
# plot_aquatic_aggregate_multi_richness_stability
plot_aquatic_aggregate_stability_richness_correlations

# view within-site coefficients and p-values (i.e. what results would be if modeled independently)
# lm(producer_stability ~ producer_richness + consumer_richness)
# View(site_results_aquatic_prod_aggregate_richness)
write.csv(row.names = F, site_results_aquatic_prod_aggregate_richness,
          here::here("data/summary_tables", "site_results_aquatic_prod_aggregate_richness.csv"))

# consumer_stability ~ producer_richness + consumer_richness
# View(site_results_aquatic_con_aggregate_richness)
write.csv(row.names = F, site_results_aquatic_con_aggregate_richness,
          here::here("data/summary_tables", "site_results_aquatic_con_aggregate_richness.csv"))

# multitrophic_stability ~ producer_stability + consumer_stability
# View(site_results_aquatic_multi_aggregate_richness)
# write.csv(row.names = F, site_results_aquatic_multi_aggregate_richness,
#           here::here("data/summary_tables", "site_results_aquatic_multi_aggregate_richness.csv"))

# view combined results (i.e. site as a random effect)
# for producer and consumer stability, producer and consumer richness are the predictor variables
# for multitrophic stability, producer and consumer stability are the predictor variables
# View(lmer_results_aquatic_aggregate_richness)
write.csv(row.names = F, lmer_results_aquatic_aggregate_richness,
          here::here("data/summary_tables", "lmer_results_aquatic_aggregate_richness.csv"))

# view sem results
sem_results_aquatic_richness_aggregate
