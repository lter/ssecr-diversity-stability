# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: May 21st, 2025

library(tidyverse)
library(dplyr)
library(ggplot2)

master = read.csv("data/synthesized_data/master_stability.csv")

##### Producer Comp - Stability ----

# Create separate data frames for each ecosystem
df_terrestrial <- master %>% filter(ecosystem == "terrestrial")
df_aquatic     <- master %>% filter(ecosystem == "aquatic")

# Theme customization for bold and larger axis titles/ticks
custom_theme <- theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    axis.title = element_text(face = "bold", size = 16),
    axis.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  )

# Plot for the terrestrial ecosystem (producers)
plot_terrestrial <- ggplot(df_terrestrial, aes(x = prod_richness, y = prod_comp_stability,
                                               color = site, shape = site)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, fill = "gray", alpha = 0.3,
              color = "black", linewidth = 1, aes(group = site)) +
  labs(
    title = "Terrestrial Ecosystems",
    x = "Producer Richness",
    y = "Producer Compositional Stability",
    color = "LTER Site",
    shape = "LTER Site"
  ) +
  custom_theme

# Plot for the aquatic ecosystem (producers)
plot_aquatic <- ggplot(df_aquatic, aes(x = prod_richness, y = prod_comp_stability,
                                       color = site, shape = site)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, fill = "gray", alpha = 0.3,
              color = "black", linewidth = 1, aes(group = site)) +
  labs(
    title = "Aquatic Ecosystems",
    x = "Producer Richness",
    y = "Producer Compositional Stability",
    color = "LTER Site",
    shape = "LTER Site"
  ) +
  custom_theme

# Save producer plots
ggsave(filename = "figures/May_present/producer_richness_comp_stability_terrestrial.png",
       plot = plot_terrestrial, width = 8, height = 6, units = "in", dpi = 600)

ggsave(filename = "figures/May_present/producer_richness_comp_stability_aquatic.png",
       plot = plot_aquatic, width = 8, height = 6, units = "in", dpi = 600)

##### Consumer Comp - Stability ----

# Plot for the terrestrial ecosystem (consumers)
plot_terrestrial_consumer <- ggplot(df_terrestrial, aes(x = con_richness, y = con_comp_stability,
                                                        color = site, shape = site)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, fill = "gray", alpha = 0.3,
              color = "black", linewidth = 1, aes(group = site)) +
  labs(
    title = "Terrestrial Ecosystems",
    x = "Consumer Richness",
    y = "Consumer Compositional Stability",
    color = "LTER Site",
    shape = "LTER Site"
  ) +
  custom_theme

# Plot for the aquatic ecosystem (consumers)
plot_aquatic_consumer <- ggplot(df_aquatic, aes(x = con_richness, y = con_comp_stability,
                                                color = site, shape = site)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_smooth(method = "lm", se = TRUE, fill = "gray", alpha = 0.3,
              color = "black", linewidth = 1, aes(group = site)) +
  labs(
    title = "Aquatic Ecosystems",
    x = "Consumer Richness",
    y = "Consumer Compositional Stability",
    color = "LTER Site",
    shape = "LTER Site"
  ) +
  custom_theme

# Save consumer plots
ggsave(filename = "figures/May_present/consumer_richness_comp_stability_terrestrial.png",
       plot = plot_terrestrial_consumer, width = 8, height = 6, units = "in", dpi = 600)

ggsave(filename = "figures/May_present/consumer_richness_comp_stability_aquatic.png",
       plot = plot_aquatic_consumer, width = 8, height = 6, units = "in", dpi = 600)

# Optionally print plots
plot_terrestrial
plot_aquatic
plot_terrestrial_consumer
plot_aquatic_consumer