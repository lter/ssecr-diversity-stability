rm(list = ls())

library(ggplot2)
source(here::here("scripts", "00_functions.r"))
# read data
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"), type='csv')
tmp <- tempfile(fileext = ".csv")
download <- drive_download(drive_folder[drive_folder$name=="master_stability.csv",], path = tmp, overwrite = TRUE)
master <- read.csv(tmp)

# z-standardize
stability_cols <- c("prod_stability", "prod_comp_stability", "con_stability", "con_comp_stability", "prod_jaccard_stability", "con_jaccard_stability")
master_z <- master %>%
  group_by(site) %>%
  mutate(across(all_of(stability_cols), ~ as.numeric(z_standard(.x)))) %>%
  ungroup()


cor.test(master_z$prod_comp_stability,
         master_z$prod_stability, method = "kendall") # p-value = 4.112e-06, tau = 0.208

(prod_stability_metric_corr <-
  ggplot() +
  geom_point(data = master, aes(x = prod_comp_stability, y = prod_stability, colour = site, shape = ecosystem), size = 2.5) +
  geom_smooth(data = master, aes(x = prod_comp_stability, y = prod_stability, colour = site), 
              method = lm, se = F) +
  geom_smooth(data = master, aes(x = prod_comp_stability, y = prod_stability ), 
                method = lm, se = T, colour = "black") +
  labs(x = "Compositional stability", y = "Aggregate stability", title = "a. Producer stability \n") +
  theme_classic() 
    
)


cor.test(master_z$con_comp_stability,
         master_z$con_stability, method = "kendall") # p-value = 1.031e-11, tau = 0.307

(con_stability_metric_corr <-
    ggplot() +
    geom_point(data = master_z, aes(x = con_comp_stability, y = con_stability, colour = site, shape = ecosystem), size = 2.5) +
    geom_smooth(data = master_z, aes(x = con_comp_stability, y = con_stability, colour = site), 
                method = lm, se = F) +
    geom_smooth(data = master_z, aes(x = con_comp_stability, y = con_stability ), 
                method = lm, se = T, colour = "black") +
    labs(x = "Compositional stability", y = "Aggregate stability", title = "b. Consumer stability \n") +
    theme_classic() 
  
)

# Comparing stability metric relationships

# Helper function to make consistent scatterplots
make_stability_plot <- function(data, xvar, yvar, xlab, ylab, title_label) {
  ggplot(data, aes_string(x = xvar, y = yvar, colour = "site", shape = "ecosystem")) +
    geom_point(size = 2.5) +
    geom_smooth(aes(group = site), method = "lm", se = FALSE) +
    geom_smooth(method = "lm", se = TRUE, colour = "black") +
    labs(x = xlab, y = ylab, title = title_label) +
    theme_classic()
}

# pproducer agg vs bray
plot_prod_stability_agg_bray <- ggplot(master_z, aes(x = prod_comp_stability, y = prod_stability, colour = site, shape = ecosystem)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = site), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "Compositional stability (Bray)",
    y = "Aggregate stability",
    title = "a. Producers: Aggregate vs. Compositional (Bray)"
  ) +
  theme_classic()

plot_prod_stability_agg_jac <- ggplot(master_z, aes(x = prod_jaccard_stability, y = prod_stability, colour = site, shape = ecosystem)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = site), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "Compositional stability (Jaccard)",
    y = "Aggregate stability",
    title = "b. Producers: Aggregate vs. Compositional (Jaccard)"
  ) +
  theme_classic()

plot_prod_stability_bray_jac <- ggplot(master_z, aes(x = prod_jaccard_stability, y = prod_comp_stability, colour = site, shape = ecosystem)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = site), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "Compositional stability (Jaccard)",
    y = "Compositional stability (Bray)",
    title = "c. Producers: Bray vs. Jaccard"
  ) +
  theme_classic()


plot_prod_stability_agg_bray
plot_prod_stability_agg_jac
plot_prod_stability_bray_jac


plot_con_stability_agg_bray <- ggplot(master_z, aes(x = con_comp_stability, y = con_stability, colour = site, shape = ecosystem)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = site), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "Compositional stability (Bray)",
    y = "Aggregate stability",
    title = "d. Consumers: Aggregate vs. Compositional (Bray)"
  ) +
  theme_classic()

plot_con_stability_agg_jac <- ggplot(master_z, aes(x = con_jaccard_stability, y = con_stability, colour = site, shape = ecosystem)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = site), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "Compositional stability (Jaccard)",
    y = "Aggregate stability",
    title = "e. Consumers: Aggregate vs. Compositional (Jaccard)"
  ) +
  theme_classic()

plot_con_stability_bray_jac <- ggplot(master_z, aes(x = con_jaccard_stability, y = con_comp_stability, colour = site, shape = ecosystem)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = site), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "Compositional stability (Jaccard)",
    y = "Compositional stability (Bray)",
    title = "f. Consumers: Bray vs. Jaccard"
  ) +
  theme_classic()

plot_con_stability_agg_bray
plot_con_stability_agg_jac
plot_con_stability_bray_jac

glimpse(master_z)

install.packages("ggridges")
library(ggridges)
# Prepare data for plotting
plot_data <- master_z %>%
  select(site, ecosystem,
         prod_stability, prod_comp_stability, prod_jaccard_stability) %>%
  pivot_longer(cols = c(prod_stability, prod_comp_stability, prod_jaccard_stability),
               names_to = "metric", values_to = "z_score")

# Create color palettes for sites grouped by ecosystem
aquatic_sites <- unique(plot_data$site[plot_data$ecosystem == "aquatic"])
terrestrial_sites <- unique(plot_data$site[plot_data$ecosystem == "terrestrial"])

aquatic_palette <- setNames(colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(length(aquatic_sites)),
                            aquatic_sites)
terrestrial_palette <- setNames(colorRampPalette(RColorBrewer::brewer.pal(9, "Greens"))(length(terrestrial_sites)),
                                terrestrial_sites)

site_colors <- c(aquatic_palette, terrestrial_palette)

# Plot ridgeline densities of z-scores, colored by site, faceted by metric
plot_z_dist <- ggplot(plot_data, aes(x = z_score, y = site, fill = site)) +
  geom_density_ridges(scale = 2, alpha = 0.7, color = "black", size = 0.2) +
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = site_colors) +
  theme_ridges(font_size = 10) +
  theme(legend.position = "none") +
  labs(title = "Distribution of Z-normalized Stability Metrics by Site",
       subtitle = "Sites colored by ecosystem shade (Blue = aquatic, Green = terrestrial)",
       x = "Z-score", y = "Site")


plot_z_dist2 <- ggplot(plot_data, aes(x = z_score, y = metric, fill = site)) +
  geom_density_ridges(scale = 2, alpha = 0.7, color = "black", size = 0.2) +
  
  # add raw data points with jitter on y so they don’t overlap on ridgeline baseline
  geom_point(aes(color = site), 
             position = position_jitter(height = 0.1), 
             alpha = 0.7, size = 1.5, shape = 21, stroke = 0.2) +
  
  # Facet so rows = metrics, columns = sites
  facet_grid(metric ~ site, scales = "free_x", space = "free") +
  
  scale_fill_manual(values = site_colors) +
  scale_color_manual(values = site_colors) +
  
  theme_ridges(font_size = 9) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(angle = 0)) +
  
  labs(title = "Z-normalized Stability Metrics Distribution by Site",
       subtitle = "Density ridges + raw points; Sites colored by ecosystem shade",
       x = "Z-score", y = "Metric")

plot_z_dist2



# all z scores for each type of stability metric
plot_z_violin <- ggplot(plot_data, aes(x = metric, y = z_score, fill = metric)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +
  theme_minimal() +
  labs(title = "Comparison of Stability Metrics (Z-normalized)",
       x = "Stability Metric",
       y = "Z-score")
