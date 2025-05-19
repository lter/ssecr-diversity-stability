#### Katherine trash script for playing around with glmms

# loading libraries
librarian::shelf(tidyverse, stringr, piecewiseSEM, glmmTMB, DHARMa, performance, 
                 ggeffects, vegan, lme4, car, MASS, dplyr, tidyr, 
                 semPlot, ggplot2, ggpubr, emmeans, lmerTest)

source('scripts/00_functions_minimize.R')

#### loading data #### 
setwd("~/Documents/SSECR/data")
agg_dss <- read.csv("combined_agg_stability.csv")

aquatic_agg_dss <- agg_dss %>%
  filter(ecosystem == "aquatic")
terrestrial_agg_dss <- agg_dss %>%
  filter(ecosystem == "terrestrial")

z_standard <- function(x){
  (x - mean(x))/sd(x)
}
# adding function to easily apply and rename to columns I want
z_standard_columns <- function(df) {
  df_z_score <- df %>%
    mutate(z_prod_rich = z_standard(prod_richness)) %>%
    mutate(z_prod_stability = z_standard(prod_stability)) %>%
    mutate(z_con_rich = z_standard(con_richness)) %>%
    mutate(z_con_stability = z_standard(con_stability)) %>%
    mutate(z_multitroph_richness = z_standard(multitroph_richness)) %>%
    mutate(z_multitroph_stability = z_standard(multitroph_stability)) %>%
    return(df_z_score)
}

### z standardize within each site 
aquatic_agg_dss_z <- aquatic_agg_dss %>%
  group_by(site) %>%
  group_split() %>%
  lapply(z_standard_columns) %>%
  bind_rows()

terr_agg_dss_z <- terrestrial_agg_dss %>%
  group_by(site) %>%
  group_split() %>%
  lapply(z_standard_columns) %>%
  bind_rows()


##### AQUATIC ####
### AQUATIC PRODUCER
aquatic_prod_mod <- lmer(data = aquatic_agg_dss_z, z_prod_stability ~ z_prod_rich + z_con_rich + (1 + z_prod_rich|site) + (1 + z_con_rich|site))

summary(aquatic_prod_mod)
plot(simulateResiduals(aquatic_prod_mod))

producer_predictmod <- ggpredict(aquatic_prod_mod, terms = "z_prod_rich", back_transform = TRUE, type = "random")
p1 <- producer_predictmod %>%
  ggplot() +
  geom_point(aes(z_prod_rich, z_prod_stability, color = site), data = aquatic_agg_dss_z, size = 3, alpha = 0.7) +
  geom_smooth(aes(z_prod_rich, z_prod_stability, color = site), data = aquatic_agg_dss_z, method = "lm", se = FALSE, linewidth = 1.5) +
  geom_line(aes(x, predicted), linewidth = 2.5) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("AIMS", "MCR Fish", "MCR Invertebrates", "SBC Fish", "SBC Invertebrates", "USVI")) +
  xlab("Producer Richness (z-standardized)") +
  ylab("Producer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "right")
p1

consumer_predictmod <- ggpredict(aquatic_prod_mod, terms = "z_con_rich", back_transform = TRUE, type = "random")
p2 <- consumer_predictmod %>%
  ggplot() +
  geom_point(aes(z_con_rich, z_prod_stability, color = site), data = aquatic_agg_dss_z, size = 3) +
  geom_smooth(aes(z_con_rich, z_prod_stability, color = site), data = aquatic_agg_dss_z, method = "lm", se = FALSE, linewidth = 1.5) +
  geom_line(aes(x, predicted), linewidth = 3) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("AIMS", "MCR Fish", "MCR Invertebrates", "SBC Fish", "SBC Invertebrates", "USVI")) +
  xlab("Consumer Richness (z-standardized)") +
  ylab("Producer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "right")
p2

pdf("producer_stability.pdf", width = 10, height = 12)
cowplot::plot_grid(p1, p2, nrow = 2, ncol = 1)
dev.off()


### AQUATIC CONSUMER
aquatic_con_mod <- lmer(data = aquatic_agg_dss_z, z_con_stability ~ z_prod_rich + z_con_rich + (1 + z_prod_rich|site) + (1 + z_con_rich|site))
summary(aquatic_con_mod)
plot(simulateResiduals(aquatic_con_mod))

predictaquatic_con_mod <- ggpredict(aquatic_con_mod, terms = "z_con_rich", back_transform = TRUE, type = "random")
predictaquatic_con_mod %>%
  ggplot() +
  geom_point(aes(z_con_rich, z_con_stability, color = site), data = aquatic_agg_dss_z, size = 3) +
  geom_line(aes(x, predicted), linewidth = 3) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("AIMS", "MCR Fish", "MCR Invertebrates", "SBC Fish", "SBC Invertebrates", "USVI")) +
  xlab("Consumer Richness (z-standardized)") +
  ylab("Consumer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "right")


#### aquatic Multitrophic
#aquatic_multi_mod <- lmer(data = aquatic_agg_dss_z, z_multitroph_stability ~ 0 + z_prod_stability + z_con_stability + (z_prod_stability + 0|site) + (z_con_stability + 0|site))
#aquatic_multi_mod <- glmmTMB(z_multitroph_stability ~ 0 + z_prod_stability + z_con_stability + (z_prod_stability + 0|site) + (z_con_stability + 0|site),
#                           data = aquatic_agg_dss_z)

#summary(aquatic_multi_mod)
#plot(simulateResiduals(aquatic_multi_mod))



########### TERRESTRIAL #############
## terrestrial Producer
terr_prod_mod <- lmer(data = terr_agg_dss_z, z_prod_stability ~ z_prod_rich + z_con_rich + (1 + z_prod_rich|site) + (1 + z_con_rich|site))
terr_prod_mod <- lm(data = terr_agg_dss_z, z_prod_stability ~ z_prod_rich + z_con_rich)

summary(terr_prod_mod)
plot(simulateResiduals(terr_prod_mod))
producer_predictmod <- ggpredict(terr_prod_mod, terms = "z_prod_rich", back_transform = TRUE, type = "random")
p3 <- producer_predictmod %>%
  ggplot() +
  geom_point(aes(z_prod_rich, z_prod_stability, color = site), data = terr_agg_dss_z, size = 3, alpha = 0.7) +
  geom_smooth(aes(z_prod_rich, z_prod_stability, color = site), data = terr_agg_dss_z, method = "lm", se = FALSE, linewidth = 1.5) +
  geom_line(aes(x, predicted), linewidth = 2.5) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR", "KBS", "KNZ")) +
  xlab("Producer Richness (z-standardized)") +
  ylab("Producer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "right")
p3
consumer_predictmod <- ggpredict(terr_prod_mod, terms = "z_con_rich", back_transform = TRUE, type = "random")
p4 <- producer_predictmod %>%
  ggplot() +
  geom_point(aes(z_con_rich, z_prod_stability, color = site), data = terr_agg_dss_z, size = 3, alpha = 0.7) +
  geom_smooth(aes(z_con_rich, z_prod_stability, color = site), data = terr_agg_dss_z, method = "lm", se = FALSE, linewidth = 1.5) +
  geom_line(aes(x, predicted), linewidth = 2.5) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR", "KBS", "KNZ")) +
  xlab("Consumer Richness (z-standardized)") +
  ylab("Producer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "right")
p4




###### TERRESTRIAL CONSUMER
terr_con_mod <- lmer(data = terr_agg_dss_z, z_con_stability ~ z_prod_rich + z_con_rich + (1 + z_prod_rich|site) + (1 + z_con_rich|site))
terr_con_mod <- lm(data = terr_agg_dss_z, z_con_stability ~ z_prod_rich + z_con_rich)

summary(terr_con_mod)
plot(simulateResiduals(terr_con_mod))
producer_predictmod <- ggpredict(terr_con_mod, terms = "z_prod_rich", back_transform = TRUE, type = "random")
p5 <- producer_predictmod %>%
  ggplot() +
  geom_point(aes(z_prod_rich, z_con_stability, color = site), data = terr_agg_dss_z, size = 3, alpha = 0.7) +
  geom_smooth(aes(z_prod_rich, z_con_stability, color = site), data = terr_agg_dss_z, method = "lm", se = FALSE, linewidth = 1.5) +
  geom_line(aes(x, predicted), linewidth = 2.5) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR", "KBS", "KNZ")) +
  xlab("Producer Richness (z-standardized)") +
  ylab("Consumer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "right")
p5

consumer_predictmod <- ggpredict(terr_con_mod, terms = "z_con_rich", back_transform = TRUE, type = "random")
p6 <- consumer_predictmod %>%
  ggplot() +
  geom_point(aes(z_con_rich, z_con_stability, color = site), data = terr_agg_dss_z, size = 3, alpha = 0.7) +
  geom_smooth(aes(z_con_rich, z_con_stability, color = site), data = terr_agg_dss_z, method = "lm", se = FALSE, linewidth = 1.5) +
  geom_line(aes(x, predicted), linewidth = 2.5) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR", "KBS", "KNZ")) +
  xlab("Consumer Richness (z-standardized)") +
  ylab("Consumer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "right")
p6


pdf("terrestrial_stability.pdf", width = 13, height = 12)
cowplot::plot_grid(p3, p5, p4, p6, nrow = 2, ncol = 2)
dev.off()












##########################################
#### OLD Assigining treatments ####
## CDR biodiversity experiment
cdr_biodiv_prod <-  read.csv("cdr_producer.csv") # again, reading in manually for now
cdr_biodiv_prod <- subset(cdr_biodiv_prod, id_confidence == 1)
cdr_treatment <- cdr_biodiv_prod %>%
  count(site, plot, treatment_seeding) %>%
  dplyr::select(!n) %>%
  rename(treatment = treatment_seeding) %>%
  mutate(site = "cdr_biodiv")

## Konza grazing treatments
knz_treatment <- tibble(
  site = c("knz", "knz", "knz", "knz", "knz", "knz", "knz", "knz", "knz", "knz", "knz", "knz", "knz", "knz"),
  plot = c("001d", "002c", "002d", "004b", "004f", "020b", "0spb", 
           "0sub", "n01a", "n01b", "n04a", "n04d", "n20a", "n20b"),
  treatment = c("ungrazed", "ungrazed", "ungrazed", "ungrazed", "ungrazed", "ungrazed", "ungrazed",
                "ungrazed", "grazed", "grazed", "grazed", "grazed", "grazed", "grazed")
)

## CDR old field
cdr_oldfield_treatment <- read.csv("cdr2_producer.csv")
cdr_oldfield_treatment <- cdr_oldfield_treatment %>%
  count(site, plot) %>%
  mutate(treatment = "cdr_none") %>%
  dplyr::select(!n) %>%
  mutate(site = "cdr_of")

## KBS 
kbs_treatment <- tibble(
  site = c("kbs", "kbs", "kbs", "kbs", "kbs", "kbs"),
  plot = c("1", "2", "3", "4", "5", "6"),
  treatment = c("kbs_none", "kbs_none", "kbs_none", "kbs_none", "kbs_none", "kbs_none")
)

## adding all together
treatment_info <- rbind(
  kbs_treatment,
  knz_treatment, 
  cdr_oldfield_treatment, 
  cdr_treatment
)


### adding to aggregated data
terrestrial_agg_dss2 <- terrestrial_agg_dss %>%
  mutate(site = if_else(str_detect(site, "cdr_biodiv"), "cdr_biodiv", site)) %>% ### making all the CDR biodiversity experiment plots one site again
  left_join(treatment_info, by = c("site", "plot")) # joining treatment data

marine_agg_dss2 <- marine_agg_dss %>% # doing it for marine also
  mutate(treatment = if_else(is.na(habitat_fine), "none", habitat_fine))


#### FROM NOAM's SCRIPT: z_standard() - function for z standardization ####
z_standard <- function(x){
  (x - mean(x))/sd(x)
}
# adding function to easily apply and rename to columns I want
z_standard_columns <- function(df) {
  df_z_score <- df %>%
    mutate(z_prod_rich = z_standard(prod_richness)) %>%
    mutate(z_prod_stability = z_standard(prod_stability)) %>%
    mutate(z_con_rich = z_standard(con_richness)) %>%
    mutate(z_con_stability = z_standard(con_stability)) %>%
    mutate(z_multitroph_richness = z_standard(multitroph_richness)) %>%
    mutate(z_multitroph_stability = z_standard(multitroph_stability)) %>%
  return(df_z_score)
}

### z standardize within each site 
terrestrial_agg_dss3 <- terrestrial_agg_dss2 %>%
  group_by(site) %>%
  group_split() %>%
  lapply(z_standard_columns) %>%
  bind_rows()

marine_agg_dss3 <- marine_agg_dss2 %>%
  group_by(site) %>%
  group_split() %>%
  lapply(z_standard_columns) %>%
  bind_rows()

total_agg_dss3 <- rbind( # combining terrestrial and aquatic -- not doing anything with this yet
  terrestrial_agg_dss3,
  marine_agg_dss3
)

### histograms
hist(terrestrial_agg_dss3$z_multitroph_stability)
hist(terrestrial_agg_dss3$z_prod_stability)
hist(terrestrial_agg_dss3$z_con_stability)
hist(terrestrial_agg_dss3$z_prod_rich)
hist(terrestrial_agg_dss3$z_con_rich)

#### glmmTMB models: gaussian, z transformed ####
# producer stability
m4 <- glmmTMB(z_prod_stability ~ z_prod_rich + z_con_rich + (1|site/treatment),
              data = terrestrial_agg_dss3,
              family = "gaussian")
summary(m4)
plot(simulateResiduals(m4))
check_model(m4)


# consumer stability
m5 <- glmmTMB(z_con_stability ~ z_prod_rich + z_con_rich + (1|site/treatment),
              data = terrestrial_agg_dss3,
              family = "gaussian")
summary(m5)
plot(simulateResiduals(m5))


# multitrophic stability
m6 <- glmmTMB(z_multitroph_stability ~ z_prod_stability + z_con_stability + (1|site/treatment),
              data = terrestrial_agg_dss3,
              family = t_family()) # student t distribution is the best fit, but not supported by piecewiseSEM
summary(m6)
plot(simulateResiduals(m6))
check_model(m6)


#### SEM ####
stability_sem <- psem(
  glmmTMB(z_prod_stability ~ z_prod_rich + z_con_rich + (1|site/treatment),
          data = terrestrial_agg_dss3,
          family = "gaussian"),
  glmmTMB(z_con_stability ~ z_prod_rich + z_con_rich + (1|site/treatment),
          data = terrestrial_agg_dss3,
          family = "gaussian"),
  glmmTMB(z_multitroph_stability ~ z_prod_stability + z_con_stability + (1|site/treatment),
          data = terrestrial_agg_dss3,
          family = "gaussian"), # using gaussian for now -- not best fit, probably don't want to use 
  z_prod_rich %~~% z_con_rich,
  z_prod_stability %~~% z_con_stability
)
summary(stability_sem)
plot(stability_sem) # plotting values



#### plotting ####

### producer stability ~ producer richness
# using model predictions
predictm4 <- ggpredict(m4, terms = c("z_prod_rich"), back_transform = TRUE, type = "random")
predictm4 %>%
  ggplot() +
  geom_point(aes(z_prod_rich, z_prod_stability, color = site), data = terrestrial_agg_dss3, size = 3) +
  geom_line(aes(x, predicted), linewidth = 2.5, linetype = 2) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR Biodiversity Experiment", "CDR Old Field", "KBS", "KNZ")) +
  xlab("Producer Richness (z-standardized)") +
  ylab("Producer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  ylim(-3.7, 6) +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.85),
        legend.box.background = element_rect(color="black", linewidth=1),
        legend.text = element_text(size = 16))


### producer stability ~ consumer richness
# using model predictions
predictm4 <- ggpredict(m4, terms = "z_con_rich", back_transform = TRUE, type = "random")
predictm4 %>%
  ggplot() +
  geom_point(aes(z_con_rich, z_prod_stability, color = site), data = terrestrial_agg_dss3, size = 3) +
  geom_line(aes(x, predicted), linewidth = 3) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR Biodiversity Experiment", "CDR Old Field", "KBS", "KNZ")) +
  xlab("Consumer Richness (z-standardized)") +
  ylab("Producer Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  ylim(-3.7, 6) +
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.85),
        legend.box.background = element_rect(color="black", linewidth=1),
        legend.text = element_text(size = 16))


### multitrophic stability ~ producer stability
# using model predictions
predictm6 <- ggpredict(m6, terms = "z_prod_stability", back_transform = TRUE, type = "random")
predictm6 %>%
  ggplot() +
  geom_point(aes(z_prod_stability, z_multitroph_stability, color = site), data = terrestrial_agg_dss3, size = 3) +
  geom_line(aes(x, predicted), linewidth = 3) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR Biodiversity Experiment", "CDR Old Field", "KBS", "KNZ")) +
  xlab("Producer Stability (z-standardized)") +
  ylab("Multitrophic Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "inside", legend.position.inside = c(0.25, 0.85),
        legend.box.background = element_rect(color="black", linewidth=1),
        legend.text = element_text(size = 16))



### multitrophic stability ~ consumer stability
# using model predictions
predictm6 <- ggpredict(m6, terms = "z_con_stability", back_transform = TRUE, type = "random")
predictm6 %>%
  ggplot() +
  geom_point(aes(z_con_stability, z_multitroph_stability, color = site), data = terrestrial_agg_dss3, size = 3) +
  geom_line(aes(x, predicted), linewidth = 3) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic(base_size = 20) +
  scale_color_brewer(palette = "Set2", labels = c("CDR Biodiversity Experiment", "CDR Old Field", "KBS", "KNZ")) +
  xlab("Consumer Stability (z-standardized)") +
  ylab("Multitrophic Stability (z-standardized)") +
  guides(color=guide_legend(title="Site")) +
  theme(legend.position = "inside", legend.position.inside = c(0.25, 0.85),
        legend.box.background = element_rect(color="black", linewidth=1),
        legend.text = element_text(size = 16))



