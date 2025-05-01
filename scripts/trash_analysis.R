#### Katherine trash script for playing around with glmms

# loading libraries
librarian::shelf(tidyverse, stringr, piecewiseSEM, glmmTMB, DHARMa, performance, 
                 ggeffects, vegan, lme4, car, MASS, dplyr, tidyr, 
                 semPlot, ggplot2, ggpubr, emmeans, lmerTest)

#### loading data #### 
setwd("~/Documents/SSECR/data")
terrestrial_agg_dss <- read.csv("terrestrial_agg_dss.csv") # reading in scripts manually for now -- downloaded from google drive


#### Assigining treatments ####
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


#### FROM NOAM's SCRIPT: z_standard() - function for z standardization ####
z_standard <- function(x){
  (x - mean(x))/sd(x)
}

## checking distributions of reponse variables
hist(terrestrial_agg_dss2$prod_stability)
hist(terrestrial_agg_dss2$prod_richness)
hist(z_standard(terrestrial_agg_dss2$con_stability))
hist(terrestrial_agg_dss2$con_stability)
hist(terrestrial_agg_dss2$con_richness)
hist(z_standard(terrestrial_agg_dss2$multitroph_stability))
hist((terrestrial_agg_dss2$multitroph_stability))


terrestrial_agg_dss2 %>%
  count(site, treatment)


#### lmer models: Similar to Noam's SEM models ####
# producer stability
m1 <- lmer(prod_stability ~ prod_richness + con_richness + (1|site/treatment),
           data = terrestrial_agg_dss2)
summary(m1)
plot(simulateResiduals(m1))
check_model(m1)


# consumer stability
m2 <- lmer(con_stability ~ prod_richness + con_richness + (1|site/treatment),
           data = terrestrial_agg_dss2)
summary(m2)
plot(simulateResiduals(m2))
check_model(m2)


# multitrophic stability
m3 <- lmer(multitroph_stability ~ prod_stability + con_stability  + (1|site/treatment),
           data = terrestrial_agg_dss2)
summary(m3)
plot(simulateResiduals(m3))
check_model(m3)



#### z-standardizing lmer models ####
m1b <- lmer(z_standard(prod_stability) ~ z_standard(prod_richness) + z_standard(con_richness) + (1|site/treatment),
            data = terrestrial_agg_dss2)
summary(m1b)
plot(simulateResiduals(m1b))

# consumer stability
m2b <- lmer(z_standard(con_stability) ~ z_standard(prod_richness) + z_standard(con_richness) + (1|site/treatment),
            data = terrestrial_agg_dss2)
summary(m2b)
plot(simulateResiduals(m2b))

# multitrophic stability
m3b <- lmer(z_standard(multitroph_stability) ~ z_standard(prod_stability) + z_standard(con_stability)  + (1|site/treatment),
            data = terrestrial_agg_dss2)
summary(m3b)
plot(simulateResiduals(m3b))



#### glmmTMB models: gaussian ####
# producer stability
m4 <- glmmTMB(prod_stability ~ prod_richness + con_richness + (1|site/treatment),
              data = terrestrial_agg_dss2,
              family = "gaussian")
summary(m4)
plot(simulateResiduals(m4))
check_model(m4)


# consumer stability
m5 <- glmmTMB(con_stability ~ prod_richness + con_richness + (1|site/treatment),
              data = terrestrial_agg_dss2,
              family = "gaussian")
summary(m5)
plot(simulateResiduals(m5))


# multitrophic stability
m6 <- glmmTMB(multitroph_stability ~ prod_stability + con_stability  + (1|site/treatment),
              data = terrestrial_agg_dss2,
              family = "gaussian")
summary(m6)
plot(simulateResiduals(m6))




#### glmmTMB models: Gamma ####
# producer stability
m7 <- glmmTMB(prod_stability ~ prod_richness + con_richness + (1|site/treatment),
              data = terrestrial_agg_dss2,
              family = Gamma(link = "log"))
summary(m7)
plot(simulateResiduals(m7))
check_model(m7)


# consumer stability
m8 <- glmmTMB(con_stability ~ prod_richness + con_richness + (1|site/treatment),
              data = terrestrial_agg_dss2,
              family = Gamma(link = "log"))
summary(m8)
plot(simulateResiduals(m8))
check_model(m8)

# multitrophic stability
m9 <- glmmTMB(multitroph_stability ~ prod_stability + con_stability + (1|site/treatment),
              data = terrestrial_agg_dss2,
              family = Gamma(link = "log"))
summary(m9)
plot(simulateResiduals(m9))
check_model(m9)


#### REMOVING KONZA #####
no_knz_aggregated <- terrestrial_agg_dss2 %>%
  filter(site != "knz")
m10 <- glmmTMB(prod_stability ~ prod_richness + con_richness + (1|site/treatment),
              data = no_knz_aggregated,
              family = Gamma(link = "log"))
summary(m10) # effect of producer richness is very sensitive to konza
plot(simulateResiduals(m10))
check_model(m10)


#### SEM ####
stability_sem <- psem(
  glmmTMB(prod_stability ~ prod_richness + con_richness + (1|site/treatment),
          data = terrestrial_agg_dss2,
          family = gaussian),
  glmmTMB(con_stability ~ prod_richness + con_richness + (1|site/treatment),
          data = terrestrial_agg_dss2,
          family = gaussian),
  glmmTMB(multitroph_stability ~ prod_stability + con_stability + (1|site/treatment),
          data = terrestrial_agg_dss2,
          family = gaussian),
  prod_richness %~~% con_richness,
  prod_stability %~~% con_stability
)
summary(stability_sem)




#### plotting ####
predictm7 <- ggpredict(m7, terms = "prod_richness", back_transform = TRUE)
predictm7 %>%
  ggplot() +
  geom_point(aes(prod_richness, prod_stability, color = site), data = terrestrial_agg_dss2) +
  geom_line(aes(x, predicted), linewidth = 2) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic() +
  scale_color_brewer(palette = "Set2")

predictm7 <- ggpredict(m7, terms = "con_richness", back_transform = TRUE)
predictm7 %>%
  ggplot() +
  geom_point(aes(con_richness, prod_stability, color = site), data = terrestrial_agg_dss2) +
  geom_line(aes(x, predicted), linewidth = 2) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic() +
  scale_color_brewer(palette = "Set2") 

predictm9 <- ggpredict(m9, terms = "prod_stability", back_transform = TRUE)
predictm9 %>%
  ggplot() +
  geom_point(aes(prod_stability, multitroph_stability, color = site), data = terrestrial_agg_dss2) +
  geom_line(aes(x, predicted), linewidth = 2) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.3) + 
  theme_classic() +
  scale_color_brewer(palette = "Set2")




