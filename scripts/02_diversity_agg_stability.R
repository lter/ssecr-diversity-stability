rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)
# retrieve functions from 00_functions.r
source(here::here("scripts", "00_functions.r"))

#### TERRESTRIAL DATA ####

##### Read in data and create agg stability dfs #####
# NOTE: PROBABLY A FASTER WAY TO DO THIS

###### KNZ ######
knz_prod_wide <- read.csv(here::here("data/KNZ", "knz_producers_wide_sub.csv"))
knz_con_wide <- read.csv(here::here("data/KNZ", "knz_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = knz_prod_wide, consumer_data = knz_con_wide, "knz")
knz_aggregate_dss$site <- rep("knz", nrow(knz_aggregate_dss))

###### KBS ######
kbs_prod_wide <- read.csv(here::here("data/KBS", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/KBS", "kbs_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = kbs_prod_wide, consumer_data = kbs_con_wide, "kbs")
kbs_aggregate_dss$site <- rep("kbs", nrow(kbs_aggregate_dss))

###### CDR OLD FIELD ######
cdr_of_prod_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = cdr_of_prod_wide, consumer_data = cdr_of_con_wide, "cdr_of")
cdr_of_aggregate_dss$site <- rep("cdr_of", nrow(cdr_of_aggregate_dss))

###### CDR BIODIVERSITY ######
# cdr_biodiv_1_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_1_producers_wide_sub.csv"))
# cdr_biodiv_1_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_1_consumers_wide_sub.csv"))
# calculate_agg_stability(producer_data = cdr_biodiv_1_prod_wide, consumer_data = cdr_biodiv_1_con_wide, "cdr_biodiv_1")
# 
# cdr_biodiv_2_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_2_producers_wide_sub.csv"))
# cdr_biodiv_2_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_2_consumers_wide_sub.csv"))
# calculate_agg_stability(producer_data = cdr_biodiv_2_prod_wide, consumer_data = cdr_biodiv_2_con_wide, "cdr_biodiv_2")
# 
# cdr_biodiv_4_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_4_producers_wide_sub.csv"))
# cdr_biodiv_4_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_4_consumers_wide_sub.csv"))
# calculate_agg_stability(producer_data = cdr_biodiv_4_prod_wide, consumer_data = cdr_biodiv_4_con_wide, "cdr_biodiv_4")
# 
# cdr_biodiv_8_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_8_producers_wide_sub.csv"))
# cdr_biodiv_8_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_8_consumers_wide_sub.csv"))
# calculate_agg_stability(producer_data = cdr_biodiv_8_prod_wide, consumer_data = cdr_biodiv_8_con_wide, "cdr_biodiv_8")
# 
# cdr_biodiv_16_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_16_producers_wide_sub.csv"))
# cdr_biodiv_16_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_16_consumers_wide_sub.csv"))
# calculate_agg_stability(producer_data = cdr_biodiv_16_prod_wide, consumer_data = cdr_biodiv_16_con_wide, "cdr_biodiv_16")

###### COMBINE TERRESTRIAL AGGREGATE DFS ######
terrestrial_agg_stability <- base::rbind(
  knz_aggregate_dss,
  kbs_aggregate_dss,
  cdr_of_aggregate_dss
)

# write.csv(row.names = F, terrestrial_agg_stability, here::here("data/synthesized_data", "terrestrial_agg_dss.csv"))

#### MARINE DATA ####
##### read in and create agg dss dataframes #####

###### GCE ###### - DROPPING FOR MAY PRESENTATION BECAUSE OF CTA ISSUE - 0 observations to prevalent
gce_prod_wide <- read.csv(here::here("data/GCE", "gce_producers_wide_sub.csv"))
gce_con_wide <- read.csv(here::here("data/GCE", "gce_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = gce_prod_wide, consumer_data = gce_con_wide, "gce")

###### USVI ######
usvi_prod_wide <- read.csv(here::here("data/USVI", "usvi_producers_wide_sub.csv"))
usvi_con_wide <- read.csv(here::here("data/USVI", "usvi_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = usvi_prod_wide, consumer_data = usvi_con_wide, "usvi")
usvi_aggregate_dss$site <- rep("usvi", nrow(usvi_aggregate_dss))

##### AIMS #####
aims_prod_wide <- read.csv(here::here("data/aims", "aims_producers_wide_sub.csv"))
aims_con_wide <- read.csv(here::here("data/aims", "aims_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = aims_prod_wide, consumer_data = aims_con_wide, "aims")


###### NEW SITE NAMES DIDN'T CARRY OVER FOR CONSUMER GROUPS FOR SOME REASON SO INPUTTING MANUALLY #####
###### SBC ######
sbc_invert_prod_wide <- read.csv(here::here("data/SBC", "sbc_invert_producers_wide_sub.csv"))
sbc_invert_wide <- read.csv(here::here("data/SBC", "sbc_invert_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = sbc_invert_prod_wide, consumer_data = sbc_invert_wide, "sbc_invert")
sbc_invert_aggregate_dss$site <- rep("sbc_invert", nrow(sbc_invert_aggregate_dss))

sbc_fish_prod_wide <- read.csv(here::here("data/SBC", "sbc_fish_producers_wide_sub.csv"))
sbc_fish_wide <- read.csv(here::here("data/SBC", "sbc_fish_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = sbc_fish_prod_wide, consumer_data = sbc_fish_wide, "sbc_fish")
sbc_fish_aggregate_dss$site <- rep("sbc_fish", nrow(sbc_fish_aggregate_dss))

###### MCR ######
mcr_invert_prod_wide <- read.csv(here::here("data/MCR", "mcr_invert_producers_wide_sub.csv"))
mcr_invert_wide <- read.csv(here::here("data/MCR", "mcr_invert_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = mcr_invert_prod_wide, consumer_data = mcr_invert_wide, "mcr_invert")
mcr_invert_aggregate_dss$site <- rep("mcr_invert", nrow(mcr_invert_aggregate_dss))

mcr_fish_prod_wide <- read.csv(here::here("data/MCR", "mcr_fish_producers_wide_sub.csv"))
mcr_fish_wide <- read.csv(here::here("data/MCR", "mcr_fish_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = mcr_fish_prod_wide, consumer_data = mcr_fish_wide, "mcr_fish")
mcr_fish_aggregate_dss$site <- rep("mcr_fish", nrow(mcr_fish_aggregate_dss))

###### COMBINE MARINE AGGREGATE DFS ######
marine_agg_stability <- rbind(
#  gce_aggregate_dss,
  usvi_aggregate_dss,
  aims_aggregate_dss,
  sbc_invert_aggregate_dss,
  sbc_fish_aggregate_dss,
  mcr_invert_aggregate_dss,
  mcr_fish_aggregate_dss
)

# write.csv(row.names = F, marine_agg_stability, here::here("data/synthesized_data", "marine_agg_dss.csv"))

##### COMBINE BOTH AGGREGATE DFS ####
combined_agg_stability <- rbind(terrestrial_agg_stability, marine_agg_stability)
# write.csv(row.names = F, combined_agg_stability, here::here("data/synthesized_data", "combined_agg_stability.csv"))



