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

###### KBS ######
kbs_prod_wide <- read.csv(here::here("data/KBS", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/KBS", "kbs_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = kbs_prod_wide, consumer_data = kbs_con_wide, "kbs")

###### CDR OLD FIELD ######
cdr_of_prod_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = cdr_of_prod_wide, consumer_data = cdr_of_con_wide, "cdr_of")

###### CDR BIODIVERSITY ######
cdr_biodiv_1_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_1_producers_wide_sub.csv"))
cdr_biodiv_1_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_1_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = cdr_biodiv_1_prod_wide, consumer_data = cdr_biodiv_1_con_wide, "cdr_biodiv_1")

cdr_biodiv_2_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_2_producers_wide_sub.csv"))
cdr_biodiv_2_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_2_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = cdr_biodiv_2_prod_wide, consumer_data = cdr_biodiv_2_con_wide, "cdr_biodiv_2")

cdr_biodiv_4_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_4_producers_wide_sub.csv"))
cdr_biodiv_4_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_4_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = cdr_biodiv_4_prod_wide, consumer_data = cdr_biodiv_4_con_wide, "cdr_biodiv_4")

cdr_biodiv_8_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_8_producers_wide_sub.csv"))
cdr_biodiv_8_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_8_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = cdr_biodiv_8_prod_wide, consumer_data = cdr_biodiv_8_con_wide, "cdr_biodiv_8")

cdr_biodiv_16_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_16_producers_wide_sub.csv"))
cdr_biodiv_16_con_wide <-  read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_16_consumers_wide_sub.csv"))
calculate_agg_stability(producer_data = cdr_biodiv_16_prod_wide, consumer_data = cdr_biodiv_16_con_wide, "cdr_biodiv_16")

###### COMBINE TERRESTRIAL AGGREGATE DFS ######
terrestrial_agg_stability <- rbind(
  knz_aggregate_dss,
  kbs_aggregate_dss,
  cdr_of_aggregate_dss,
  cdr_biodiv_1_aggregate_dss,
  cdr_biodiv_2_aggregate_dss,
  cdr_biodiv_4_aggregate_dss,
  cdr_biodiv_8_aggregate_dss,
  cdr_biodiv_16_aggregate_dss
)

# write.csv(row.names = F, terrestrial_agg_stability, here::here("data/synthesized_data", "terrestrial_agg_dss.csv"))
