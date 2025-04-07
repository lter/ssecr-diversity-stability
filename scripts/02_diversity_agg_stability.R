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
calculate_agg_stability(producer_data = knz_prod_wide, consumer_data = knz_con_wide, "Terrestrial")

###### KBS ######
kbs_prod_wide <- read.csv(here::here("data/KBS", "kbs_producers_wide_sub.csv"))
kbs_con_wide <- read.csv(here::here("data/KBS", "kbs_consumers_wide_sub.csv"))

###### CDR OLD FIELD ######
cdr_of_prod_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_producers_wide_sub.csv"))
cdr_of_con_wide <- read.csv(here::here("data/CDR_oldfield", "cdr_of_consumers_wide_sub.csv"))

###### CDR BIODIVERSITY ######
cdr_biodiv_prod_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_producers_wide_sub.csv"))
cdr_biodiv_con_wide <- read.csv(here::here("data/CDR_biodiv", "cdr_biodiv_consumers_wide_sub.csv"))