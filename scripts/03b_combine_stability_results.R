# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: May 13th, 2025

rm(list = ls())
librarian::shelf(googledrive, vegan, lme4, car, dplyr, tidyr, lattice, lavaan,
                 piecewiseSEM, semPlot, ggplot2, codyn, ggpubr, performance, emmeans)

# read combined dss
combined_agg_stability <- read.csv(here::here("data/synthesized_data", "combined_agg_stability.csv"))

# Read length CSVs ----
#### KNZ ####
knz_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "knz_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)
  

knz_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "knz_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)


#### AIMS ####
aims_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "aims_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)

aims_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "aims_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)

#### KBS ####
kbs_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "kbs_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)

kbs_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "kbs_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)

#### CDR ####
cdr_of_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "cdr_of_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)

cdr_of_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "cdr_of_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)

#### USVI ####
usvi_fish_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "usvi_fish_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)

usvi_fish_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "usvi_fish_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)

# usvi_invert_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "usvi_invert_producer_lengths.csv")) %>%
#   mutate(prod_comp_stability = mean_length,
#          prod_total_length = Trajectory) %>%
#   select(site, plot, prod_comp_stability, prod_total_length)
# 
# usvi_invert_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "usvi_invert_consumer_lengths.csv")) %>%
#   mutate(con_comp_stability = mean_length,
#          con_total_length = Trajectory) %>%
#   select(site, plot, con_comp_stability, con_total_length)


#### SBC ####
sbc_fish_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "sbc_fish_producer_lengths.csv")) %>%
   mutate(prod_comp_stability = mean_length,
          prod_total_length = Trajectory) %>%
   select(site, plot, prod_comp_stability, prod_total_length)
sbc_fish_producer_lengths$site <- rep("sbc_fish", nrow(sbc_fish_producer_lengths))

sbc_fish_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "sbc_fish_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)
sbc_fish_consumer_lengths$site <- rep("sbc_fish", nrow(sbc_fish_consumer_lengths))

sbc_invert_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "sbc_invert_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)
sbc_invert_producer_lengths$site <- rep("sbc_invert", nrow(sbc_invert_producer_lengths))

sbc_invert_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "sbc_invert_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)
sbc_invert_consumer_lengths$site <- rep("sbc_invert", nrow(sbc_invert_consumer_lengths))

#### MCR ####
mcr_fish_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "mcr_fish_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)
mcr_fish_producer_lengths$site <- rep("mcr_fish", nrow(mcr_fish_producer_lengths))

mcr_fish_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "mcr_fish_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)
mcr_fish_consumer_lengths$site <- rep("mcr_fish", nrow(mcr_fish_consumer_lengths))

mcr_invert_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "mcr_invert_producer_lengths.csv")) %>%
  mutate(prod_comp_stability = mean_length,
         prod_total_length = Trajectory) %>%
  select(site, plot, prod_comp_stability, prod_total_length)
mcr_invert_producer_lengths$site <- rep("mcr_invert", nrow(mcr_invert_producer_lengths))

mcr_invert_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "mcr_invert_consumer_lengths.csv")) %>%
  mutate(con_comp_stability = mean_length,
         con_total_length = Trajectory) %>%
  select(site, plot, con_comp_stability, con_total_length)
mcr_invert_consumer_lengths$site <- rep("mcr_invert", nrow(mcr_invert_consumer_lengths))

#### MERGE WITH COMB STABILITY ####
