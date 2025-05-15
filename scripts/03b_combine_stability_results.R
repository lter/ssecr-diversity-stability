# Project Information ----
# SSECR: Diversity-Stability Relationships
# Co-Leads: James Sturges & Dr. Junna Wang
# Last Updated: May 13th, 2025

# Load Libraries ----
library(readr)
library(tidyverse)
library(here)

# Read length CSVs ----
knz_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "knz_producer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

knz_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "knz_consumer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

aims_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "aims_producer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

aims_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "aims_consumer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

kbs_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "kbs_producer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

kbs_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "kbs_consumer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

cdr_of_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "cdr_of_producer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

cdr_of_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "cdr_of_consumer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

usvi_fish_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "usvi_fish_producer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

usvi_fish_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "usvi_fish_consumer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

usvi_invert_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "usvi_invert_producer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

usvi_invert_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "usvi_invert_consumer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

sbc_fish_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "sbc_fish_producer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

sbc_fish_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "sbc_fish_consumer_lengths.csv")) %>%
  select(site, plot, mean_length, Trajectory)

sbc_invert_producer_lengths <- read_csv(here("tables/wide_output_minimize/producer", "sbc_invert_producer_lengths.csv"))
sbc_invert_consumer_lengths <- read_csv(here("tables/wide_output_minimize/consumer", "sbc_invert_consumer_lengths.csv"))