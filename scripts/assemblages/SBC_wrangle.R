# title: "SBC"
# authors: "James Sturges"
# date-modified: 4 Feb 2025

library(raster) 
library(gdm)            
library(reshape2)         
library(lme4)             
library(MASS)             
library(AER)
library(vegan)              
library(ecotraj)
library(purrr)
library(RColorBrewer)
library(vegclust)
library(ecotraj)
library(smacof)
library(tidyverse)
library(lubridate)

#20 year electrofishing dataset within Everglades National Park
SBC_raw = read.csv("data/SBC/sbc_all.csv")

#any up stream removals
SBC_filtered = SBC_raw %>% 
  mutate(abundance = if_else(abundance < 0, 0, abundance))


SBC_filtered = SBC_filtered %>% 
  filter(id_confidence > 0)


SBC_producer = SBC_filtered %>% 
  filter(taxa_type %in% "producer")

SBC_primary_consumer = SBC_filtered %>% 
  filter(taxa_type %in% "primary_consumer")

SBC_predator = SBC_filtered %>% 
  filter(taxa_type %in% "predator")

unique(SBC_primary_consumer$taxon_name)

unique_taxa_counts <- SBC_filtered %>%
  group_by(taxa_type) %>%
  summarise(unique_taxa_count = n_distinct(taxon_name))

glimpse(SBC_filtered)








map20$Date <- mdy(map20$date)

map20 <- map20 %>%
  mutate(year = as.numeric(as.character(year)),
         drainage.site = paste0(drainage,site),
         common_spp_name = paste0(COMMON_NAME, sep = "_", FISHES_DECAPODS))
