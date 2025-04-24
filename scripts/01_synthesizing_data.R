rm(list = ls())
librarian::shelf(googledrive, dplyr, tidyr, summarytools, here, codyn)

# retrieve functions from 00_functions.r
source(here::here("scripts", "00_functions.r"))

#### TERRESTRIAL DATA ####

##### Read in data and filter data #####

###### KNZ ######
knz_prod <-  read.csv(here::here("data/KNZ", "knz_producer.csv"))
knz_con <- read.csv(here::here("data/KNZ", "knz_consumer.csv"))

# remove not confident IDs
knz_prod <- subset(knz_prod, id_confidence == 1)
knz_con <- subset(knz_con, id_confidence == 1)

# consumers are read in as integers due to ID codes. convert all spp to factors
knz_con$taxon_name <- as.factor(knz_con$taxon_name)
knz_prod$taxon_name <- as.factor(knz_prod$taxon_name)

filter_data(site_name = "knz", producer_data = knz_prod, consumer_data = knz_con, mean_sum = "mean", output_folder = "data/KNZ", write_csv = TRUE)

###### KBS ######
# read data
kbs_prod <-  read.csv(here::here("data/KBS", "kbs_producer.csv"))
kbs_con <- read.csv(here::here("data/KBS", "kbs_consumer.csv"))

# subset confident ids - I am not subsetting kbs_con for specific trophic level at this point Mar 31 2025
kbs_prod <- subset(kbs_prod, id_confidence == 1)
kbs_con <- subset(kbs_con, id_confidence == 1)

filter_data(site_name = "kbs", producer_data = kbs_prod, consumer_data = kbs_con, mean_sum = "sum", output_folder = "data/KBS", write_csv = TRUE)

###### CDR OLD FIELD ######
# read data and subset confident IDs
cdr_of_prod <-  read.csv(here::here("data/CDR_oldfield", "cdr2_producer.csv"))
cdr_of_prod <- subset(cdr_of_prod, id_confidence == 1)

cdr_of_con <- read.csv(here::here("data/CDR_oldfield", "cdr2_consumer.csv"))
cdr_of_con <- subset(cdr_of_con, id_confidence == 1)
# only one family sampled in years 1992 and 1993 --> remove
cdr_of_con <- cdr_of_con[!cdr_of_con$year %in% c(1992, 1993),]

filter_data(site_name = "cdr_of", producer_data = cdr_of_prod, consumer_data = cdr_of_con, mean_sum = "mean", output_folder = "data/CDR_oldfield", write_csv = TRUE)

###### CDR BIODIVERSITY ######
# read data and subset confident IDs
cdr_biodiv_prod <-  read.csv(here::here("data/CDR_biodiv", "cdr_producer.csv"))
cdr_biodiv_prod <- subset(cdr_biodiv_prod, id_confidence == 1)

cdr_biodiv_con <- read.csv(here::here("data/CDR_biodiv", "cdr_consumer.csv"))
cdr_biodiv_con <- subset(cdr_biodiv_con, id_confidence == 1)
# cdr_biodiv_con abundances being read in as characters Convert to numeric
cdr_biodiv_con$abundance <- as.numeric(cdr_biodiv_con$abundance)

# set treatments as factor
cdr_biodiv_prod$treatment_seeding <- as.factor(cdr_biodiv_prod$treatment_seeding)
cdr_biodiv_con$treatment_seeding <- as.factor(cdr_biodiv_con$treatment_seeding)

# Split dfs by treatment
cdr_split_prod <- split(cdr_biodiv_prod, cdr_biodiv_prod$treatment_seeding)
cdr_split_con <- split(cdr_biodiv_con, cdr_biodiv_con$treatment_seeding)

# Assign each subset to a variable

# Modify each split to update 'site' and assign to global environment
list2env(
  setNames(
    lapply(names(cdr_split_prod), function(treatment) {
      df_split <- cdr_split_prod[[treatment]]
      df_split$site <- paste0(df_split$site, "_", treatment)
      return(df_split)
    }),
    paste0("cdr_biodiv_prod_", names(cdr_split_prod))
  ),
  envir = .GlobalEnv
)

list2env(
  setNames(
    lapply(names(cdr_split_con), function(treatment) {
      df_split <- cdr_split_con[[treatment]]
      df_split$site <- paste0(df_split$site, "_", treatment)
      return(df_split)
    }),
    paste0("cdr_biodiv_con_", names(cdr_split_con))
  ),
  envir = .GlobalEnv
)

# filter data for each treatment - there's probably a better way to do this
filter_data(site_name = "cdr_biodiv_1", producer_data = cdr_biodiv_prod_1, consumer_data = cdr_biodiv_con_1,
            mean_sum = "mean", output_folder = "data/CDR_biodiv", write_csv = TRUE, minimize = FALSE)

filter_data(site_name = "cdr_biodiv_2", producer_data = cdr_biodiv_prod_2, consumer_data = cdr_biodiv_con_2,
            mean_sum = "mean", output_folder = "data/CDR_biodiv", write_csv = TRUE, minimize = FALSE)

filter_data(site_name = "cdr_biodiv_4", producer_data = cdr_biodiv_prod_4, consumer_data = cdr_biodiv_con_4,
            mean_sum = "mean", output_folder = "data/CDR_biodiv", write_csv = TRUE, minimize = FALSE)

filter_data(site_name = "cdr_biodiv_8", producer_data = cdr_biodiv_prod_8, consumer_data = cdr_biodiv_con_8,
            mean_sum = "mean", output_folder = "data/CDR_biodiv", write_csv = TRUE, minimize = FALSE)

filter_data(site_name = "cdr_biodiv_16", producer_data = cdr_biodiv_prod_16, consumer_data = cdr_biodiv_con_16,
            mean_sum = "mean", output_folder = "data/CDR_biodiv", write_csv = TRUE, minimize = FALSE)


#### AQUATIC DATA ####

###### GCE ######
gce_prod <-  read.csv(here::here("data/GCE", "gce_producers.csv"))
gce_con <- read.csv(here::here("data/GCE", "gce_consumers.csv"))


# correct column name
colnames(gce_prod)[colnames(gce_prod) == "scale_abundnace"] <- "scale_abundance"
colnames(gce_con)[colnames(gce_con) == "scale_abundnace"] <- "scale_abundance"

# subset confident ids
gce_prod <- subset(gce_prod, id_confidence == 1)
gce_con <- subset(gce_con, id_confidence == 1)


# gce_prod has a -1 instead of a 1 in one of the habitat_fine --> correct to 1?
gce_prod <- gce_prod %>%
  mutate(habitat_fine = ifelse(habitat_fine == -1, 1, habitat_fine))

# NOTE: each plot can belong to 1 of 3 zones - should we re-factor plot to paste0(gce_prod$plot, "-", gce_prod$habitat_fine)?
# moving ahead with this approach - delete these lines if this is incorrect
# filtering seems to not work properly if we don't do this because of spatial co-location issues
gce_prod_mutated <- gce_prod %>%
  mutate(plot = paste0(gce_prod$plot, "-", gce_prod$habitat_fine))

gce_con_mutated <- gce_con %>%
  mutate(plot = paste0(gce_con$plot, "-", gce_con$habitat_fine))

filter_data(site_name = "gce", producer_data = gce_prod_mutated, consumer_data = gce_con_mutated, mean_sum = "mean", output_folder = "data/gce", write_csv = TRUE)

##### USVI #####
usvi_prod <-  read.csv(here::here("data/USVI", "usvi_benthic_cover_algae.csv"))
usvi_con <- read.csv(here::here("data/USVI", "usvi_fish_census.csv"))

# subset confident ids
usvi_prod <- subset(usvi_prod, id_confidence == 1)
usvi_con <- subset(usvi_con, id_confidence == 1)

# MUTATING PLOT AS ABOVE FOR FILTERING - RETURN HERE PENDING JUNNA'S DATA EXPLORATION
usvi_prod_mutated <- usvi_prod %>%
  mutate(plot = paste0(usvi_prod$plot, "-", usvi_prod$habitat_fine))

usvi_con_mutated <- usvi_con %>%
  mutate(plot = paste0(usvi_con$plot, "-", usvi_con$habitat_fine))

filter_data(site_name = "usvi", producer_data = usvi_prod_mutated, consumer_data = usvi_con_mutated, mean_sum = "mean", output_folder = "data/USVI", write_csv = TRUE)

##### SBC #####
sbc_all <-  read.csv(here::here("data/SBC", "sbc_all.csv"))

# subset producers
sbc_prod <- subset(sbc_all, taxa_type == "producer")

# coerce macroalge = algae, as we do not make that distinction in our protocol anymore
sbc_prod <- sbc_prod %>%
  mutate(guild = ifelse(guild == "algae", "macroalgae", guild))


sbc_prod <- subset(sbc_prod, id_confidence == 1)

# subset out invert and fish as unique consumer guilds
sbc_invert <- subset(sbc_all, guild == "invert")
sbc_fish <- subset(sbc_all, guild == "fish")
sbc_fish <- subset(sbc_fish, id_confidence == 1)

# remove suspension feeders from inverts --> don't interact with the benthos/primary producers at all as consumers
sbc_invert <- sbc_invert[!(sbc_invert$taxa_type %in% c("suspension_feeder_detritivore", "suspension_feeder")),]
sbc_invert <- subset(sbc_invert, id_confidence == 1)

# create the two sets of data
filter_data(site_name = "sbc_fish", producer_data = sbc_prod, consumer_data = sbc_fish, mean_sum = "mean", output_folder = "data/sbc", write_csv = TRUE)

filter_data(site_name = "sbc_invert", producer_data = sbc_prod, consumer_data = sbc_invert, mean_sum = "mean", output_folder = "data/sbc", write_csv = TRUE)

##### MCR #####
mcr_prod <-  read.csv(here::here("data/MCR", "mcr_algae.csv"))

# coerce macroalge = algae, as we do not make that distinction in our protocol anymore
# remove filter feeders
mcr_prod <- subset(mcr_prod, taxa_type == "producer") %>%
  mutate(guild = ifelse(guild == "algae", "macroalgae", guild))

mcr_prod <- subset(mcr_prod, id_confidence == 1)

# fish
mcr_fish <- read.csv(here::here("data/MCR", "mcr_fish.csv"))
mcr_fish <- subset(mcr_fish, id_confidence == 1)
# fish transects have two different sizes which makes the merge break
# coercing into a single value BUT WE WILL PROBABLY HAVE TO FIND A WAY TO ADDRESS THIS AT SOMEPOINT
mcr_fish$scale_abundance <- rep("transect", nrow(mcr_fish))

# inverts - remove suspension feeders for reasons stated above
mcr_invert <- read.csv(here::here("data/MCR", "mcr_invertebrate.csv"))
mcr_invert <- mcr_invert[!(mcr_invert$taxa_type %in% c("suspension_feeder")),]
mcr_invert <- subset(mcr_invert, id_confidence == 1)

filter_data(site_name = "mcr_fish", producer_data = mcr_prod, consumer_data = mcr_fish, mean_sum = "mean", output_folder = "data/MCR", write_csv = TRUE)


filter_data(site_name = "mcr_invert", producer_data = mcr_prod, consumer_data = mcr_invert, mean_sum = "mean", output_folder = "data/MCR", write_csv = TRUE)

