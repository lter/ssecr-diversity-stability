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



