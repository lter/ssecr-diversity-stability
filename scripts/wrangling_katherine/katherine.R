#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Katherine Hulting
# Load `librarian` package
library(librarian)
# Install missing packages and load needed libraries
shelf(tidyverse, summarytools, googlesheets4, googledrive)



#### KBS LTER harmonization ####
# CONSUMER 
# loading data
kbs_consum_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-kbs.23.30&entityid=8d33fa9169147f266d20bdcd09a07820"
kbs_consumer_raw <- read.csv(file = kbs_consum_url)

kbs_consumer <- kbs_consumer_raw %>%
  filter(Treatment == "T7") %>% # filtering out non-early successional treatments - only want T7 treatments
  mutate(Sample_Date = as_datetime(Sample_Date)) %>% # converting sampling date to datetime
  mutate(month = month(Sample_Date)) %>% # adding month as a column - already ahve year as a column
  filter(Adults != 0) %>% # removing rows that have "0" for counts of insects
  mutate(site = "KBS", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "insect", 
         unit_abundance = "count", 
         scale_abundance = "1 trap") %>%
  mutate(plot = Replicate, subplot = Station, abundance = Adults, taxon_name = Species, year = Year, # renaming columns
         Replicate = NULL, Station = NULL, Adults = NULL, Species = NULL, Year = NULL) %>% # removing old columns
  mutate(abundance = as.numeric(abundance)) %>%
  mutate(year = as.double(year)) %>%
  mutate(unique_ID = paste(site, plot, subplot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "unique_ID", "taxon_name", "abundance", 
                  "unit_abundance", "scale_abundance"))

kbs_consumer <- kbs_consumer %>%
  mutate(id_confidence = if_else( # adding confidence column for species ID -- not going to include species IDed as "Other"
    taxon_name %in% c("Other"), 0, 1
  )) %>%
  mutate(herbivore = dplyr::case_when( # classifying feeding group
    taxon_name %in% c(
      "Adalia bipunctata", "Brachiacantha ursina", "Chilocorus stigma",
      "Coccinella septempunctata", "Coccinella trifaciata", "Coleomegilla maculata",
      "Cycloneda munda", "Harmonia axyridis", "Hippodamia convergens", "Hippodamia glacialis",
      "Hippodamia parenthesis", "Hippodamia tredecimpunctata", "Hippodamia variegata", 
      "Propylea quatuorodecipuntata"
    ) ~ "predator",
    taxon_name %in% c( # these are true omnivores, but include species in the families that have different feeding modes
      "Cantharid", "Carabid", "Lacewing", "Lampyrid", "Mecoptera",
      "Psyllobora vigintimaculata" # consumes primarily fungus
    ) ~ "omnivore",
    taxon_name %in% c(
      "Syrphid"
    ) ~ "nectarivorous"
  ))


write.csv(kbs_consumer,"kbs_consumer.csv", row.names = FALSE) # exporting csv to local computer

googledrive::drive_upload(media = file.path("kbs_consumer.csv"), overwrite = T, # exporting to google drive
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1O2n89tOIMNZGXTzCZNb0Qsj_C8dRI09l"))


# PRODUCER
kbs_produce_url <- "https://lter.kbs.msu.edu/datatables/291.csv"
kbs_producer_raw <- read.csv(file = kbs_produce_url, skip = 26) # skipping metadata rows at top of file

kbs_producer <- kbs_producer_raw %>%
  filter(Treatment == "T7") %>% # filtering out non-early successional treatments - only want T7 treatments
  mutate(Date = as_datetime(Date)) %>% # converting sampling date to datetime
  mutate(month = month(Date)) %>% # adding month as a column - already ahve year as a column
  mutate(site = "KBS", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "plant", 
         unit_abundance = "g/m2", 
         scale_abundance = "1 m2") %>%
  mutate(Replicate = str_sub(Replicate, -1), # removing "R" in front of rep # to be consistent with insect data
         Station = str_sub(Station, -1)) %>% # removing "S" in front of station # to be consistent with insect data
  mutate(plot = Replicate, subplot = Station, abundance = Biomass_g, taxon_name = Species, year = Year, # renaming columns
         Replicate = NULL, Station = NULL, Biomass_g = NULL, Species = NULL, Year = NULL) %>% # removing old columns
  mutate(abundance = as.numeric(abundance)) %>%
  mutate(year = as.double(year)) %>%
  mutate(unique_ID = paste(site, plot, subplot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "unique_ID", "taxon_name", "abundance", 
                  "unit_abundance", "scale_abundance"))

kbs_producer$taxon_name <- str_trim(kbs_producer$taxon_name, side = "both") # removing white space from strings

kbs_producer <- kbs_producer %>%
  mutate(taxon_name = dplyr::case_when(
    taxon_name %in% c("Brassica rapa L.", "Brassica sp.", "Brassica kaber (DC.) L.C.Wheeler") ~ "Brassica sp.", # 25% IDed at genus
    taxon_name %in% c("Carex muhlenburgii", "Carex sp.") ~ "Carex sp.", # 14% (1/7) IDed at genus
    taxon_name %in% c("Celastrus orbiculatus Thunb.", "Celastrus sp.") ~ "Celastrus sp.", # 17% (7/39) IDed at genus
    taxon_name %in% c("Cerastium arvense L.", "Cerastium sp.", "Cerastium vulgatum L.") ~ "Cerastium sp.", # 14% (15/104) IDed at genus
    taxon_name %in% c("Cirsium altissimum (L.) Spreng.", "Cirsium arvense (L.) Scop.", "Cirsium discolor (Muhl. ex Willd.) Spreng.",
                      "Cirsium sp.", "Cirsium vulgare (Savi) Tenore") ~ "Cirsium sp.", # 43% (23/53) IDed at genus
    taxon_name %in% c("Cornus racemosa Lam.", "Cornus sp.") ~ "Cornus sp.", # 86% (12/14) IDed at genus
    taxon_name %in% c("Desmodium illinoense", "Desmodium marilandicum (L.) DC.", 
                      "Desmodium paniculatum (L.) DC.", "Desmodium sp.") ~ "Desmodium sp.", # 53% (8/15) IDed at genus
    taxon_name %in% c("Festuca rubra L.", "Festuca sp.") ~ "Festuca sp.", # 33% (2/6) IDed at genus
    taxon_name %in% c("Fragaria sp.", "Fragaria virginiana Duchesne") ~ "Fragaria sp.", # 33% (1/3) IDed at genus
    taxon_name %in% c("Geum laciniatum", "Geum sp.", "Geum virginianum") ~ "Geum sp.", # 88% (49/56) IDed at genus
    taxon_name %in% c("Hieracium aurantiacum L.", "Hieracium caespitosum", "Hieracium sp.") ~ "Hieracium sp.", # 93% (244/261) IDed at genus
    taxon_name %in% c("Juncus acuminatus Michx.", "Juncus sp.", "Juncus tenuis Willd.") ~ "Juncus sp.", # 6% (10/162) IDed at genus
    taxon_name %in% c("Lactuca canadensis L.", "Lactuca saligna L.", "Lactuca sp.", "Lactuca serriola L.") ~ "Lactuca sp.", # 22% (4/18) IDed at genus
    taxon_name %in% c("Lonicera morrowii", "Lonicera spp.") ~ "Lonicera spp.", # 86% (6/7) IDed at genus
    taxon_name %in% c("Melilotus alba var. annua H.S.Coe", "Melilotus officinalis (L.) Lam.", "Melilotus sp.") ~ "Melilotus sp.", # 29% (5/17) IDed at genus
    taxon_name %in% c("Monarda fistulosa L.", "Monarda sp.") ~ "Monarda sp.", # 20% (1/5) IDed at genus
    taxon_name %in% c("Plantago lanceolata L.", "Plantago major L.", "Plantago rugelii Dcne.", "Plantago sp.") ~ "Plantago sp.", # 10% (1/10) IDed at genus
    taxon_name %in% c("Populus deltoides", "Populus sp.") ~ "Populus sp.", # 21% (3/14) IDed at genus
    taxon_name %in% c("Prunus serotina Ehrh.", "Prunus spp.", "Prunus virginiana L.") ~ "Prunus spp.", # 22% (2/9) IDed at genus
    taxon_name %in% c("Rhamnus cathartica L.", "Rhamnus frangula L.", "Rhamnus sp.") ~ "Rhamnus sp.", # 33% (2/6) IDed at genus
    taxon_name %in% c("Rosa multiflora Thunb. ex Murr.", "Rosa sp.") ~ "Rosa sp.", # 87% (13/15) IDed at genus
    taxon_name %in% c("Rubus allegheniensis T.C. Porter", "Rubus flagellaris", "Rubus occidentalis L.", "Rubus sp.") ~ "Rubus sp.", # 17% (41/239) IDed at genus
    taxon_name %in% c("Sonchus arvensis L.", "Sonchus asper (L.) Hill", "Sonchus oleraceus L.", "Sonchus sp.") ~ "Sonchus sp.", # 20% (1/5) IDed at genus
    taxon_name %in% c("Veronica arvensis L.", "Veronica peregrina L.", "Veronica sp.") ~ "Veronica sp.", # 66% (14/21) IDed at genus
    taxon_name %in% c("Vicia sp.", "Vicia villosa  Roth") ~ "Vicia sp.", # 47% (8/17) IDed at genus
    taxon_name %in% c("Vitex agnus-castus L.", "Vitis aestivalis Michx.", "Vitis riparia Michx.", "Vitis sp.") ~ "Vitis sp.", # 40% (4/10) IDed at genus
    .default = taxon_name
  )) %>%
  mutate(id_confidence = dplyr::case_when(
    taxon_name %in% c(
      "another unknown dicot", "unknown Asteraceae", "unknown Brassicaceae", "unknown Sedge",
      "Unknown dicot", "Unknown grass", "Unknown monocot", "Unkown Fabaceae", "UnSorted",
      "Composite Basal", "Composite sp.", "Dicots", "Monocots", "Unknown Orchidaceae", 
      "Composite Basal Leaves", "Aster basal leaves", 
      "Aster sp.", "Bromus sp.", # genus classifications
      "Erigeron sp.", "Poa sp.", "Rumex species", "Setaria sp.", "Solidago sp.", "Trifolium sp."
    ) ~ 0,
    .default = 1
  ))

write.csv(kbs_producer,"kbs_producer.csv", row.names = FALSE) # exporting csv

googledrive::drive_upload(media = file.path("kbs_producer.csv"), overwrite = T, # exporting to google drive
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1O2n89tOIMNZGXTzCZNb0Qsj_C8dRI09l"))



#### KNZ LTER harmonization ####
# producer
knz_produce_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.69.24&entityid=63768b48f41e790a40e7fa4f9267c3a2"
knz_producer_raw <- read.csv(file = knz_produce_url)


knz_producer <- knz_producer_raw %>%
  filter(SoilType == "f") %>% # only keeping upland (florance) soil types to match consumer
  mutate(site = "KNZ", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "plant", 
         unit_abundance = "cover class", 
         scale_abundance = "10 m2") %>%
  mutate(plot = WaterShed, subplot = Transect, sub_subplot = Plot, 
         abundance = Cover, genus = AB_genus, species = AB_species, year = RecYear,
         month = RecMonth, day = RecDay) %>% # renaming columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "sub_subplot", "year", "month", "day", "unique_ID", "genus", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

# changing cover classes to midpoint of cover class - according to Konza sampling manual https://lter.konza.ksu.edu/sites/default/files/MM_0.pdf
knz_producer$abundance <- as.factor(knz_producer$abundance)
knz_producer$abundance <- fct_recode(knz_producer$abundance, "0.5" = "1", "3.0" = "2", "15.0" = "3",
                                     "37.5" = "4", "62.5" = "5", "85.0" = "6", "97.5" = "7")
knz_producer$abundance <- as.numeric(as.character(knz_producer$abundance))


# assigning confidence to species IDs
knz_producer <- knz_producer %>%
  mutate(taxon_name = paste(genus, species, sep = " ")) %>% # temporarily pasting together genus and species for assigning ID confidence
  mutate(id_confidence = if_else( # adding confidence column for species ID 
    taxon_name %in% c("annual forb", "carex spp.", "euphor spp."), 0, 1
  )) %>%
  dplyr::select(!c("genus", "species"))

write.csv(knz_producer,"knz_producer.csv", row.names = FALSE) # exporting csv

googledrive::drive_upload(media = file.path("knz_producer.csv"), overwrite = T, # exporting to google drive
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1UUXzJUKvjcRZW-yzI78O_GLFtSPUCclg"))



# consumer
knz_consume_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.29.23&entityid=3fb352e2478f776517f7e880fe31b808"
knz_consumer_raw <- read.csv(file = knz_consume_url)


knz_consumer <- knz_consumer_raw %>%
  filter(SOILTYPE == "fl") %>% # only keeping upland (florance) soil types - others were not sampled long term
  mutate(site = "KNZ", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "insect", 
         unit_abundance = "count", 
         scale_abundance = "200 sweeps") %>%
  mutate(plot = WATERSHED, subplot = REPSITE, 
       abundance = TOTAL, taxon_name = SPECIES, year = RECYEAR,
       month = RECMONTH, day = RECDAY) %>% # renaming columns
  mutate(plot = tolower(plot)) %>% # lowercasing plot to be consistent with producer
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "taxon_name", "abundance", 
                  "unit_abundance", "scale_abundance"))

knz_consumer$abundance <- str_replace(knz_consumer$abundance, "1 01", "101") # fixing abundance typo
knz_consumer <- knz_consumer %>%
  filter(!abundance %in% c("", "0")) %>% # removing 0 and NA for abundance
  mutate(abundance = as.numeric(abundance)) # converting abundance to numeric

  

knz_consumer <- knz_consumer %>%
  mutate(taxon_name = dplyr::case_when( # fixing typos/capitalizations
    taxon_name %in% c("Brachystola magna", "brachystol magna") ~ "Brachystola magna",
    taxon_name %in% c("Schistocerca lineata", "schistocer lineata") ~ "Schistocerca lineata",
    taxon_name %in% c("Paratfylotropidia brunneri", "paratylota brunneri", "paratylotr brunneri") ~ "Paratylotropidia brunneri",
    taxon_name %in% c("Hypochlora alba", "hypochlora alba") ~ "Hypochlora alba",
    taxon_name %in% c("Campylacantha olivacea", "campylacan olivacea") ~ "Campylacantha olivacea",
    taxon_name %in% c("Hesperotettix speciosus", "hesperotet speciosus") ~ "Hesperotettix speciosus",
    taxon_name %in% c("Hesperotettix viridis", "hesperotet viridis") ~ "Hesperotettix viridis",
    taxon_name %in% c("Hesperotettix spp.", "hesperotet species", "hesperotet spp.") ~ "Hesperotettix spp.",
    taxon_name %in% c("Phoetaliotes nebrascensis", "phoetaliot nebrascen") ~ "Phoetaliotes nebrascensis",
    taxon_name %in% c("Melanoplus scudderi", "melanoplus scudderi") ~ "Melanoplus scudderi",
    taxon_name %in% c("Melanoplus sanguinipes", "melanoplus sanguinip") ~ "Melanoplus sanguinipes",
    taxon_name %in% c("Melanoplus femurrubrum", "melanoplus femurrubr") ~ "Melanoplus femurrubrum",
    taxon_name %in% c("Melanoplus keeleri", "melanoplus keeleri") ~ "Melanoplus keeleri",
    taxon_name %in% c("Melanoplus packardii", "melanoplus packardii") ~ "Melanoplus packardii",
    taxon_name %in% c("Melanoplus differentialis", "melanoplus different") ~ "Melanoplus differentialis",
    taxon_name %in% c("Melanoplus bivittatus", "melanoplus bivittatu") ~ "Melanoplus bivittatus",
    taxon_name %in% c("Melanoplus confusus", "melanoplus confusus") ~ "Melanoplus confusus",
    taxon_name %in% c("Melanoplus spp.", "melanoplus species", "melanoplus spp.") ~ "Melanoplus spp.",
    taxon_name %in% c("Eritettix simplex", "eritettix simplex") ~ "Eritettix simplex",
    taxon_name %in% c("Syrbula admirabilis", "syrbula admirabil") ~ "Syrbula admirabilis",
    taxon_name %in% c("Orphulella speciosa", "orphulella speciosa", "orphullela speciosa") ~ "Orphulella speciosa",
    taxon_name %in% c("Mermiria picta", "mermiria picta") ~ "Mermiria picta",
    taxon_name %in% c("Mermiria bivittata", "mermiria bivitatta", "mermiria bivittata") ~ "Mermiria bivittata",
    taxon_name %in% c("Opeia obscura", "opeia obscura") ~ "Opeia obscura",
    taxon_name %in% c("Pseuodopomala brachyptera", "pseudopoma brachypte") ~ "Pseuodopomala brachyptera",
    taxon_name %in% c("Boopedon auriventris", "boopedon auriventr") ~ "Boopedon auriventris",
    taxon_name %in% c("Boopedon nubilum", "boopedon nubilum") ~ "Boopedon nubilum",
    taxon_name %in% c("Boopedon gracile", "boopedon gracile") ~ "Boopedon gracile",
    taxon_name %in% c("Ageneotettix deorum", "ageneotett deorum") ~ "Ageneotettix deorum",
    taxon_name %in% c("Mermiria spp.", "mermiria species", "mermiria spp.") ~ "Mermiria spp.",
    taxon_name %in% c("Chortophaga viridifasciata", "chortophag viridifas") ~ "Chortophaga viridifasciata",
    taxon_name %in% c("Arphia xanthoptera", "arphia xanthopte") ~ "Arphia xanthoptera",
    taxon_name %in% c("Arphia simplex", "arphia simplex") ~ "Arphia simplex",
    taxon_name %in% c("Arphia conspersa", "arphia conspersa") ~ "Arphia conspersa",
    taxon_name %in% c("Hadrotettix trifasciatus", "hadrotetti trifascia") ~ "Hadrotettix trifasciatus",
    taxon_name %in% c("Hippiscus rugosus", "hippiscus rugosus") ~ "Hippiscus rugosus",
    taxon_name %in% c("Pardalophora haldemani", "pardalopho haldemani") ~ "Pardalophora haldemani",
    taxon_name %in% c("Arphia spp.", "arphia species", "arphia spp.") ~ "Arphia spp.",
    taxon_name %in% c("Schistocerca obscura", "schistocer obscura") ~ "Schistocerca obscura",
    taxon_name %in% c("Encoptolophus sordidus", "encoptolop sordidus") ~ "Encoptolophus sordidus",
    taxon_name %in% c("Melanoplus angustipennis", "melanoplus angustipe") ~ "Melanoplus angustipennis",
    taxon_name %in% c("Xanthippus corallipes", "xanthippus corallipe") ~ "Xanthippus corallipes",
    taxon_name %in% c("Encoptolophus subgracilis", "encoptolop subgracil") ~ "Encoptolophus subgracilis",
    taxon_name %in% c("Encoptolphus spp.", "encoptolop spp.") ~ "Encoptolophus spp.",
    taxon_name %in% c("melanoplus foedus") ~ "Melanoplus foedus",
    taxon_name %in% c("melanoplus occidenta") ~ "Melanoplus occidenta",
    taxon_name %in% c("oedipodinae", "Oedipodinae spp.") ~ "Oedipodinae spp.",
    taxon_name %in% c("pardalopho apiculata") ~ "Pardalopho apiculata",
    taxon_name %in% c("pardalopho species", "pardalopho spp.") ~ "Pardalopho species",
    taxon_name %in% c("psoloessa delicatul") ~ "Psoloessa delicatul",
    .default = as.character(taxon_name)
  )) %>%
  mutate(taxon_name = dplyr::case_when( # grouping at genus when >5% of observations are at genus level
    taxon_name %in% c("Amblycorypha oblongifolia", "Amblycorypha rotundifolia", "Amblycorypha spp.") ~ "Amblycorypha spp.", # 20% (1/5) at genus level
    taxon_name %in% c("Arphia conspersa", "Arphia simplex", "Arphia spp.", "Arphia xanthoptera") ~ "Arphia spp.", # 53% (230/431) at genus level
    taxon_name %in% c("Encoptolophus sordidus", "Encoptolophus spp.", "Encoptolophus subgracilis") ~ "Encoptolophus spp.", # 18% (8/44) at genus level
    taxon_name %in% c("Hesperotettix speciosus", "Hesperotettix spp.", "Hesperotettix viridis") ~ "Hesperotettix spp.", # 40% (442/1084) at genus level
    taxon_name %in% c("Mermiria bivittata", "Mermiria picta", "Mermiria spp.") ~ "Mermiria spp.", # 30% (174/588) at genus level
    taxon_name %in% c("Neoconocephalus ensiger", "Neoconocephalus robustus", "Neoconocephalus spp.") ~ "Neoconocephalus spp.", # 9% (1/11) at genus level
    taxon_name %in% c("Pardalopho apiculata", "Pardalopho species") ~ "Pardalopho spp.", # 20% (20/102) at genus level
    taxon_name %in% c("Pardalophora haldemani", "Pardalophora spp.") ~ "Pardalophora spp.", # 8% (7/90) at genus level
    taxon_name %in% c("Schistocerca alutacea", "Schistocerca lineata", "Schistocerca obscura", "Schistocerca spp.") ~ "Schistocerca spp.", # 12% (13/107) at genus level,
    .default = taxon_name
  )) %>%
  mutate(herbivore = "herbivore") %>% # all herbivores
  mutate(id_confidence = dplyr::case_when( # assigning ID confidence
    taxon_name %in% c(
      "unknown ", "unknown", "Unknown", "Boopedon spp.", "Hadrotettix spp.",
      "Melanoplus spp.", "Oedipodinae spp.", "Scudderia spp."
    ) ~ 0,
    .default = 1
  )) 


write.csv(knz_consumer,"knz_consumer.csv", row.names = FALSE) # exporting csv

googledrive::drive_upload(media = file.path("knz_consumer.csv"), overwrite = T, # exporting to google drive
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1UUXzJUKvjcRZW-yzI78O_GLFtSPUCclg"))





#### CDR LTER harmonization ####
# producer 
cdr_producer_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-cdr.273.11&entityid=27ddb5d8aebe24db99caa3933e9bc8e2"
cdr_producer_raw <- read.csv(file = cdr_producer_url)


cdr_producer <- cdr_producer_raw %>%
  mutate(site = "CDR", # adding general LTER/dataset info to each row
         taxa_type = "producer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "plant", 
         unit_abundance = "g/m2", 
         scale_abundance = "m2") %>%
  mutate(plot = Plot, subplot = Strip, 
         abundance = `Biomass..g.m2.`, species = Species, year = Year,
         month = Month) %>% # renaming columns
  separate(Date, c("month_discard", "day", "year_discard"), sep = "/") %>% # separating date components in order to get "day" -- not using month/year from this, discarding those extra columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

cdr_producer$species <- str_replace(cdr_producer$species, "Festuca Sp.", "Festuca sp.") # fixing typo
cdr_producer$species <- str_replace(cdr_producer$species, "cyperus sp", "Cyperus sp.") # fixing typo
cdr_producer$species <- str_replace(cdr_producer$species, "leptoloma sp.", "Leptoloma sp.") # fixing typo


cdr_producer <- cdr_producer %>%
  mutate(id_confidence = dplyr::case_when(
    species %in% c("32 Species Weeds", "Fungi", "Grasses", "Green matter", "Green matter (alive)", 
          "Miscellaneous Forb", "Miscellaneous forb", "Miscellaneous grass", "Miscellaneous grasses",
          "Miscellaneous herbs", "Miscellaneous litter", "Miscellaneous sedges", "Miscellaneous sp.",
          "Mosses", "Mosses & lichens", "Real Weeds", "Unsorted Biomass", "Unsorted biomass", "Weeds",
          "era sp", "erg sp", "miscellaneous seedhead", "unknown forb", "unknown grass",
          "Agrostis sp.", "Erigeron sp.", "Euphorbia sp.", "Lepidium sp.", "Leptoloma sp.",
          "Lupinus sp.", "Panicum sp.", "Petalostemum sp.", "Rumex sp.", "Silene sp.",
          "Solidago sp.", "Sporobolus sp.", "Trifolium sp.") ~ 0,
    .default = 1
  ))

cdr_producer %>% # still have some to group/drop
  filter(id_confidence == 1) %>%
  count(species)
# need to seperate genus/species into 2 parts




# consumer
cdr_consumer_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-cdr.418.8&entityid=aae64949e1ef41513062633cfb6da7d5"
cdr_consumer_raw <- read.delim(file = cdr_consumer_url)


cdr_consumer <- cdr_consumer_raw %>%
  mutate(site = "CDR", # adding general LTER/dataset info to each row
         taxa_type = "consumer",
         ecosystem = "terrestrial",
         habitat_broad = "grassland",
         habitat_fine = "grassland",
         biome = "temperate",
         guild = "insect", 
         unit_abundance = "count", 
         scale_abundance = "25 sweeps") %>%
  mutate(plot = Plot, subplot = NA, 
         abundance = Count, species = paste(Genus, Specific.epithet, Further.ID, sep = " "), year = Year,
         month = Month, day = NA) %>% # renaming columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))



cdr_consumer_raw$Further.ID <- str_trim(cdr_consumer_raw$Further.ID, side = c("both")) 
cdr_consumer_raw$Further.ID <- str_replace_all(cdr_consumer_raw$Further.ID, "^na$", "") 

cdr_consumer_trial_taxonomy <- cdr_consumer_raw %>%
  mutate(ID = paste(Genus, Specific.epithet, Further.ID, sep = " ")) %>%
  count(Order, Family.subfamily., ID)


## Loading info on taxonomy resolutions from google drive
googledrive::drive_auth() # Authenticate Google drive 
cdr_consumer_taxonomy <- read_sheet("https://docs.google.com/spreadsheets/d/14KEoAB88NcpEHVWZ0-VvB8hEXTNMWwCjf0kWB311fJg/edit?gid=45649463#gid=45649463")

cdr_consumer_trial_taxonomy$ID <- str_trim(cdr_consumer_trial_taxonomy$ID, side = c("both")) 
cdr_cleaned_species$PreferredName <- str_trim(cdr_cleaned_species$PreferredName, side = c("both")) 

cdr_consumer_trial_taxonomy %>%
  full_join(cdr_cleaned_species, by = c("ID" = "PreferredName")) %>%
  filter(!is.na(n.x)) %>%
  View()



cdr_cleaned_species <- cdr_consumer_taxonomy %>%
  count(Order, Family, PreferredName, PreferredCode)










