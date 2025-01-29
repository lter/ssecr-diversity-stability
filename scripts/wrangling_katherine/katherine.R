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
         abundance = `Biomass..g.m2.`, taxon_name = Species, year = Year,
         month = Month) %>% # renaming columns
  separate(Date, c("month_discard", "day", "year_discard"), sep = "/") %>% # separating date components in order to get "day" -- not using month/year from this, discarding those extra columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "taxon_name", "abundance", 
                  "unit_abundance", "scale_abundance"))

cdr_producer$taxon_name <- str_replace(cdr_producer$taxon_name, "Festuca Sp.", "Festuca sp.") # fixing typo
cdr_producer$taxon_name <- str_replace(cdr_producer$taxon_name, "cyperus sp", "Cyperus sp.") # fixing typo
cdr_producer$taxon_name <- str_replace(cdr_producer$taxon_name, "leptoloma sp.", "Leptoloma sp.") # fixing typo



cdr_producer <- cdr_producer %>%
  mutate(taxon_name = dplyr::case_when( # grouping at genus level
    taxon_name %in% c("Aristida basiramea", "Aristida sp.") ~ "Aristida sp.", #37% (334/902) at genus level
    taxon_name %in% c("Cyperus filiculmis", "Cyperus schweinitzii", "Cyperus sp.") ~ "Cyperus sp.", #77% (1523/1973) at genus level
    taxon_name %in% c("Digitaria ischaemum", "Digitaria sanguinalis", "Digitaria sp.") ~ "Digitaria sp.", #95% (311/327) at genus level
    taxon_name %in% c("Equisetum arvense", "Equisetum laevigatum", "Equisetum sp.") ~ "Equisetum sp.", #6% (3/51) at genus level
    taxon_name %in% c("Festuca ovina", "Festuca sp.") ~ "Festuca sp.", #78% (59/76) at genus level
    taxon_name %in% c("Hieracium aurantiacum", "Hieracium longipilum", "Hieracium sp.") ~ "Hieracium sp.", #7% (1/14) at genus level
    taxon_name %in% c("Lactuca biennis", "Lactuca sp.") ~ "Lactuca sp.", #33% (1/3) at genus level
    taxon_name %in% c("Oxalis sp.", "Oxalis stricta") ~ "Oxalis sp.", #76% (10/13) at genus level
    taxon_name %in% c("Potentilla argentea", "Potentilla sp.") ~ "Potentilla sp.", #33% (1/3) at genus level
    taxon_name %in% c("Setaria italica", "Setaria lutescens (glauca)", "Setaria sp.", "Setaria viridis") ~ "Setaria sp.", #19% (7/37) at genus level
    taxon_name %in% c("Calamagrostis canadensis", "Calamagrostis sp.") ~ "Calamagrostis sp.", #14% (1/7) at genus level
    .default = taxon_name
  )) %>%
  mutate(id_confidence = dplyr::case_when( # assigining ID confidence
    taxon_name %in% c("32 Species Weeds", "Fungi", "Grasses", "Green matter", "Green matter (alive)", 
          "Miscellaneous Forb", "Miscellaneous forb", "Miscellaneous grass", "Miscellaneous grasses",
          "Miscellaneous herbs", "Miscellaneous litter", "Miscellaneous sedges", "Miscellaneous sp.",
          "Mosses", "Mosses & lichens", "Real Weeds", "Unsorted Biomass", "Unsorted biomass", "Weeds",
          "era sp", "erg sp", "miscellaneous seedhead", "unknown forb", "unknown grass",
          "Agrostis sp.", "Erigeron sp.", "Euphorbia sp.", "Lepidium sp.", "Leptoloma sp.",
          "Lupinus sp.", "Panicum sp.", "Petalostemum sp.", "Rumex sp.", "Silene sp.",
          "Solidago sp.", "Sporobolus sp.", "Trifolium sp.") ~ 0,
    .default = 1
  ))


write.csv(cdr_producer,"cdr_producer.csv", row.names = FALSE) # exporting csv

googledrive::drive_upload(media = file.path("cdr_producer.csv"), overwrite = T, # exporting to google drive
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1UPaIm6Tp8aQH0gUrOCEz8tV_mkArQkhw"))






# consumer
cdr_consumer_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-cdr.418.8&entityid=aae64949e1ef41513062633cfb6da7d5"
cdr_consumer_raw <- read.delim(file = cdr_consumer_url)

# reading in taxonomy info
# matches old taxonomy to new taxonomy
#cdr_consumer_taxonomy <- read_sheet("https://docs.google.com/spreadsheets/d/1R1byFNUthmeHypO1gvNslVnW2YwiKlLu1Kaw7hxdvRA/edit?usp=drive_link")
#cdr_consumer_taxonomy <- as.data.frame(cdr_consumer_taxonomy)
#setwd("~/Documents/SSECR/project/cdr")
#cdr_consumer_taxonomy <- read.csv("CDR-taxonomy.csv")
# matches old taxonomy to trophic level
#cdr_trophic_level <- read.csv("Trophic level.csv")
#cdr_trophic_level <- read_sheet("https://docs.google.com/spreadsheets/d/1fmNRM13-M3q1TNNhxYRh85TFfSerQmb7nNK1PmKai_k/edit?gid=0#gid=0")
#cdr_trophic_level <- as.data.frame(cdr_trophic_level)
#setwd("~/Documents/SSECR/ssecr-diversity-stability")

#cdr_trophic_level <- cdr_trophic_level %>%
#  select(c("Code", "Trophic"))
#cdr_trophic_level$Trophic <- as.numeric(cdr_trophic_level$Trophic)
#cdr_trophic_level <- cdr_trophic_level %>%
#  filter(Code != "")

#cdr_consumer_taxonomy <- cdr_consumer_taxonomy %>% # joining codes/updated names to trophic level
#  left_join(cdr_trophic_level, by = c("OLD_PreferredCode" = "Code")) %>%
#  select(!c("id_confidence"))

# fixing updates taxonomy names
cdr_consumer <- cdr_consumer_raw %>%
  mutate(Further.ID = case_when(
    Further.ID %in% c("na") ~ "",
    .default = Further.ID
  )) %>%
  mutate(taxon_name = paste(Order, Family.subfamily., Genus, Specific.epithet, Further.ID, sep = " ")) 

cdr_consumer$taxon_name <- str_trim(cdr_consumer$taxon_name, side = "both") # removing white space from strings

cdr_consumer <- cdr_consumer %>%
  mutate(taxon_name = dplyr::case_when(
    taxon_name %in% c("Araneae Araneidae Argiope undet ", "Araneae Araneidae Argiope undet (small)", "Araneae Araneidae Argiope undet") ~ "Araneae Araneidae Argiope sp.",
    taxon_name %in% c("Araneae Thomisidae Misumenops asperatus", "Araneae Thomisidae Misumenops undet", "Araneae Thomisidae Misumenops undet sp2.(pale.hairy)") ~ "Araneae Thomisidae Misumenops sp.",
    taxon_name %in% c("Coleoptera Chrysomelidae Altica fuscoaenea", "Coleoptera Chrysomelidae Altica undet") ~ "Coleoptera Chrysomelidae Altica sp.",
    taxon_name %in% c("Coleoptera Chrysomelidae Pachybrachis carbonarius (black)", "Coleoptera Chrysomelidae Pachybrachis undet", 
                      "Coleoptera Chrysomelidae Pachybrachis undet sp2.(black/white)", "Coleoptera Chrysomelidae Pachybrachis undet sp3.(red/black)") ~ "Coleoptera Chrysomelidae Pachybrachis sp.",
    taxon_name %in% c("Coleoptera Curculionidae Sitona flavescens", "Coleoptera Curculionidae Sitona undet") ~ "Coleoptera Curculionidae Sitona sp.",
    taxon_name %in% c("Coleoptera Elateridae Ctenicera undet", "Coleoptera Elateridae undet undet (large)") ~ "Coleoptera Elateridae sp.",
    taxon_name %in% c("Coleoptera Helodidae Cyphon obscurus?", "Coleoptera Helodidae Cyphon padi?",
                      "Coleoptera Helodidae Cyphon undet", "Coleoptera Helodidae Cyphon variabilis") ~ "Coleoptera Helodidae Cyphon sp.",
    taxon_name %in% c("Collembola undet undet undet", "Collembola undet undet undet (black.hunchback)",
                      "Collembola undet undet undet (globular)") ~ "Collembola sp.",
    taxon_name %in% c("Diptera Bombyliidae Phthiria undet", "Diptera Bombyliidae Phthiria undet sp1.(yellow)", 
                      "Diptera Bombyliidae Phthiria undet sp2.(green/yellow)", "Diptera Bombyliidae Phthiria undet sp3.(black)") ~ "Diptera Bombyliidae Phthiria sp.",
    taxon_name %in% c("Diptera Bombyliidae Villa lateralis", "Diptera Bombyliidae Villa undet") ~ "Diptera Bombyliidae Villa sp.",
    taxon_name %in% c("Diptera Cecidomyiidae undet undet", "Diptera Cecidomyiidae undet undet (orange)",
                      "Diptera Cecidomyiidae undet undet sp1.(large)", "Diptera Cecidomyiidae undet undet sp3.(red)") ~ "Diptera Cecidomyiidae sp.",
    taxon_name %in% c("Diptera Ceratopogonidae undet undet", "Diptera Ceratopogonidae undet undet sp1.(blue)",
                      "Diptera Ceratopogonidae undet undet sp2.(black)", "Diptera Ceratopogonidae undet undet sp4.(orange)",
                      "Diptera Ceratopogonidae undet undet sp7.(large)", "Diptera Ceratopogonidae undet undet sp8.") ~ "Diptera Ceratopogonidae sp.",
    taxon_name %in% c("Diptera Chironomidae undet undet", "Diptera Chironomidae undet undet .sp4.(green)",
                      "Diptera Chironomidae undet undet sp2.(black)", "Diptera Chironomidae undet undet sp3.(black/yellow.leg)",
                      "Diptera Chironomidae undet undet sp5.", "Diptera Chironomidae undet undet sp6.(large)") ~ "Diptera Chironomidae sp.",
    taxon_name %in% c("Diptera Drosophilidae Drosophila undet", "Diptera Drosophilidae Opomyza? undet (shiny)",
                      "Diptera Drosophilidae Scaptomyza undet") ~ "Diptera Drosophilidae sp.",
    taxon_name %in% c("Diptera Empididae Drapetus? undet (yellow.leg)", "Diptera Empididae Hybos? undet (humpback)",
                      "Diptera Empididae Platypalpus undet", "Diptera Empididae undet undet") ~ "Diptera Empididae sp.",
    taxon_name %in% c("Diptera Heleomyzidae Heleomyza undet", "Diptera Heleomyzidae Tephrochlamys undet",
                      "Diptera Heleomyzidae undet undet") ~ "Diptera Heleomyzidae sp.",
    taxon_name %in% c("Diptera Milichiidae Eusiphona undet", "Diptera Milichiidae Phyllomyza undet",
                      "Diptera Milichiidae undet undet") ~ "Diptera Milichiidae sp.",
    taxon_name %in% c("Diptera Muscidae Coenosia undet", "Diptera Muscidae Coenosia undet sp1.(other)",
                      "Diptera Muscidae Coenosia undet sp2.(large)", "Diptera Muscidae Coenosia undet sp3.(yellow.legs)") ~ "Diptera Muscidae Coenosia sp.",
    taxon_name %in% c("Diptera Phoridae undet undet", "Diptera Phoridae undet undet sp2.(dark)",
                      "Diptera Phoridae undet undet sp3.(pale)") ~ "Diptera Phoridae sp.",
    taxon_name %in% c("Diptera Sarcophagidae Blaesoxipha(Acridiophaga) undet", "Diptera Sarcophagidae Senotainia undet",
                      "Diptera Sarcophagidae undet undet") ~ "Diptera Sarcophagidae sp.",
    taxon_name %in% c("Diptera Sphaeroceridae Leptocera undet", "Diptera Sphaeroceridae Leptocera(Pteremis) undet (small)",
                      "Diptera Sphaeroceridae Leptocera(Rachispoda) undet (pale)") ~ "Diptera Sphaeroceridae Leptocera sp.",
    taxon_name %in% c("Diptera Syrphidae Lejops bilineatus", "Diptera Syrphidae Lejops undet") ~ "Diptera Syrphidae Lejops sp.",
    taxon_name %in% c("Diptera Syrphidae Paragus bicolor", "Diptera Syrphidae Paragus undet") ~ "Diptera Syrphidae Paragus sp.",
    taxon_name %in% c("Diptera Tephritidae Urophora undet", "Diptera Tephritidae Urophora undet sp2.(other)") ~ "Diptera Tephritidae Urophora sp.",
    taxon_name %in% c("Hemiptera Alydidae Alydus conspersus", "Hemiptera Alydidae Alydus eurinus",
                      "Hemiptera Alydidae Alydus undet") ~ "Hemiptera Alydidae Alydus sp.",
    taxon_name %in% c("Hemiptera Lygaeidae Cymus discors", "Hemiptera Lygaeidae Cymus undet") ~ "Hemiptera Lygaeidae Cymus sp.",
    taxon_name %in% c("Hemiptera Lygaeidae Geocoris bullatus", "Hemiptera Lygaeidae Geocoris undet") ~ "Hemiptera Lygaeidae Geocoris sp.",
    taxon_name %in% c("Hemiptera Lygaeidae Slaterobius insignis", "Hemiptera Lygaeidae Sphaerobius insignis") ~ "Hemiptera Lygaeidae Slaterobius insignis",
    taxon_name %in% c("Hemiptera Pentatomidae Euschistus servus(euschistoides)", "Hemiptera Pentatomidae Euschistus tristigmus",
                      "Hemiptera Pentatomidae Euschistus undet", "Hemiptera Pentatomidae Euschistus variolarius") ~ "Hemiptera Pentatomidae Euschistus sp.",
    taxon_name %in% c("Hemiptera Pentatomidae Homaemus bijugis", "Hemiptera Pentatomidae Homaemus undet") ~ "Hemiptera Pentatomidae Homaemus sp.",
    taxon_name %in% c("Homoptera Cicadellidae Chlorotettix undet", "Homoptera Cicadellidae Chlorotettix unicolor") ~ "Homoptera Cicadellidae Chlorotettix sp.",
    taxon_name %in% c("Homoptera Cicadellidae Deltocephalus flavicosta", "Homoptera Cicadellidae Deltocephalus gnarus",
                      "Homoptera Cicadellidae Deltocephalus undet") ~ "Homoptera Cicadellidae Deltocephalus sp.",
    taxon_name %in% c("Homoptera Cicadellidae Phlepsius irroratus", "Homoptera Cicadellidae Phlepsius undet") ~ "Homoptera Cicadellidae Phlepsius sp.",
    taxon_name %in% c("Homoptera Delphacidae Delphacodes campestris", "Homoptera Delphacidae Delphacodes undet") ~ "Homoptera Delphacidae Delphacodes sp.",
    taxon_name %in% c("Homoptera Dictyopharidae Scolops angustatus", "Homoptera Dictyopharidae Scolops desiccatus",
                      "Homoptera Dictyopharidae Scolops sulcipes", "Homoptera Dictyopharidae Scolops undet") ~ "Homoptera Dictyopharidae Scolops sp.",
    taxon_name %in% c("Homoptera Membracidae Stictocephala basalis", "Homoptera Membracidae Stictocephala lutea",
                      "Homoptera Membracidae Stictocephala undet") ~ "Homoptera Membracidae Stictocephala sp.",
    taxon_name %in% c("Hymenoptera Bethylidae Pristocera armifera", "Hymenoptera Bethylidae undet undet",
                      "Hymenoptera Bethylidae undet undet sp1", "Hymenoptera Bethylidae undet undet sp2") ~ "Hymenoptera Bethylidae sp.",
    taxon_name %in% c("Hymenoptera Braconidae Apanteles undet", "Hymenoptera Braconidae Apanteles(Glyptapanteles) undet") ~ "Hymenoptera Braconidae Apanteles sp.",
    taxon_name %in% c("Hymenoptera Braconidae Bracon undet", "Hymenoptera Braconidae Bracon undet sp1.(black)",
                      "Hymenoptera Braconidae Bracon undet sp2.(black/orange)", "Hymenoptera Braconidae Bracon undet sp3.(orange)") ~ "Hymenoptera Braconidae Bracon sp.",
    taxon_name %in% c("Hymenoptera Ceraphronidae Aphanogmus? undet (peeny,.broken.stigma)", "Hymenoptera Ceraphronidae undet undet",
                      "Hymenoptera Ceraphronidae undet undet sp3.") ~ "Hymenoptera Ceraphronidae sp.",
    taxon_name %in% c("Hymenoptera Eucharitidae undet undet", "Hymenoptera Eucharitidae undet undet sp1.(dark.brown)") ~ "Hymenoptera Eucharitidae sp.",
    taxon_name %in% c("Hymenoptera Eupelmidae undet undet", "Hymenoptera Eupelmidae undet undet sp1.(green.flat.back)",
                      "Hymenoptera Eupelmidae undet undet sp2.(brown.humpback)") ~ "Hymenoptera Eupelmidae sp.",
    taxon_name %in% c("Hymenoptera Halictidae Agapostemon splendens", "Hymenoptera Halictidae Agapostemon undet") ~ "Hymenoptera Halictidae Agapostemon sp.",
    taxon_name %in% c("Hymenoptera Halictidae Dialictus laevissimus", "Hymenoptera Halictidae Dialictus pilosus",
                      "Hymenoptera Halictidae Dialictus undet", "Hymenoptera Halictidae Dialictus vierecki") ~ "Hymenoptera Halictidae Dialictus sp.",
    taxon_name %in% c("Hymenoptera Halictidae Halictus confusus", "Hymenoptera Halictidae Halictus undet") ~ "Hymenoptera Halictidae Halictus sp.",
    taxon_name %in% c("Hymenoptera Platygasteridae Amblyaspis undet (spine)", "Hymenoptera Platygasteridae Inostemma undet",
                      "Hymenoptera Platygasteridae undet undet", "Hymenoptera Platygasteridae undet undet sp1.(small)", "Hymenoptera Platygasteridae undet undet sp2.(long.tail)",
                      "Hymenoptera Platygasteridae undet undet sp3.(skinny)", "Hymenoptera Platygasteridae undet undet sp4.(yellow.leg)") ~ "Hymenoptera Platygasteridae sp.",
    taxon_name %in% c("Hymenoptera Pompilidae Aporinellus undet", "Hymenoptera Pompilidae undet undet") ~ "Hymenoptera Pompilidae sp.",
    taxon_name %in% c("Hymenoptera Sphecidae Cerceris clypeata", "Hymenoptera Sphecidae Cerceris undet") ~ "Hymenoptera Sphecidae Cerceris sp.",
    taxon_name %in% c("Hymenoptera Sphecidae Philanthus lepidus", "Hymenoptera Sphecidae Philanthus undet") ~ "Hymenoptera Sphecidae Philanthus sp.",
    taxon_name %in% c("Hymenoptera Tiphiidae Myzinum carolinianum(collare)", "Hymenoptera Tiphiidae Myzinum maculata",
                      "Hymenoptera Tiphiidae Myzinum undet") ~ "Hymenoptera Tiphiidae Myzinum sp.",
    taxon_name %in% c("Hymenoptera Torymidae undet undet (green)", "Hymenoptera Torymidae undet undet (green/orange)") ~ "Hymenoptera Torymidae sp.",
    taxon_name %in% c("Lepidoptera Lycaenidae Everes comyntas", "Lepidoptera Lycaenidae Everes undet (hairy)",
                      "Lepidoptera Lycaenidae Lycaena phlaeas(americana)", "Lepidoptera Lycaenidae undet undet") ~ "Lepidoptera Lycaenidae sp.",
    taxon_name %in% c("Lepidoptera Misc.Macrolepidoptera undet undet", "Lepidoptera Misc.Macrolepidoptera undet undet (black)",
                      "Lepidoptera Misc.Macrolepidoptera undet undet (brown)", "Lepidoptera Misc.Macrolepidoptera undet undet (green.hairy)",
                      "Lepidoptera Misc.Macrolepidoptera undet undet (green.prognathus)", "Lepidoptera Misc.Macrolepidoptera undet undet (spiny)") ~ "Lepidoptera Misc.Macrolepidoptera sp.",
    taxon_name %in% c("Lepidoptera Misc.Microlepidoptera undet undet", "Lepidoptera Misc.Microlepidoptera undet undet sp1.(plain)",
                      "Lepidoptera Misc.Microlepidoptera undet undet sp2.(bar)", "Lepidoptera Misc.Microlepidoptera undet undet sp3.(dark)") ~ "Lepidoptera Misc.Microlepidoptera sp.",
    taxon_name %in% c("Lepidoptera Pyralidae Crambus undet", "Lepidoptera Pyralidae Crambus undet (shield)", 
                      "Lepidoptera Pyralidae Crambus undet sp4.(brown/white)", "Lepidoptera Pyralidae Crambus undet sp5.(square)") ~ "Lepidoptera Pyralidae Crambus sp.",
    taxon_name %in% c("Neuroptera Chrysopidae Chrysopa oculata (+vars)", "Neuroptera Chrysopidae Chrysopa undet", 
                      "Neuroptera Chrysopidae undet undet") ~ "Neuroptera Chrysopidae sp.",
    taxon_name %in% c("Neuroptera Coniopterygidae Conwentzia undet", "Neuroptera Coniopterygidae undet undet") ~ "Neuroptera Coniopterygidae sp.",
    taxon_name %in% c("Odonata Libellulidae Sympetrum obtrusum", "Odonata Libellulidae Sympetrum undet",
                      "Odonata Libellulidae Sympetrum vicinum") ~ "Odonata Libellulidae Sympetrum sp.",
    taxon_name %in% c("Orthoptera Acrididae Arphia conspersa", "Orthoptera Acrididae Arphia pseudonietana",
                      "Orthoptera Acrididae Arphia undet") ~ "Orthoptera Acrididae Arphia sp.",
    taxon_name %in% c("Orthoptera Tettigoniidae Conocephalus brevipennis", "Orthoptera Tettigoniidae Conocephalus fasciatus",
                      "Orthoptera Tettigoniidae Conocephalus saltans", "Orthoptera Tettigoniidae Conocephalus strictus", 
                      "Orthoptera Tettigoniidae Conocephalus undet") ~ "Orthoptera Tettigoniidae Conocephalus sp.",
    taxon_name %in% c("Psocoptera undet undet undet", "Psocoptera undet undet undet sp1.(marked.wing)", "Psocoptera undet undet undet sp2.(clear.wing)",
                      "Psocoptera undet undet undet sp3.(brown)", "Psocoptera undet undet undet sp4.(wingless)", "Psocoptera undet undet undet sp5.(large)") ~ "Psocoptera sp.",
    taxon_name %in% c("Homoptera Issidae Bruchomorpha dorsata", "Homoptera Issidae Bruchomorpha nasuta", "Homoptera Issidae Bruchomorpha tristis", 
                      "Homoptera Issidae Bruchomorpha undet") ~ "Homoptera Issidae Bruchomorpha sp.",
    taxon_name %in% c("Hymenoptera Andrenidae Perdita perpallida(citrinella)", "Hymenoptera Andrenidae Perdita undet") ~ "Hymenoptera Andrenidae Perdita sp.",
    taxon_name %in% c("Hymenoptera Chalcididae Brachymeria undet", "Hymenoptera Chalcididae Spilochalcis undet", 
                      "Hymenoptera Chalcididae undet undet") ~ "Hymenoptera Chalcididae sp.",
    taxon_name %in% c("Hymenoptera Formicidae Pheidole pilifera", "Hymenoptera Formicidae Pheidole undet") ~ "Hymenoptera Formicidae Pheidole sp.",
    taxon_name %in% c("Hymenoptera Megachilidae Coelioxys undet", "Hymenoptera Megachilidae Megachile undet", 
                      "Hymenoptera Megachilidae Osmia undet", "Hymenoptera Megaspilidae undet undet") ~ "Hymenoptera Megachilidae sp.",
    taxon_name %in% c("Lepidoptera Arctiidae Holomelina aurantiaca", "Lepidoptera Arctiidae undet undet (hairy)") ~ "Lepidoptera Arctiidae sp.",
    taxon_name %in% c("Lepidoptera Tortricidae undet undet", "Lepidoptera Tortricidae undet undet (red.stripes)",
                      "Lepidoptera Tortricidae undet undet sp1", "Lepidoptera Tortricidae undet undet sp4", 
                      "Lepidoptera Tortricidae undet undet sp5") ~ "Lepidoptera Tortricidae sp.",
    .default = taxon_name
  )) %>%
  mutate(id_confidence = dplyr::case_when(
    taxon_name %in% c(
      "Araneae undet undet undet", "Coleoptera Coccinellidae undet undet", "Coleoptera Coccinellidae undet undet (hairy)", "Coleoptera Meloidae Epicauta undet",
      "Coleoptera undet Coleoptera undet", "Coleoptera undet undet undet", "Diptera Agromyzidae Agromyza undet", "Diptera Agromyzidae undet undet",
      "Diptera Anthomyiidae undet undet", "Diptera Chloropidae Hippelates undet", "Diptera Chloropidae Thaumatomyia undet", "Diptera Chloropidae undet undet",
      "Diptera Chloropidae undet undet (orange)", "Diptera Chloropidae undet undet sp1", "Diptera Dolichopodidae undet undet", "Diptera Dolichopodidae undet undet sp1",
      "Diptera Ephydridae undet undet", "Diptera Ephydridae undet undet sp2", "Diptera Muscidae undet undet", "Diptera Muscidae undet undet (other)",
      "Diptera Pipunculidae undet undet", "Diptera Sciomyzidae undet undet", "Diptera Syrphidae undet undet", "Diptera Tachinidae undet undet", "Hemiptera Miridae Lygus undet",
      "Hemiptera Miridae undet undet", "Hemiptera Pentatomidae undet undet", "Hemiptera undet undet undet", "Homoptera Cicadellidae Scleroracus undet",
      "Homoptera Cicadellidae undet undet", "Hymenoptera Braconidae undet undet", "Hymenoptera Eulophidae undet undet", "Hymenoptera Ichneumonidae undet undet",
      "Hymenoptera Pteromalidae undet undet", "Lepidoptera Noctuidae undet undet", "Lepidoptera Pyralidae undet undet", "Lepidoptera undet undet undet",
      "na na na na", "na? na? na? na? na?", "none none none none none", "unk unk unk unk unk"
    ) ~ 0,
    .default = 1
  ))


cdr_consumer <- cdr_consumer %>%
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
         abundance = Count, year = Year,
         month = Month, day = NA) %>% # renaming columns
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "taxon_name", "abundance", 
                  "unit_abundance", "scale_abundance", "id_confidence"))


write.csv(cdr_consumer,"cdr_consumer.csv", row.names = FALSE) # exporting csv

googledrive::drive_upload(media = file.path("cdr_consumer.csv"), overwrite = T, # exporting to google drive
                          path = googledrive::as_id("https://drive.google.com/drive/u/1/folders/1UPaIm6Tp8aQH0gUrOCEz8tV_mkArQkhw"))










