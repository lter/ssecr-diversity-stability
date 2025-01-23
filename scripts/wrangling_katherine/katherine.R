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
  mutate(plot = Replicate, subplot = Station, abundance = Adults, species = Species, year = Year, # renaming columns
         Replicate = NULL, Station = NULL, Adults = NULL, Species = NULL, Year = NULL) %>% # removing old columns
  mutate(abundance = as.numeric(abundance)) %>%
  mutate(year = as.double(year)) %>%
  mutate(unique_ID = paste(site, plot, subplot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

kbs_consumer <- kbs_consumer %>%
  mutate(id_confidence = if_else( # adding confidence column for species ID -- not going to include species IDed as "Other"
    species %in% c("Other"), 0, 1
  )) %>%
  mutate(herbivore = dplyr::case_when( # classifying feeding group
    species %in% c(
      "Adalia bipunctata", "Brachiacantha ursina", "Chilocorus stigma",
      "Coccinella septempunctata", "Coccinella trifaciata", "Coleomegilla maculata",
      "Cycloneda munda", "Harmonia axyridis", "Hippodamia convergens", "Hippodamia glacialis",
      "Hippodamia parenthesis", "Hippodamia tredecimpunctata", "Hippodamia variegata", 
      "Propylea quatuorodecipuntata"
    ) ~ "predator",
    species %in% c( # these are true omnivores, but include species in the families that have different feeding modes
      "Cantharid", "Carabid", "Lacewing", "Lampyrid", "Mecoptera",
      "Psyllobora vigintimaculata" # consumes primarily fungus
    ) ~ "omnivore",
    species %in% c(
      "Syrphid"
    ) ~ "nectarivorous"
  )) %>%
  separate(species, sep = " ", into = c("genus", "species")) # separating into genus and species column


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
  mutate(plot = Replicate, subplot = Station, abundance = Biomass_g, species = Species, year = Year, # renaming columns
         Replicate = NULL, Station = NULL, Biomass_g = NULL, Species = NULL, Year = NULL) %>% # removing old columns
  mutate(abundance = as.numeric(abundance)) %>%
  mutate(year = as.double(year)) %>%
  mutate(unique_ID = paste(site, plot, subplot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance"))

kbs_producer$species <- str_trim(kbs_producer$species, side = "both") # removing white space from strings

kbs_producer <- kbs_producer %>%
  mutate(id_confidence = dplyr::case_when(
    species %in% c(
      "another unknown dicot", "unknown Asteraceae", "unknown Brassicaceae", "unknown Sedge",
      "Unknown dicot", "Unknown grass", "Unknown monocot", "Unkown Fabaceae", "UnSorted",
      "Composite Basal", "Composite sp.", "Dicots", "Monocots", "Unknown Orchidaceae", 
      "Composite Basal Leaves", "Aster basal leaves", 
      "Aster sp.", "Brassica sp.", "Bromus sp.", "Carex sp.", "Celastrus sp.", "Cerastium sp.", "Cirsium sp.", "Cornus sp.", # genus classifications
      "Desmodium sp.", "Erigeron sp.", "Festuca sp.", "Fragaria sp.", "Geum sp.", "Hieracium sp.", "Juncus sp.", "Lactuca sp.", "Lonicera spp.",
      "Melilotus sp.", "Monarda sp.", "Plantago sp.", "Poa sp.", "Populus sp.", "Prunus spp.", "Rhamnus sp.", "Rosa sp.",
      "Rubus sp.", "Rumex species", "Setaria sp.", "Solidago sp.", "Sonchus sp.", "Trifolium sp.", "Veronica sp.", "Vicia sp.", "Vitis sp."
    ) ~ 0,
    species %in% c(
      "Abutilon theophrasti Medikus", "Acer spp.", "Achillea millefolium L.", "Agrostis gigantea Roth",
      "Ambrosia artemisiifolia L.", "Antennaria plantaginifolia (L.) Richards.", "Anthemis cotula L.",
      "Apocynum androsaemifolium L.", "Apocynum cannabinum L.", "Arabidopsis thaliana (L.) Heynh.", 
      "Arabis glabra (L.) Bernh.", "Arctium minus (Hill) Bernh.", "Arrhenatherum elatius (L.) Beauv. ex J. & C. Presl",
      "Artemisia campestris ssp. Caudata (Michx.) Hall & Clements", "Artemisia vulgaris L.", "Asclepias syriaca L.",
      "Asparagus officinalis L.", "Asplenium platyneuron (L.) Oakes", "Aster cordifolius", "Aster ericoides L.",
      "Aster novae-angliae L.", "Aster pilosus Willd.", "Aster sagittifolius", "Barbarea vulgaris R. Br.", 
      "Botrychium dissectum", "Brassica kaber (DC.) L.C.Wheeler", "Brassica kaber (DC.) L.C.Wheeler", 
      "Brassica rapa L.", "Bromus inermis Leyss.", "Bromus japonicus Thunb. ex Murr.", "Bromus mollis L.", "Bromus tectorum L.",
      "Carex muhlenburgii", "Celastrus orbiculatus Thunb.", "Centaurea stoebe L. ssp. micranthos (Gugler) Hayek",
      "Cerastium arvense L.", "Cerastium vulgatum L.", "Chenopodium album L.", "Cirsium altissimum (L.) Spreng.", "Cirsium arvense (L.) Scop.",
      "Cirsium discolor (Muhl. ex Willd.) Spreng.", "Cirsium vulgare (Savi) Tenore", "Convolvulus arvensis L.", "Conyza canadensis (L.) Cronq.",
      "Cornus racemosa Lam.", "Crataegus spp.", "Crepis capillaris (L.) Wallr.", "Cyperus esculentus L.", "Dactylis glomerata L.",
      "Daucus carota L.", "Desmodium illinoense", "Desmodium marilandicum (L.) DC.", "Desmodium paniculatum (L.) DC.", "Dianthus armeria L.",
      "Diervilla sp.", "Digitaria sanguinalis (L.) Scop.", "Duchesnea indica (Andr.) Focke", "Echinochloa crus-galli (L.) Beauv.", 
      "Elaeagnus umbellata Thunb.", "Elymus canadensis", "Elymus repens (L.) Gould", "Epilobium coloratum Biehler.", "Eragrostis cilianensis (All.) E.Mosher", 
      "Erigeron annuus (L.) Pers.", "Erigeron strigosus Muhl. ex Willd.", "Euthamia graminifolia (L.) Nutt.", "Festuca rubra L.", 
      "Fragaria virginiana Duchesne", "Geum laciniatum", "Geum virginianum", "Gnaphalium obtusifolium L.", "Helianthus occidentalis Riddell", 
      "Hieracium aurantiacum L.", "Hieracium caespitosum", "Hypericum perforatum L.", "Juncus acuminatus Michx.", "Juncus tenuis Willd.", 
      "Lactuca canadensis L.", "Lactuca saligna L.", "Lactuca serriola L.", "Lonicera morrowii", "Lotus corniculatus L.", "Malus spp.", 
      "Medicago lupulina L.", "Medicago sativa L.", "Melilotus alba var. annua H.S.Coe", "Melilotus officinalis (L.) Lam.", 
      "Mimosa sp.", "Monarda fistulosa L.", "Oenothera biennis L.", "Oxalis stricta L.", "Panicum dichotomiflorum Michx.", "Parthenocissus quinquefolia (L.) Planch.",
      "Phalaris angusta Nees ex Trin.", "Phalaris arundinacea L.", "Phleum pratense L.", "Physalis heterophylla Nees", "Physalis longifolia  Nutt.",
      "Phytolacca americana L.", "Plantago lanceolata L.", "Plantago major L.", "Plantago rugelii Dcne.", "Poa annua L.", "Poa compressa L.", "Poa pratensis L.",
      "Polygonatum biflorum (Walt.) Ell.", "Polygonum convolvulus L.", "Polygonum pensylvanicum L.", "Populus deltoides", "Potentilla argentea L.", 
      "Potentilla arguta Pursh", "Potentilla arguta Pursh", "Potentilla norvegica L.", "Potentilla recta L.", "Potentilla reptans L.", 
      "Potentilla simplex Michx.", "Prunus serotina Ehrh.", "Prunus virginiana L.", "Ranunculus abortivus L.", "Rhamnus cathartica L.",
      "Rhamnus frangula L.", "Rhus glabra L.", "Rhus typhina L.", "Robinia pseudoacacia L.", "Rosa multiflora Thunb. ex Murr.", "Rubus allegheniensis T.C. Porter", "Rubus flagellaris",
      "Rubus occidentalis L.", "Rumex acetosella L.", "Rumex crispus L.", "Rumex obtusifolius L.", "Sassafras albidum (Nutt.) Nees", 
      "Setaria faberi Herrm.", "Setaria pumila (Poir.) Roem. & Schult", "Setaria viridis (L.) Beauv.", "Silene alba (Mill.) E.H.L.Krause",
      "Solanum sp.", "Solidago canadensis L.", "Solidago juncea Aiton", "Solidago nemoralis Ait.", "Solidago rugosa Ait.", "Solidago speciosa L.",
      "Sonchus arvensis L.", "Sonchus asper (L.) Hill", "Sonchus oleraceus L.", "Stellaria media (L.) Vill.", "Taraxacum officinale Weber in", 
      "Toxicodendron radicans (L.) Ktze.", "Trifolium agrarium", "Trifolium arvense L.", "Trifolium campestre Schreb.", "Trifolium dubium Sibth.",
      "Trifolium hybridum L.", "Trifolium pratense L.", "Trifolium repens L.", "Verbascum blattaria L.", "Verbascum thapsus L.", "Veronica arvensis L.",
      "Veronica peregrina L.", "Vicia villosa  Roth", "Vitex agnus-castus L.", "Vitis aestivalis Michx.", "Vitis riparia Michx.", "Xanthium spinosum L."
    ) ~ 1
  )) %>%
  mutate(genus = word(species, 1, sep=" ")) %>% # subsetting first word of name as genus
  mutate(species = word(species, 2, sep=" ")) %>% # subsetting second word of name as genus
  select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
           "plot", "subplot", "year", "month", "unique_ID", "genus", "species", "id_confidence", "abundance", 
           "unit_abundance", "scale_abundance")) # reordering columns


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
  mutate(genus_species = paste(genus, species, sep = " ")) %>% # temporarily pasting together genus and species for assigning ID confidence
  mutate(id_confidence = if_else( # adding confidence column for species ID 
    genus_species %in% c("annual forb"), 0, 1
  )) %>%
  dplyr::select(!c("genus_species"))

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
       abundance = TOTAL, species = SPECIES, year = RECYEAR,
       month = RECMONTH, day = RECDAY) %>% # renaming columns
  mutate(plot = tolower(plot)) %>% # lowercasing plot to be consistent with producer
  mutate(unique_ID = paste(site, habitat_fine, plot, sep = "_")) %>% # adding unique ID that matches producer dataset
  dplyr::select(c("site", "taxa_type", "ecosystem", "habitat_broad", "habitat_fine", "biome", "guild", 
                  "plot", "subplot", "year", "month", "day", "unique_ID", "species", "abundance", 
                  "unit_abundance", "scale_abundance", "SPCODE"))

knz_consumer$abundance <- str_replace(knz_consumer$abundance, "1 01", "101") # fixing abundance typo
knz_consumer <- knz_consumer %>%
  filter(!abundance %in% c("", "0")) %>% # removing 0 and NA for abundance
  mutate(abundance = as.numeric(abundance)) # converting abundance to numeric

  

knz_consumer <- knz_consumer %>%
  mutate(species = dplyr::case_when(
    species %in% c("Brachystola magna", "brachystol magna") ~ "Brachystola magna",
    species %in% c("Schistocerca lineata", "schistocer lineata") ~ "Schistocerca lineata",
    species %in% c("Paratfylotropidia brunneri", "paratylota brunneri", "paratylotr brunneri") ~ "Paratylotropidia brunneri",
    species %in% c("Hypochlora alba", "hypochlora alba") ~ "Hypochlora alba",
    species %in% c("Campylacantha olivacea", "campylacan olivacea") ~ "Campylacantha olivacea",
    species %in% c("Hesperotettix speciosus", "hesperotet speciosus") ~ "Hesperotettix speciosus",
    species %in% c("Hesperotettix viridis", "hesperotet viridis") ~ "Hesperotettix viridis",
    species %in% c("Hesperotettix spp.", "hesperotet species", "hesperotet spp.") ~ "Hesperotettix spp.",
    species %in% c("Phoetaliotes nebrascensis", "phoetaliot nebrascen") ~ "Phoetaliotes nebrascensis",
    species %in% c("Melanoplus scudderi", "melanoplus scudderi") ~ "Melanoplus scudderi",
    species %in% c("Melanoplus sanguinipes", "melanoplus sanguinip") ~ "Melanoplus sanguinipes",
    species %in% c("Melanoplus femurrubrum", "melanoplus femurrubr") ~ "Melanoplus femurrubrum",
    species %in% c("Melanoplus keeleri", "melanoplus keeleri") ~ "Melanoplus keeleri",
    species %in% c("Melanoplus packardii", "melanoplus packardii") ~ "Melanoplus packardii",
    species %in% c("Melanoplus differentialis", "melanoplus different") ~ "Melanoplus differentialis",
    species %in% c("Melanoplus bivittatus", "melanoplus bivittatu") ~ "Melanoplus bivittatus",
    species %in% c("Melanoplus confusus", "melanoplus confusus") ~ "Melanoplus confusus",
    species %in% c("Melanoplus spp.", "melanoplus species", "melanoplus spp.") ~ "Melanoplus spp.",
    species %in% c("Eritettix simplex", "eritettix simplex") ~ "Eritettix simplex",
    species %in% c("Syrbula admirabilis", "syrbula admirabil") ~ "Syrbula admirabilis",
    species %in% c("Orphulella speciosa", "orphulella speciosa", "orphullela speciosa") ~ "Orphulella speciosa",
    species %in% c("Mermiria picta", "mermiria picta") ~ "Mermiria picta",
    species %in% c("Mermiria bivittata", "mermiria bivitatta", "mermiria bivittata") ~ "Mermiria bivittata",
    species %in% c("Opeia obscura", "opeia obscura") ~ "Opeia obscura",
    species %in% c("Pseuodopomala brachyptera", "pseudopoma brachypte") ~ "Pseuodopomala brachyptera",
    species %in% c("Boopedon auriventris", "boopedon auriventr") ~ "Boopedon auriventris",
    species %in% c("Boopedon nubilum", "boopedon nubilum") ~ "Boopedon nubilum",
    species %in% c("Boopedon gracile", "boopedon gracile") ~ "Boopedon gracile",
    species %in% c("Ageneotettix deorum", "ageneotett deorum") ~ "Ageneotettix deorum",
    species %in% c("Mermiria spp.", "mermiria species", "mermiria spp.") ~ "Mermiria spp.",
    species %in% c("Chortophaga viridifasciata", "chortophag viridifas") ~ "Chortophaga viridifasciata",
    species %in% c("Arphia xanthoptera", "arphia xanthopte") ~ "Arphia xanthoptera",
    species %in% c("Arphia simplex", "arphia simplex") ~ "Arphia simplex",
    species %in% c("Arphia conspersa", "arphia conspersa") ~ "Arphia conspersa",
    species %in% c("Hadrotettix trifasciatus", "hadrotetti trifascia") ~ "Hadrotettix trifasciatus",
    species %in% c("Hippiscus rugosus", "hippiscus rugosus") ~ "Hippiscus rugosus",
    species %in% c("Pardalophora haldemani", "pardalopho haldemani") ~ "Pardalophora haldemani",
    species %in% c("Arphia spp.", "arphia species", "arphia spp.") ~ "Arphia spp.",
    species %in% c("Schistocerca obscura", "schistocer obscura") ~ "Schistocerca obscura",
    species %in% c("Encoptolophus sordidus", "encoptolop sordidus") ~ "Encoptolophus sordidus",
    species %in% c("Melanoplus angustipennis", "melanoplus angustipe") ~ "Melanoplus angustipennis",
    species %in% c("Xanthippus corallipes", "xanthippus corallipe") ~ "Xanthippus corallipes",
    species %in% c("Encoptolophus subgracilis", "encoptolop subgracil") ~ "Encoptolophus subgracilis",
    species %in% c("Encoptolphus spp.", "encoptolop spp.") ~ "Encoptolophus spp.",
    species %in% c("melanoplus foedus") ~ "Melanoplus foedus",
    species %in% c("melanoplus occidenta") ~ "Melanoplus occidenta",
    species %in% c("oedipodinae", "Oedipodinae spp.") ~ "Oedipodinae spp.",
    species %in% c("pardalopho apiculata") ~ "Pardalopho apiculata",
    species %in% c("pardalopho species", "pardalopho spp.") ~ "Pardalopho species",
    species %in% c("psoloessa delicatul") ~ "Psoloessa delicatul",
    .default = as.character(species)
  )) %>%
  mutate(herbivore = "herbivore") %>%
  mutate(id_confidence = dplyr::case_when(
    species %in% c(
      "unknown ", "unknown", "Unknown", "Boopedon spp.", "Hadrotettix spp.",
      "Melanoplus spp.", "Oedipodinae spp."
    ) ~ 0,
    .default = 1
  )) # STILL HAVE SOME TO GROUP AT GENUS LEVEL!!
# SPLIT SPECIES INTO SPECIES AND GENUS


write.csv(knz_consumer,"knz_consumer.csv", row.names = FALSE) # exporting csv




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










