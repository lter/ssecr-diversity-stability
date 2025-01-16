#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Kelsey Solomon


setwd("//Users//kelseysolomon//Desktop//SSECR///Data")

library(lme4)
library(reshape2)
library(reshape)
library(vegan)
library(plyr)
library(Rmisc)
library(ggplot2)
library(GLMMadaptive)
library(lmerTest)
library(labdsv)
library(indicspecies)
library(tidyverse)
library(betapart)
library(TITAN2)
library(optimos.prime)
library(plotly)
library(tibble)
library(e1071)
library(rioja)
library(tidyr)
library(googledrive)
library(summarytools)

## ------------------------------------------ ##
#         PIE Producers Dataset Harmonization
# 
## ------------------------------------------ ##

#############Notes about this dataset######################
##URL: https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-pie.25.11
##McH and EPH likely need to be taken out because they were hayed until 2002
##PUH and CC are reference sites and are fine
##RM and RR are likely fine too.. originally set up to track invasion by Phragmites austrails
##Couldn't find quadrat size, need to look into methods because it was not in the metadata




######Main table harmonization

##Define URL as an object
dt_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-pie.25.11&entityid=bedfe532c6e5d98caf9f54c44ff609c5"

##Read data into R
pie_producers_raw <- read.csv(file = dt_url)

##Convert data table to long format using column numbers
pie_producers_long <- pivot_longer(
  pie_producers_raw,
  cols = 7:51,                                 
  names_to = "species",
  values_to ="abundance")

##Average replications across subplots
pie_producers_summary <-aggregate(abundance~species+Date+Site+Transect+Distance,data=pie_producers_long,mean)

##Renaming column titles 
pie_producers_summary2<- pie_producers_summary %>% 
  rename("YMD"="Date",
         "plot"="Transect",
         "subplot"="Distance",
         "genus.species"="species")

##Adding columns for harmonization
pie_producers_summary2$site="PIE"
pie_producers_summary2$taxa_type="producer"
pie_producers_summary2$ecosystem="aquatic"
pie_producers_summary2$habitat_broad="saltmarsh"
pie_producers_summary2$habitat_fine="saltmarsh"
pie_producers_summary2$biome="temperate"
pie_producers_summary2$guild="plant"
pie_producers_summary2$herbivore="no"
pie_producers_summary2$unit_abundance="mean.percent.cover"
pie_producers_summary2$scale_abundnace="1m2"  ##Need to double check quadrat size! 

#Date columns
pie_producers_summary2$year <- format(as.Date(pie_producers_summary2$YMD, format = "%Y-%m-%d"), "%Y")
pie_producers_summary2$month <- format(as.Date(pie_producers_summary2$YMD, format = "%Y-%m-%d"), "%m")
pie_producers_summary2$day <- format(as.Date(pie_producers_summary2$YMD, format = "%Y-%m-%d"), "%d")

##genus and species columns
pie_producers_summary2$genus <- sub("\\..*", "", pie_producers_summary2$genus.species)  # Extract text before the period
pie_producers_summary2$species <- ifelse(grepl("\\.", pie_producers_summary2$genus.species),
                                         sub(".*\\.", "", pie_producers_summary2$genus.species),
                                         "")

##unique_ID column
pie_producers_summary2$unique_ID<- paste(pie_producers_summary2$site, 
                                         pie_producers_summary2$habitat_fine,
                                         pie_producers_summary2$plot,
                                         sep = "_")

##Adding "sp." to species column if no species is listed
pie_producers_summary2$species[pie_producers_summary2$species == ""] <- "sp."


######Taxa table creation

##Creating dataframe for genus.species for taxa table
#column genus.species only
pie_producers_taxa_list<- pie_producers_summary2 %>% distinct(genus.species)

#all columns
#pie_producers_taxa_list <- pie_producers_summary2 %>% distinct(genus.species, .keep_all = TRUE)


##Creating id_confidence column
pie_producers_taxa_list$id_confidence<-"1"

##Marking id_confidence as "0" for taxa where ID is not confident
pie_producers_taxa_list$id_confidence[pie_producers_taxa_list$genus.species == "Algal.mat"] <- 0
pie_producers_taxa_list$id_confidence[pie_producers_taxa_list$genus.species == "Bare.ground"] <- 0
pie_producers_taxa_list$id_confidence[pie_producers_taxa_list$genus.species == "Ditch"] <- 0
pie_producers_taxa_list$id_confidence[pie_producers_taxa_list$genus.species == "Unknown"] <- 0
pie_producers_taxa_list$id_confidence[pie_producers_taxa_list$genus.species == "Wrack"] <- 0
pie_producers_taxa_list$id_confidence[pie_producers_taxa_list$genus.species == "Creek"] <- 0



######Main table harmonization part 2

##Merging taxa table with main table
pie_producers_summary3<-merge(pie_producers_summary2, pie_producers_taxa_list, by="genus.species")


##Deleting rows with an abundance value >1
pie_producers_summary4<- pie_producers_summary3 %>%
  filter(abundance <= 1)

##Removing unnecessary columns
pie_producers_summary4$YMD <- NULL


##Rearrange columns to harmonized order
pie_producers <-pie_producers_summary3 %>% 
                    select(site, 
                           taxa_type, 
                           ecosystem,
                           habitat_broad,
                           habitat_fine,
                           biome,
                           guild,
                           herbivore,
                           year,
                           month,
                           day,
                           plot,
                           subplot,
                           unique_ID,
                           unit_abundance,
                           scale_abundnace,
                           genus,
                           species,
                           abundance,
                           id_confidence)



#####Writing csv files

##Main table
write.csv(pie_producers, "pie_producers.csv")

##Taxa table
write.csv(pie_producers_taxa_list, "pie_producers_taxa_list.csv")



######Uploading to google drive

##Main table
googledrive::drive_upload(media = file.path("pie_producers.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/16oEmL7Vf4WgjQCw4QacSa5Bl_wfVRCgH"))

##Taxa table
googledrive::drive_upload(media = file.path("pie_producers_taxa_list.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1JZI8mff4GlHt7AMkvcOz1oeVAuqqwY4o"))



######Looking at data
summarytools::view(summarytools::dfSummary(pie_producers), footnote = NA)


#####Creating summary page
#pie_producers_year<- pie_producers %>% distinct(year)
#pie_producers_month<- pie_producers %>% distinct(month)
#pie_producers_site<-pie_producers_raw %>% distinct(Site)




## ------------------------------------------ ##
#         PIE Consumers Dataset Harmonization
# 
## ------------------------------------------ ##

#############Notes about this dataset######################
##Only RM for birds, so maybe for veg we can cut out all plots but RM
### should we convert count data to rel abundance?



######Main table#######
##Define URL as an object
dt_url <- "https://pasta.lternet.edu/package/data/eml/knb-lter-pie/175/9/226e8eb36e07d85824c82c72a2f0c84d"

##Read data into R
pie_consumers_long <- read.csv(file = dt_url)

##Creating "year", "month", and "day" columns
pie_consumers_long$year <- format(as.Date(pie_consumers_long$DATE, format = "%Y-%m-%d"), "%Y")
pie_consumers_long$month <- format(as.Date(pie_consumers_long$DATE, format = "%Y-%m-%d"), "%m")
pie_consumers_long$day <- format(as.Date(pie_consumers_long$DATE, format = "%Y-%m-%d"), "%d")

##Combining genus and species into one column
#pie_consumers_long$genus.species<- paste(pie_consumers_long$Genus, 
       #                     pie_consumers_long$Species,
         #                    sep = ".")

##Convert to long format
pie_consumers_summary <-aggregate(IN_Total~Genus+Species+year+month+day+CIRCLE,data=pie_consumers_long,sum)  #sum or mean?

##Renaming column titles for harmonization
pie_consumers_summary2<- pie_consumers_summary %>% 
  rename("plot"="CIRCLE",
         "genus"="Genus",
         "species"="Species",
         "abundance"="IN_Total")


##Adding columns for dataset info and harmonization
pie_consumers_summary2$site="PIE"
pie_consumers_summary2$taxa_type="consumer"
pie_consumers_summary2$ecosystem="aquatic"
pie_consumers_summary2$habitat_broad="saltmarsh"
pie_consumers_summary2$habitat_fine="saltmarsh"
pie_consumers_summary2$biome="temperate"
pie_consumers_summary2$guild="bird"
pie_consumers_summary2$herbivore="no"
pie_consumers_summary2$unit_abundance="count"
pie_consumers_summary2$scale_abundnace="100_m_2"  

#subplot is the same as day because bird counts were conducted over 3 days in teh same month, each day representing a "rep"
pie_consumers_summary2$subplot<-pie_consumers_summary2$day 



##Creating column for genus.species 
pie_consumers_summary2$genus.species<-paste(pie_consumers_summary2$genus, 
                                                  pie_consumers_summary2$species,
                                                 sep = ".")

##Creating column for unique_ID
pie_consumers_summary2$unique_ID<- paste(pie_consumers_summary2$site, 
                                       pie_consumers_summary2$habitat,
                                       pie_consumers_summary2$plot,
                                       sep = "_")




######Taxa table creation

##Creating dataframe for genus.species for taxa table
#column genus.species only
pie_consumers_taxa_list<- pie_consumers_summary2 %>% distinct(genus.species)

##Creating id_confidence column
pie_consumers_taxa_list$id_confidence<-"1"

##Marking id_confidence as "0" for taxa where ID is not confident
##Not needed



######Main table harmonization part 2

##Merging taxa table with main table
pie_consumers_summary3<-merge(pie_consumers_summary2, pie_consumers_taxa_list, by="genus.species")


##Deleting rows with no taxa name
pie_consumers_summary4<- pie_consumers_summary3 %>%
  filter(genus.species != ".")

##Removing unnecessary columns
pie_consumers_summary4$genus.species <- NULL


##Rearrange columns to correct order
pie_consumers <-pie_consumers_summary4 %>% 
  select(site, 
         taxa_type, 
         ecosystem,
         habitat_broad,
         habitat_fine,
         biome,
         guild,
         herbivore,
         year,
         month,
         day,
         plot,
         subplot,
         unique_ID,
         unit_abundance,
         scale_abundnace,
         genus,
         species,
         abundance,
         id_confidence)


#####Writing csv files

##Main table
write.csv(pie_consumers, "pie_consumers.csv")

##Taxa table
write.csv(pie_consumers_taxa_list, "pie_consumers_taxa_list.csv")



######Uploading to google drive

##Main table
googledrive::drive_upload(media = file.path("pie_consumers.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/16oEmL7Vf4WgjQCw4QacSa5Bl_wfVRCgH"))

##Taxa table
googledrive::drive_upload(media = file.path("pie_consumers_taxa_list.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1JZI8mff4GlHt7AMkvcOz1oeVAuqqwY4o"))



#####Creating summary page
#pie_consumers_year<- pie_consumers %>% distinct(year)
#pie_consumers_month<- pie_consumers %>% distinct(month)
#pie_consumers_site<-pie_producers_raw %>% distinct(Site)


