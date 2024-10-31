#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Kelsey Solomon
#### Last Updated: October 31st, 2024



#####PIE VEGETATION DATASET HARMONIZATION###########

#######Notes about this dataset
##McH and EPH likely need to be taken out because they were hayed until 2002
##PUH and CC are reference sites and are fine
##RM and RR are likely fine too.. originally set up to track invasion by Phragmites austrails
##Couldn't find quadrat size, need to look into methods because it was not in the metadata

# Define URL as an object
dt_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-pie.25.11&entityid=bedfe532c6e5d98caf9f54c44ff609c5"

# Read it into R
pie_producer_long <- read.csv(file = dt_url)

##Convert to long format using column numbers
pie_producer_long <- pivot_longer(
  pie_producer_wide,
  cols = 7:51,                                 
  names_to = "species",
  values_to ="abundance")


##Average reps across subplots
pie_producer_summary <-aggregate(abundance~species+Date+Site+Transect,data=pie_producer_long,mean)


##Renaming column titles for harmonization
pie_producer_summary2<- pie_producer_summary %>% 
  rename("year"="Date",
         "plot"="Site",
         "subplot"="Transect")

##Adding columns for dataset info and harmonization
pie_producer_summary2$site="pie"
pie_producer_summary2$habitat="saltmarsh"
pie_producer_summary2$guild="vegetation"
pie_producer_summary2$unitAbund="mean.percent.cover"
pie_producer_summary2$scaleAbund="1_m2"  ##Need to double check quadrat size! 

##Creating uniqueID
pie_producer_summary2$uniqueID<- paste(pie_producer_summary2$site, 
                                       pie_producer_summary2$habitat,
                                       pie_producer_summary2$plot,
                                       pie_producer_summary2$subplot,
                                       sep = "_")

##Rearrange columns to correct order
pie_producer_final <-pie_producer_summary2 %>% 
                    select(year, 
                           site, 
                           habitat,
                           plot,
                           subplot,
                           uniqueID,
                           guild,
                           species,
                           abundance,
                           unitAbund,
                           scaleAbund)

##Write csv
write.csv(pie_producer_final, "pie_producer_final.csv")






##############PIE CONSUMER DATASET HARMONIZATION############

#######Notes about this dataset
##Only RM for birds, so maybe for veg we can cut out all plots but RM
### should we convert count data to rel abundance?




# Define URL as an object
dt_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-pie.25.11&entityid=bedfe532c6e5d98caf9f54c44ff609c5"

# Read it into R
pie_consumer_long <- read.csv(file = dt_url)

#pie_consumer_long<-read.csv("PIE_BIRDS.csv")



#Extracting year from date
pie_consumer_long$year <- substr(pie_consumer_long$DATE, nchar(pie_consumer_long$DATE) - 1, 
                                 nchar(pie_consumer_long$DATE))

pie_consumer_long$year2<-paste("20",pie_consumer_long$year,sep="")


##Combining genus and species
pie_consumer_long$Species2<- paste(pie_consumer_long$Genus, 
                             pie_consumer_long$Species,
                             sep = "_")


##Average reps across -- in this case, counts are done over 3 days in the same year (each day is a rep)
pie_consumer_summary <-aggregate(IN_Total~Species2+year2+Sanc_Code+CIRCLE,data=pie_consumer_long,sum)  #sum or mean?


##Renaming column titles for harmonization
pie_consumer_summary2<- pie_consumer_summary %>% 
  rename("year"="year2",
         "plot"="Sanc_Code",
         "subplot"="CIRCLE",
         "species"="Species2",
         "abundance"="IN_Total")


##Adding columns for dataset info and harmonization
pie_consumer_summary2$site="pie"
pie_consumer_summary2$habitat="saltmarsh"
pie_consumer_summary2$guild="birds"
pie_consumer_summary2$unitAbund="count"
pie_consumer_summary2$scaleAbund="100_m_radius"  


##Creating uniqueID
pie_consumer_summary2$uniqueID<- paste(pie_consumer_summary2$site, 
                                       pie_consumer_summary2$habitat,
                                       pie_consumer_summary2$plot,
                                       pie_consumer_summary2$subplot,
                                       sep = "_")

##Rearrange columns to correct order
pie_consumer_final <-pie_consumer_summary2 %>% 
  select(year, 
         site, 
         habitat,
         plot,
         subplot,
         uniqueID,
         guild,
         species,
         abundance,
         unitAbund,
         scaleAbund)

##Write csv
write.csv(pie_consumer_final, "pie_consumer_final.csv")


