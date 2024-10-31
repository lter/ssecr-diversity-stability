#### Project Information ----

## ------------------------------------------ ##
#         SSECR Diversity-Stability
# 
## ------------------------------------------ ##

#### Author(s): Kelsey Solomon
#### Last Updated: October 31st, 2024



#####PIE VEGETATION DATASET HARMONIZATION

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


