# install.packages(librarian)
librarian::shelf(tidyr)

# read data
data_url <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.437.2&entityid=d9c5d73d11fc6a16ec41139b63b27751"
data_long <- read.csv(file = data_url) # should be 217512 obs, 11 var
length(unique(data_long$species)) # 298 species in the dataset

# subset guilds
algae_long <- subset(data_long, guild == "algae")
coral_long <- subset(data_long, guild == "coral")
invert_long <- subset(data_long, guild == "sessile.invert")

# pivot wide
data_wide <- data_long %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0))

algae_wide <- algae_long %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0))

coral_wide <- coral_long %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0))

invert_wide <- invert_long %>%
  pivot_wider(names_from = species, values_from = abundance, values_fill = list(abundance = 0))