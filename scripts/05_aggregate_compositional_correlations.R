rm(list = ls())

library(ggplot2)
source(here::here("scripts", "00_functions.r"))
# read data
drive_folder <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1xa-ypKd_ovsRov_Ol_uvE7ZI2rCc5SSj"), type='csv')
tmp <- tempfile(fileext = ".csv")
download <- drive_download(drive_folder[drive_folder$name=="master_stability.csv",], path = tmp, overwrite = TRUE)
master <- read.csv(tmp)

# z-standardize
stability_cols <- c("prod_stability", "prod_comp_stability", "con_stability", "con_comp_stability", "prod_jaccard_stability", "con_jaccard_stability")
master_z <- master %>%
  group_by(site) %>%
  mutate(across(all_of(stability_cols), ~ as.numeric(z_standard(.x)))) %>%
  ungroup()


cor.test(master_z$prod_comp_stability,
         master_z$prod_stability, method = "kendall") # p-value = 4.112e-06, tau = 0.208

(prod_stability_metric_corr <-
  ggplot() +
  geom_point(data = master, aes(x = prod_comp_stability, y = prod_stability, colour = site, shape = ecosystem), size = 2.5) +
  geom_smooth(data = master, aes(x = prod_comp_stability, y = prod_stability, colour = site), 
              method = lm, se = F) +
  geom_smooth(data = master, aes(x = prod_comp_stability, y = prod_stability ), 
                method = lm, se = T, colour = "black") +
  labs(x = "Compositional stability", y = "Aggregate stability", title = "a. Producer stability \n") +
  theme_classic() 
    
)


cor.test(master_z$con_comp_stability,
         master_z$con_stability, method = "kendall") # p-value = 1.031e-11, tau = 0.307

(con_stability_metric_corr <-
    ggplot() +
    geom_point(data = master_z, aes(x = con_comp_stability, y = con_stability, colour = site, shape = ecosystem), size = 2.5) +
    geom_smooth(data = master_z, aes(x = con_comp_stability, y = con_stability, colour = site), 
                method = lm, se = F) +
    geom_smooth(data = master_z, aes(x = con_comp_stability, y = con_stability ), 
                method = lm, se = T, colour = "black") +
    labs(x = "Compositional stability", y = "Aggregate stability", title = "b. Consumer stability \n") +
    theme_classic() 
  
)

