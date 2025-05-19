rm(list = ls())

library(ggplot2)
source(here::here("scripts", "00_functions.r"))
# read data
master <- read.csv(here::here(file = "data/synthesized_data", "master_stability.csv"))


cor.test(z_standard(master$prod_comp_stability),
         z_standard(master$prod_stability), method = "kendall") # p-value = 3.921e-05, tau = 0.185

(prod_stability_metric_corr <-
  ggplot() +
  geom_point(data = master, aes(x = z_standard(prod_comp_stability), y = z_standard(prod_stability), colour = site, shape = ecosystem), size = 2.5) +
  geom_smooth(data = master, aes(x = z_standard(prod_comp_stability), y = z_standard(prod_stability), colour = site), 
              method = lm, se = F) +
  geom_smooth(data = master, aes(x = z_standard(prod_comp_stability), y = z_standard(prod_stability) ), 
                method = lm, se = T, colour = "black") +
  labs(x = "Compositional stability", y = "Aggregate stability", title = "a. Producer stability \n") +
  theme_classic() 
    
)


cor.test(z_standard(master$con_comp_stability), z_standard(master$con_stability), method = "kendall") # p-value = 7.184e-07, tau = 0.223
(con_stability_metric_corr <-
    ggplot() +
    geom_point(data = master, aes(x = z_standard(con_comp_stability), y = z_standard(con_stability), colour = site, shape = ecosystem), size = 2.5) +
    geom_smooth(data = master, aes(x = z_standard(con_comp_stability), y = z_standard(con_stability), colour = site), 
                method = lm, se = F) +
    geom_smooth(data = master, aes(x = z_standard(con_comp_stability), y = z_standard(con_stability) ), 
                method = lm, se = T, colour = "black") +
    labs(x = "Compositional stability", y = "Aggregate stability", title = "b. Consumer stability \n") +
    theme_classic() 
  
)
