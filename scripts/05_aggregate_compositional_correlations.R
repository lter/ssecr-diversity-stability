rm(list = ls())

library(ggplot2)

# read data
master <- read.csv(here::here(file = "data/synthesized_data", "master_stability.csv"))


cor.test(log(master$prod_comp_stability+1), log(master$prod_stability+1), method = "kendall") # p-value = 3.921e-05, tau = -0.185

(prod_stability_metric_corr <-
  ggplot() +
  geom_point(data = master, aes(x = log(prod_comp_stability+1), y = log(prod_stability+1), colour = site, shape = ecosystem), size = 2.5) +
  geom_smooth(data = master, aes(x = log(prod_comp_stability+1), y = log(prod_stability+1), colour = site), 
              method = lm, se = F) +
  geom_smooth(data = master, aes(x = log(prod_comp_stability+1), y = log(prod_stability+1) ), 
                method = lm, se = T, colour = "black") +
  labs(x = "Compositional stability", y = "Aggregate stability", title = "a. Producer stability \n") +
  theme_classic() 
    
)

cor.test(log(master$con_comp_stability+1), log(master$con_stability+1), method = "kendall") # p-value = 7.184e-07, tau = -0.223
(con_stability_metric_corr <-
    ggplot() +
    geom_point(data = master, aes(x = log(con_comp_stability+1), y = log(con_stability+1), colour = site, shape = ecosystem),  size = 2.5) +
    geom_smooth(data = master, aes(x = log(con_comp_stability+1), y = log(con_stability+1), colour = site), 
                method = lm, se = F) +
    geom_smooth(data = master, aes(x = log(con_comp_stability+1), y = log(con_stability+1) ), 
                method = lm, se = T, colour = "black") +
    labs(x = "Compositional stability", y = "Aggregate stability", title = "b. Consumer stability \n") +
    theme_classic() 
  
)
