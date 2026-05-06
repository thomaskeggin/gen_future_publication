# set --------------------------------------------------------------------------
library(tidyverse)

# wrangle ----------------------------------------------------------------------
dist_plot <-
  ggplot() +
  
  stat_function(fun = dnorm,
                n = 1000,
                args = list(mean = 0,
                            sd = 1),
                colour = alpha("black",0.5)) +
  
  geom_vline(xintercept = -4,
             linetype = "dashed",
             colour = alpha("black",0.5)) +
  
  lims(x = c(-4,4),
       y = c(0.001,0.5)) +
  
  coord_flip() +
  
  theme_classic() +
  
  theme(axis.line.x = element_blank(),
        
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# export -----------------------------------------------------------------------
ggsave("./plots/latitudinal_diversity_gradient/gaussian_distribution_diagram.png", #output
       dist_plot,
       height = 60*0.5,
       width = 45*0.5,
       units = "mm",
       dpi = 300)

