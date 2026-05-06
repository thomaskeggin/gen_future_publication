# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# peak summaries
peak_gaussian_summaries <-
  read_csv("./results/latitudinal_diversity_gradient/peak_gaussian_summaries.csv", #input
           show_col_types = F)

# load in the parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# wrangle ----------------------------------------------------------------------
# keep only significant gaussian fits
results <-
  peak_gaussian_summaries |> 
  left_join(parameters) |> 
  filter(sig_bon == TRUE) |> 
  mutate(hemisphere = ifelse(hemisphere == 1,
                             "North",
                             "South"))

# plot -------------------------------------------------------------------------
latitude_barplot <-
  ggplot(results |> filter(parameter == "lat_peak_latitude")) +
  
  geom_boxplot(aes(x = hemisphere,
                   y = abs(Estimate),
                   fill = hemisphere),
               width = 0.5) +
  
  facet_grid(cols = vars(category_long)) +
  
  labs(x = "",
       y = "Absolute latitude",
       fill = "Hemisphere") +
  
  scale_fill_manual(values = c("#88CCEE",
                               "#CC6677")) +
  
  theme_bw()

# export -----------------------------------------------------------------------
ggsave("./plots/latitudinal_diversity_gradient/latitudinal_peak_category.png", #output
       latitude_barplot,
       height = 4,
       width = 8)
