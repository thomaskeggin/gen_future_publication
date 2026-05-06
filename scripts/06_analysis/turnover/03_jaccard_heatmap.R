# set --------------------------------------------------------------------------
library(tidyverse)
library(gen3sisExtra)
library(scales)

# load -------------------------------------------------------------------------
# turnover
turnover_df <-
  readRDS("./results/turnover/01_turnover_df.rds") #input

# parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# wrangle ----------------------------------------------------------------------
# aggregate to simulation and add parameter values
turnover_sim <-
  turnover_df |> 
  group_by(run_id) |> 
  reframe(jaccard = mean(jaccard)) |> 
  left_join(parameters) |> 
  select(dispersal_range,
         adaptive_rate,
         jaccard)

# scale and interpolate for heat plot
turnover_heat <-
  interpolateParameterResponse(turnover_sim)

# un-scale the parameters
turnover_heat_unscaled <-
  turnover_heat |> 
  
  mutate(dispersal_range = rescale(dispersal_range,
                                   range(turnover_sim$dispersal_range)),
         adaptive_rate = rescale(adaptive_rate,
                                 range(turnover_sim$adaptive_rate))
  )

# plot -------------------------------------------------------------------------
aspect_ratio <-
  max(turnover_sim$dispersal_range) / max(turnover_sim$adaptive_rate)

heat_plot <-
  ggplot(turnover_heat_unscaled) +
  
  geom_tile(aes(x = dispersal_range,
                y = adaptive_rate,
                fill = jaccard)) +
  
  geom_contour(aes(x=dispersal_range,
                   y=adaptive_rate,
                   z = jaccard),
               colour = "white") +
  
  # layout
  coord_fixed(ratio = aspect_ratio) +
  labs(title = "Jaccard similarity between 2013 and 2100",
       x = "Dispersal range (km)",
       y = "Adaptive rate (°C)",
       fill = "Jaccard\nsimilarity") +
  
  # theme
  scale_fill_viridis_c(na.value = "transparent",
                       direction = -1) +
  theme_classic()

# export -----------------------------------------------------------------------
ggsave("./plots/turnover/jaccard_heat_plot.png", #output
       heat_plot,
       height = 6,
       width = 8)
