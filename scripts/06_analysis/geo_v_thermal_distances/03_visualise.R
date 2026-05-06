# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# model information
model_info <-
  readRDS("./results/geo_v_thermal_distances/02_models.rds") #input

# year/timestep information
y2t <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F)

# wrangle ----------------------------------------------------------------------
plot_me <-
  model_info$summary_info |> 
  left_join(y2t) |> 
  mutate(year = gsub("y_","",year) |> as.numeric()) |> 
  select(-timestep) |> 
  rename(`Adjusted R-squared` = adj_r,
         `Slope estimate` = slope) |> 
  pivot_longer(cols = -c(year,p,p_bon))

# plot -------------------------------------------------------------------------
model_plot <-
  ggplot(plot_me) +
  
  geom_line(aes(x = year, y = value)) +
  
  geom_point(aes(x = year, y = value,
                 fill = p_bon),
             size = 2,
             shape = 21) +
  
  labs(x = "",y = "",
       fill = "P-value\nBonferr. corrected",
       caption = str_wrap("Lowering slope values indicate less latitudinal thermal change over geographic distance.",
                          100)
       #title = "Linear regression of the covariance between geographic and thermal distances across latitude"
       ) +
  
  scale_fill_viridis_c() +
  
  facet_wrap(~name,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  
  theme_bw() +
  
  theme(strip.placement.y = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(family = "serif"))

# export -----------------------------------------------------------------------
ggsave("./plots/geo_v_therm_models.png", #output
       model_plot,
       height = 90,
       width = 180,
       units = "mm",
       dpi = 300)

# 
