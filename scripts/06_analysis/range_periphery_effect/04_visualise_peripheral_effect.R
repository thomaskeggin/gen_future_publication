# set --------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)

# load -------------------------------------------------------------------------
# periphery slope model
slope_model <-
  readRDS("./results/range_periphery_effect/03_periphery_effect_model.rds") #input

# model fits
model_summaries <-
  readRDS("./results/range_periphery_effect/02_model_summary_tables.rds") #input

# region metrics
region_metrics <-
  read_csv("./data_processed/realms/02_region_metrics.csv", #input
           show_col_types = F)

# coastlines
coastlines <-
  vect("./data/coastlines/gshhg-shp-2.3.7/GSHHS_shp/c/GSHHS_c_L1.shp") #input

# spalding
spalding <-
  vect("./data/ecoregions/Marine_Ecoregions_Of_the_World_(MEOW)-shp/Marine_Ecoregions_Of_the_World__MEOW_.shp") |>  #input
  rename(ecoregion = ECOREGION) |> 
  project(coastlines)

# wrangle ----------------------------------------------------------------------
# scatter plot
plot_data <-
  slope_model$model |>
  as_tibble() |> 
  left_join(model_summaries$pgls) |> 
  rename(ecoregion = region) |> 
  mutate(predicted = predict(slope_model)) |>
  rename(log_delta_sst = `log(delta_sst)`,
         log_distance = `log(distance)`) |> 
  pivot_longer(cols = contains("log"))

scatter_data <-
  plot_data |> 
  pivot_longer(cols = c(slope,predicted),
               names_to = "slope_type",
               values_to = "slope_estimate") |> 
  
  # remove non-significant distance predictions
  filter(!(slope_type == "predicted" & name == "log_delta_sst")) |> 
  
  mutate(name = gsub("log_delta_sst",
                     "SST change from\n2013 to 2100 (log(C))",
                     name),
         
         name = gsub("log_distance",
                     "Mean least-cost distance\nbetween habitable patches (km)",
                     name))

# plot scatter -----------------------------------------------------------------
# slope vs temp.
scatter_plot <-
  ggplot(scatter_data) +
  
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  
  geom_point(aes(x = value,
                 y = slope_estimate,
                 colour = slope_type)) +
  
  facet_wrap(~name,
             scales = "free_x") +
  
  labs(y = str_wrap(#"Periphery effect. Slope of the thermal suitability vs peripheralness model",
    "Periphery effect model slope (R)",
    40),
    colour = "Slope\nestimate",
    x = "") +
  
  scale_colour_manual(values = c("black",
                                 "#1F8573")) +
  
  theme_bw() +
  theme(text = element_text(family = "serif")) +
  
  plot_annotation(tag_levels = "a")

# plot maps --------------------------------------------------------------------
# wrangle
map_data <-
  spalding |> 
  left_join(plot_data) |> 
  filter(ecoregion %in% unique(model_summaries$pgls$region))

# plot slopes
map_slope <-
  ggplot() +
  
  geom_spatvector(data = map_data,
                  aes(fill= slope)) +
  
  geom_spatvector(data = coastlines,
                  fill = "#FFFFF2") +
  
  labs(fill = "Periphery effect") +
  
  scale_fill_gradient(#high = "#14806E",
    high = "#00493A",
    low = "white",
    na.value = "white") +
  theme_minimal() +
  
  theme(axis.text = element_blank()) +
  theme(text = element_text(family = "serif")) 

# plot delta sst
map_sst <-
  ggplot() +
  
  geom_spatvector(data = map_data |> filter(name == "log_delta_sst"),
                  aes(fill= value)) +
  
  geom_spatvector(data = coastlines,
                  fill = "#FFFFF2") +
  
  labs(fill = "SST change from\n2013 to 2100 (log(C))") +
  
  scale_fill_gradient(#high = "#A6001C",
    high = "#800015",
    low = "white",
    na.value = "transparent") +
  theme_minimal()+
  
  theme(axis.text = element_blank()) +
  theme(text = element_text(family = "serif"))

# plot habitat density
map_density<-
  ggplot() +
  
  geom_spatvector(data = map_data |> filter(name == "log_distance"),
                  aes(fill= value)) +
  
  geom_spatvector(data = coastlines,
                  fill = "#FFFFF2") +
  
  labs(fill = "Mean least-cost distance\nbetween habitable patches (km)") +
  
  scale_fill_gradient(#high = "#332288",
    high = "#302080",
    low = "white",
    na.value = "transparent") +
  theme_minimal()+
  
  theme(axis.text.y = element_blank()) +
  theme(text = element_text(family = "serif"))

# combine
combination_plot <-
  (map_slope / map_sst / map_density) +
  
  plot_layout(guides  = "collect")+
  
  plot_annotation(tag_levels = list("b","",""))

# export -----------------------------------------------------------------------
# scatter plot
ggsave("./plots/range_periphery_effect/slope_v_sst.png", #output
       scatter_plot,
       width = 13*0.66,
       height = 6*0.66)

# map plots
ggsave("./plots/range_periphery_effect/maps.png", #output
       combination_plot,
       width = 10,
       height = 8)

