# set --------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

point_size <-
  3

point_colour_alpha <-
  0.5

target_metrics <-
  c("range_size_mean",
    "species_richness_global",
    "species_richness_cell_mean",
    "fragmentation_mean",
    "abundance_cell_mean",
    "occupied_cells",
    ##"betweenness_mean",
    #"cluster_count_mean",
    #"cluster_size_mean_mean",
    #"cohesion_mean",
    "inter_cluster_distance_mean")

# load -------------------------------------------------------------------------
# clustering results
clustering_results <-
  readRDS("./results/cluster_responses/01_cluster_responses.rds") #input

# delta metrics
metrics <-
  read_csv("./results/04_metrics_simulation/04_metrics_simulation_0.csv", #input
           show_col_types = F) |> 
  select(run_id,
         all_of(target_metrics))

# metric names
metric_names <-
  read_csv("./results/metric_descriptions.csv", #input
           show_col_types = F)

# dispersal thresholds
dispersal_thresholds <-
  c(35.42, # minimum cell to cell distance
    readRDS("G:/gen_future/results/dispersal_thresholds.rds")) #input

# wrangle ----------------------------------------------------------------------
# kmeans colour scheme
kmeans_colours <-
  clustering_results$kmeans_key$kmeans_colour

names(kmeans_colours) <-
  clustering_results$kmeans_key$kmeans_cluster_name

# isolate extinct simulations
extinct_sims <-
  clustering_results$results |> 
  filter(kmeans_cluster_name == "Global extinction")

# plot response metrics --------------------------------------------------------
plot_data <-
  clustering_results$results |> 
  select(run_id,
         `Dispersal range (km / year)`,
         `Adaptive rate (°C / year)`) |> 
  left_join(metrics) |> 
  
  pivot_longer(cols = all_of(target_metrics),
               names_to = "column") |> 
  
  group_by(column) |> 
  mutate(value = rescale(value)) |> 
  left_join(metric_names)



aspect_ratio <-
  max(plot_data$`Dispersal range (km / year)`) / max(plot_data$`Adaptive rate (°C / year)`)

clustering_metric_reponses <-
  ggplot(plot_data) +
  
  geom_point(aes(x = `Dispersal range (km / year)`,
                 y = `Adaptive rate (°C / year)`,
                 fill = value),
             colour = alpha("black",point_colour_alpha),
             shape = 21,
             size = point_size) +
  
  # extinction simulations
  geom_point(data = extinct_sims,
             aes(x = `Dispersal range (km / year)`,
                 y = `Adaptive rate (°C / year)`),
             colour = alpha("black",point_colour_alpha),
             fill = alpha("black",0.75),
             shape = 21,
             size = point_size) +
  
  facet_wrap(~clean_name,
             nrow = 3) +
  
  coord_fixed(ratio = aspect_ratio) +
  
  # labels
  labs(fill = "Scaled response\nvalue") +
  
  # colours
  # scale_fill_gradient2(#high = "#092C38",
  #   high = "#2F5768",
  #   mid = "white",
  #   #low = "#AC1909",
  #   low = "#863C34",
  #   na.value = "transparent") +
  scale_fill_viridis_c(begin = 0.25) +
  
  # theme
  theme_classic() +
  theme(text = element_text(family = "serif"))

# export -----------------------------------------------------------------------
scalar <-
  0.7

ggsave("./plots/clustering_metric_responses_.png", #output
       clustering_metric_reponses,
       height = 10 * scalar,
       width = 13 * scalar)

