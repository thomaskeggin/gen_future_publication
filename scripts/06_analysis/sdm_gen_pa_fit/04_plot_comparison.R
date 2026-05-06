# set --------------------------------------------------------------------------
library(tidyverse)
library(gen3sisExtra)
library(patchwork)

# load -------------------------------------------------------------------------
# parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# sdm - gen comparison
comparison <-
  read_csv("./results/sdm_gen_pa_fit/fit_results.csv") #input

# clustering results
clustering_results <-
  readRDS("./results/cluster_responses/01_cluster_responses.rds") #input


# wrangle ---------------------------------------------------------------------
# join fit and parameters
comp <-
  left_join(parameters,
            comparison)

# create heat plot data
sr <-
  comp |> 
  select(`Dispersal range (km / year)`,
         `Adaptive rate (°C / year)`,
         species_richness) |> 
  interpolateParameterResponse(F)

spearman <-
  comp |> 
  select(`Dispersal range (km / year)`,
         `Adaptive rate (°C / year)`,
         pa_spearman) |> 
  interpolateParameterResponse(F)

pearson <-
  comp |> 
  select(`Dispersal range (km / year)`,
         `Adaptive rate (°C / year)`,
         pa_pearson) |> 
  interpolateParameterResponse(F)

jac <-
  comp |> 
  select(`Dispersal range (km / year)`,
         `Adaptive rate (°C / year)`,
         pa_jaccard) |> 
  interpolateParameterResponse(F)

# kmeans polygons
kmeans_polygons <-
  clustering_results$results |> 
  
  group_by(kmeans_cluster_name) |> 
  
  slice(chull(`Dispersal range (km / year)`,
              `Adaptive rate (°C / year)`))

# plot -------------------------------------------------------------------------
# common theme elements
theme_common <-
  theme_classic() +
  theme(text = element_text(family = "serif",
                            size = 10))

aspect_ratio <-
  max(rmse$`Dispersal range (km / year)`) / max(rmse$`Adaptive rate (°C / year)`)

# species richness
plot_sr <-
  ggplot(sr) +
  
  geom_tile(aes(x = `Dispersal range (km / year)`,
                y = `Adaptive rate (°C / year)`,
                fill = species_richness)) +
  
  # polygons
  geom_polygon(data = kmeans_polygons |>
                 filter(kmeans_cluster_name %in% c("Species expansion")),
               aes(x = `Dispersal range (km / year)`,
                   y = `Adaptive rate (°C / year)`,
                   group = factor(kmeans_cluster_name)),
               fill = "white",
               linewidth = 1,
               colour = alpha("black",1),
               alpha = 0) +
  
  coord_fixed(ratio = aspect_ratio) +
  
  scale_fill_gradient(#high = "#AC1909",
                      low = "#382E59",
                      high = "white",
                      na.value = "transparent") +
  
  labs(fill = str_wrap("Mean absolute difference in species richness",15),
       x = "") +
  
  theme_common +
  
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# pearson
plot_pearson <-
  ggplot(pearson) +
  
  geom_tile(aes(x = `Dispersal range (km / year)`,
                y = `Adaptive rate (°C / year)`,
                fill = pa_pearson)) +
  
  # polygons
  geom_polygon(data = kmeans_polygons |>
                 filter(kmeans_cluster_name %in% c("Species expansion")),
               aes(x = `Dispersal range (km / year)`,
                   y = `Adaptive rate (°C / year)`,
                   group = factor(kmeans_cluster_name)),
               fill = "white",
               linewidth = 1,
               colour = alpha("black",1),
               alpha = 0) +
  
  coord_fixed(ratio = aspect_ratio) +
  
  scale_fill_gradient(#high = "#AC1909",
    high = "#382E59",
    low = "white",
    na.value = "transparent") +
  
  labs(fill = str_wrap("Pearson correlation coefficient",15)) +
  
  theme_common

# spearman
plot_spearman <-
  ggplot(spearman) +
  
  geom_tile(aes(x = `Dispersal range (km / year)`,
                y = `Adaptive rate (°C / year)`,
                fill = pa_spearman)) +
  
  # polygons
  geom_polygon(data = kmeans_polygons |>
                 filter(kmeans_cluster_name %in% c("Species expansion")),
               aes(x = `Dispersal range (km / year)`,
                   y = `Adaptive rate (°C / year)`,
                   group = factor(kmeans_cluster_name)),
               fill = "white",
               linewidth = 1,
               colour = alpha("black",1),
               alpha = 0) +
  
  coord_fixed(ratio = aspect_ratio) +
  
  scale_fill_gradient(#high = "#AC1909",
    high = "#382E59",
    low = "white",
    na.value = "transparent") +
  
  labs(fill = str_wrap("Spearman correlation coefficient",15),
       y = "") +
  
  theme_common +
  
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# jaccard
plot_jac <-
  ggplot(jac) +
  
  geom_tile(aes(x = `Dispersal range (km / year)`,
                y = `Adaptive rate (°C / year)`,
                fill = pa_jaccard)) +
  
  # polygons
  geom_polygon(data = kmeans_polygons |>
                 filter(kmeans_cluster_name %in% c("Species expansion")),
               aes(x = `Dispersal range (km / year)`,
                   y = `Adaptive rate (°C / year)`,
                   group = factor(kmeans_cluster_name)),
               fill = "white",
               linewidth = 1,
               colour = alpha("black",1),
               alpha = 0) +
  
  coord_fixed(ratio = aspect_ratio) +
  
  scale_fill_gradient(low = "white",
                      #high = "#092C38",
                      high = "#382E59",
                      na.value = "transparent") +
  
  labs(fill = "Jaccard\nsimilarity",
       x = "",
       y = "") +
  
  theme_common +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

# composite
plot_composite <-
  (plot_sr + plot_jac) /
  (plot_pearson + plot_spearman) +
  plot_annotation(tag_levels = "a")

# export -----------------------------------------------------------------------
ggsave("./plots/sdm_v_gen/sdm_v_gen_heat.jpg", #output
       plot_composite,
       height = 180*1,
       width = 220*1,
       units = "mm",
       dpi = 300)
