# set --------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

point_size <-
  3

point_colour_alpha <-
  0.5

# load -------------------------------------------------------------------------
# clustering results
clustering_results <-
  readRDS("./results/cluster_responses/01_cluster_responses.rds") #input

# delta metrics
delta_metrics <-
  readRDS("./results/parameters_v_responses/delta_metrics_df.rds") #input

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
# pivot longer
delta_metrics_long <-
  delta_metrics |> 
  
  pivot_longer(cols = contains("delta"))

# scale values to match the same colour scheme
heat_list <-
  delta_metrics_long |> 
  
  group_by(name) |> 
  
  group_split()

for(i in 1:length(heat_list)){
  
  # find largest absolute value
  maximum_value <-
    abs(heat_list[[i]]$value) |> max(na.rm = T)
  
  # rescale
  heat_list[[i]]$value <-
    
    scales::rescale(heat_list[[i]]$value,
                    from = c(-maximum_value,
                             maximum_value),
                    to = c(-1,1))
}

heat_scaled <-
  do.call(rbind.data.frame,
          heat_list)

# convert metric names to clean names
heat_df <-
  heat_scaled |> 
  select(-contains("category")) |> 
  rename(column = name) |> 
  mutate(column = gsub("delta_","", column)) |> 
  left_join(metric_names)

# calculate aspect ratio between parameters to generate square plots
aspect_ratio <-
  max(heat_scaled$`Dispersal range (km)`) / max(heat_scaled$`Adaptive rate (°C)`)

# plot
plots_responses <-
  ggplot(heat_df) +
  
  # response metrics
  geom_point(aes(x = `Dispersal range (km)`,
                 y = `Adaptive rate (°C)`,
                 fill = value),
             colour = alpha("black",point_colour_alpha),
             shape = 21,
             size = point_size) +
  
  # extinction simulations
  geom_point(data = extinct_sims,
             aes(x = `Dispersal range (km)`,
                 y = `Adaptive rate (°C)`),
             colour = alpha("black",point_colour_alpha),
             fill = alpha("black",0.75),
             shape = 21,
             size = point_size) +
  
  facet_wrap(~clean_name,
             nrow = 1) +
  
  coord_fixed(ratio = aspect_ratio) +
  
  # labels
  labs(fill = "Scaled response\nvalue") +
  
  # colours
  scale_fill_gradient2(#high = "#092C38",
    high = "#2F5768",
    mid = "white",
    #low = "#AC1909",
    low = "#863C34",
    na.value = "transparent") +
  
  # theme
  theme_classic() +
  theme(text = element_text(family = "serif"))

# plot pca ---------------------------------------------------------------------
# wrangle
pca_scaled <-
  clustering_results$results |> 
  select(dispersal_range,
         adaptive_rate,
         kmeans_cluster,
         kmeans_cluster_name,
         PC1,
         PC2,
         run_id) |> 
  
  mutate(PC1 = scales::rescale(PC1) * clustering_results$pca_variance["PC1"],
         PC2 = scales::rescale(PC2) * clustering_results$pca_variance["PC2"])

# plot
plot_pca <-
  ggplot(pca_scaled) +
  
  geom_point(aes(x = PC2,
                 y = -PC1,
                 fill = factor(kmeans_cluster_name)),
             colour = alpha("black",point_colour_alpha),
             shape = 21,
             ##stroke = 3,
             size = point_size) +
  
  scale_fill_manual(values = kmeans_colours) +
  
  labs(y = paste0("PC1\n(",clustering_results$pca_variance["PC1"]," % of variance)"),
       x = paste0("PC2\n(",clustering_results$pca_variance["PC2"]," % of variance)"),
       #title = "Principal component response hyperspace",
       #caption = "Scaled response metrics binned using kmeans() and 5 groups",
       fill = "Response\ncluster") +
  
  theme_classic() +
  theme(axis.text = element_text(colour = "transparent"),
        text = element_text(family = "serif")) +
  
  coord_fixed(ratio = 1)

# plot kmeans ------------------------------------------------------------------
plot_kmeans <-
  ggplot(clustering_results$results) +
  geom_point(aes(x = `Dispersal range (km)`,
                 y = `Adaptive rate (°C)`,
                 fill = factor(kmeans_cluster_name)),
             colour = alpha("black",point_colour_alpha),
             shape = 21,
             size = point_size) +
  
  scale_fill_manual(values = kmeans_colours) +
  
  labs(fill = "Response\ncluster") +
  
  theme_classic() +
  theme(text = element_text(family = "serif")) +
  
  coord_fixed(ratio = 550/0.22)

# plot kmeans thresholds -------------------------------------------------------
# wrangle
kmeans_polygons <-
  clustering_results$results |> 
  
  group_by(kmeans_cluster_name) |> 
  
  slice(chull(`Dispersal range (km)`,
              `Adaptive rate (°C)`))

# plot
plot_kmeans_polygons <-
  ggplot(kmeans_polygons) +
  
  # polygons
  geom_polygon(aes(x = `Dispersal range (km)`,
                   y = `Adaptive rate (°C)`,
                   fill = factor(kmeans_cluster_name)),
               alpha = 0.7) +
  
  # dispersal thresholds
  geom_vline(xintercept = dispersal_thresholds[1:5],
             linetype = "dashed") +
  
  # adaptation threshold
  geom_hline(yintercept = 0.04,
             linetype = "dashed") +
  
  scale_fill_manual(values = kmeans_colours) +
  
  labs(fill = "Response\ncluster") +
  
  theme_classic() +
  theme(text = element_text(family = "serif"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none") +
  
  coord_fixed(ratio = 550/0.22) +
  
  scale_y_continuous(position = "right")

# compile figures --------------------------------------------------------------
composite_plot <-
  plots_responses /
  (plot_pca +
     plot_kmeans +
     plot_kmeans_polygons) +
  
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect")

# export -----------------------------------------------------------------------
scalar <-
  0.7

ggsave("./plots/metric_responses.png", #output
       composite_plot,
       height = 10 * scalar,
       width = 16 * scalar)

