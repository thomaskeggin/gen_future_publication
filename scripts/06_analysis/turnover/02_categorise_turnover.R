# set --------------------------------------------------------------------------
library(tidyverse)
library(tidyterra)
library(terra)
library(patchwork)

# load -------------------------------------------------------------------------
# turnover
turnover_df <-
  readRDS("./results/turnover/01_turnover_df.rds") #input

# seascape
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] |>  #input
  select(x,y) |> 
  as_tibble(rownames = "cell")

# parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# coastlines
coastlines <-
  vect("./data/coastlines/gshhg-shp-2.3.7/GSHHS_shp/c/GSHHS_c_L1.shp") #input

# unsupervised clusters
kmeans_clusters <-
  readRDS("./results/kmeans_response_clusters.rds") #input

# wrangle ----------------------------------------------------------------------
turnover_metrics <-
  c("colonisations",
    "extinctions",
    "turnover"
    #"jaccard",
    #"temporal_union"
  )

# aggregate to categories
turnover_cat <-
  
  turnover_df |> 
  
  left_join(parameters) |> 
  left_join(kmeans_clusters) |> 
  
  group_by(kmeans_cluster_name,cell) |> 
  
  reframe(across(any_of(turnover_metrics),
                 ~ mean(.x))) |> 
  
  left_join(seascape) |> 
  
  # make extinctions negative
  mutate(extinctions = -extinctions)

turnover_jaccard <-
  
  turnover_df |> 
  
  left_join(parameters) |> 
  left_join(kmeans_clusters) |> 
  
  group_by(kmeans_cluster_name,cell) |> 
  
  reframe(jaccard = mean(jaccard)) |> 
  
  left_join(seascape)

# pivot longer
turnover_long <-
  turnover_cat |> 
  
  pivot_longer(cols = any_of(turnover_metrics))

# by latitude
jaccard_lat <-
  
  turnover_df |> 
  
  left_join(parameters) |> 
  left_join(kmeans_clusters) |> 
  left_join(seascape) |> 
  
  group_by(kmeans_cluster_name,
           y) |> 
  
  reframe(jaccard = mean(jaccard)) |> 
  
  # filter to 50 degree latitude
  filter(abs(y) <= 50)

# plot -------------------------------------------------------------------------
# map Jaccard
map_jaccard <-
  ggplot(turnover_jaccard |>
           arrange(kmeans_cluster_name) |> 
           mutate(kmeans_cluster_name = str_wrap(kmeans_cluster_name,15),
                  kmeans_cluster_name = factor(kmeans_cluster_name,
                                               levels = unique(kmeans_cluster_name)))) +
  
  # coastlines
  geom_spatvector(data = coastlines,
                  colour = alpha("black",0.2),
                  fill = alpha("#F2E4C9",0.5)) +
  
  geom_tile(aes(x=x,y=y,
                fill = jaccard)) +
  
  facet_grid(rows = vars(kmeans_cluster_name)) +
  
  
  labs(x = "",
       y = "",
       fill = "Jaccard\nsimilarity",
       title = "Jaccard similarity between 2013 and 2100") +
  
  scale_fill_viridis_c(direction = -1) +
  # scale_fill_gradient(high = "#023535",
  #                     low = "white",
  #                     limits = c(0,1)) +
  
  theme_classic() +
  
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

# Map other turnover metrics
scale_limits <-
  c(-max(abs(turnover_long$value)),
    max(abs(turnover_long$value)))


map_turnover <-
  ggplot(turnover_long) +
  
  # cell outlines
  geom_tile(aes(x=x,y=y),
            colour = alpha("black",0.1)) +
  
  # coastlines
  geom_spatvector(data = coastlines,
                  colour = alpha("black",0.2),
                  fill = alpha("#F2E4C9",0.5)) +
  
  # cell fill
  geom_tile(aes(x=x,y=y,
                fill = value)) +
  
  # layout
  facet_grid(cols = vars(name),
             rows = vars(kmeans_cluster_name)) +
  
  
  labs(x = "",
       y = "",
       fill = "Species\ngain/loss",
       title = "Turnover metrics between 2013 and 2100") +
  
  scale_fill_gradient2(high = "#00122E",
                       mid = "white",
                       low = "#480000",
                       midpoint = 0,
                       limits = scale_limits,
                       na.value = "white") +
  
  theme_bw()


# boxplot
boxplot <-
  ggplot(turnover_cat) +
  
  geom_boxplot(aes(x = kmeans_cluster_name,
                   y = log(jaccard))) +
  
  labs(x = "",
       caption = "Important! Values of 0 (no similarity) have been omitted.",
       title = "Jaccard similarity between 2013 and 2100") +
  
  theme_classic()

# latitude plot
jac_col_plot <-
  ggplot(jaccard_lat) +
  
   geom_vline(xintercept = 0,
              linetype = "dashed",
              alpha = 0.5) +
   
   geom_col(aes(x = y, y = jaccard,
                fill = jaccard,
                colour = jaccard)) +
  
  geom_violin(aes(x = 0, y = jaccard),
              width = 50,
              alpha = 0.2) +
  
  facet_grid(rows = vars(kmeans_cluster_name)) +
  
  scale_fill_gradient(low = "#023535",
                      high = "#0FC2C0",
                      limits = c(0,1)) +
  
  scale_colour_gradient(low = "#023535",
                        high = "#0FC2C0",
                        limits = c(0,1)) +
  
  coord_flip() +
  
  lims(y = c(0,1)) +
  
  labs(y = "Jaccard similarity",
       x = "Latitude",
       fill = "Jaccard\nsimilarity",
       colour = "Jaccard\nsimilarity",
       title = "Jaccard similarity\nbetween 2013 and 2100") +
  
  theme_bw()

# jaccard histogram
hist_data <-
  turnover_df |>
  left_join(parameters) |> 
  left_join(kmeans_clusters) |> 
  left_join(seascape) |> 
  group_by(kmeans_cluster_name,run_id) |> 
  reframe(jaccard = mean(jaccard))

jac_hist_plot <-
  ggplot(hist_data) +
  
  # geom_histogram(aes(x = jaccard),
  #                bins = 50,
  #                fill = "#015958") +
  
  geom_density(aes(x = jaccard),
                 #bins = 50,
                 fill = "#015958") +
  
  facet_grid(rows = vars(kmeans_cluster_name)) +
  
  lims(x = c(0,1)) +
  
  theme_bw()

# histogram and map composite --------------------------------------------------
# If we want to keep this, tidy this up properly

# export plot ------------------------------------------------------------------
# map Jaccard
ggsave("./plots/turnover/jaccard_map.png", #output
       map_jaccard,
       height = 7.5,
       width = 6)

# map other turnover metrics
ggsave("./plots/turnover/turnover_map.png", #output
       map_turnover,
       height = 10,
       width = 15)

# box plot
# ggsave("./plots/turnover/turnover_boxplot.png", #output
#        boxplot,
#        height = 7,
#        width = 5)

# latitude plot
ggsave("./plots/turnover/jaccard_latitude_plot.png", #output
       jac_col_plot,
       height = 7*1.25,
       width = 3*1.25)

# jaccard histogram
ggsave("./plots/turnover/jaccard_histogram.png", #output
       jac_hist_plot,
       height = 6*1.25,
       width = 4*1.25)
       
# export plot data -------------------------------------------------------------
saveRDS(jaccard_lat,
        "./results/turnover/jaccard_lat_plot.rds") #output
       
       