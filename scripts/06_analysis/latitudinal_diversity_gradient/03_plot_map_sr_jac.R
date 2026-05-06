# How does species richness change latitudinally with climate change?

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(terra)
library(tidyterra)
library(progress)
library(patchwork)
library(ggh4x)

# title wrap characters 
wrap_characters <-
  20

# plot colours
colours <-
  list(delta_sr = c("#CC6677",
                    "#88CCEE"),
       jaccard = c("#3F76A6"))

# common theme elements
theme_common <-
  theme(panel.grid = element_blank(),
        text = element_text(family = "serif"))

# load -------------------------------------------------------------------------
# unsupervised clusters
kmeans_clusters <-
  readRDS("./results/kmeans_response_clusters.rds") #input

# species initialisation
species_info <-
  readRDS("./data_processed/species/species_initialisation_information.rds") #input

# coastlines
coastlines <-
  vect("./data/coastlines/gshhg-shp-2.3.7/GSHHS_shp/c/GSHHS_c_L1.shp") #input

# latitudinal metrics 
lat_metrics <-
  readRDS("./results/latitudinal_diversity_gradient/latitudinal_metrics.rds") |>  #input
  left_join(kmeans_clusters)

# seascape
sea <-
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] |> #input
  select(x,y) |> 
  as_tibble(rownames = "cell")

# jaccard 
jaccard_lat <-
  readRDS("./results/turnover/jaccard_lat_plot.rds")  #input

# wrangle latitudinal bar plots ------------------------------------------------
# starting latitudinal peaks
species_info_df <-
  list()

for(i in names(species_info)){
  
  species_info_df[[i]] <-
    tibble(cell = names(species_info[[i]]$abundance),
           abundance = species_info[[i]]$abundance)
}

species_info_df <-
  do.call(rbind.data.frame,species_info_df)

#find maximum richness for each hemisphere
species_info_df_agg <-
  species_info_df |>
  right_join(sea) |>
  group_by(cell) |>
  mutate(richness = n(),
         hemisphere = sign(y)) |>
  group_by(hemisphere,y) |>
  reframe(richness_mean_start = mean(richness))

species_info_df_agg$loess_fit <-
  loess(richness_mean_start ~ y,
        span = 0.1,
        data = species_info_df_agg) |>
  predict()

species_info_df_agg <-
  species_info_df_agg |>
  group_by(hemisphere) |>
  mutate(maximum = loess_fit == max(loess_fit))

# wrangle for plot
ldg_data <-
  lat_metrics |> 
  filter(year == 2100) |> 
  
  
  group_by(kmeans_cluster_name,y) |> 
  reframe(richness_mean_2100 = mean(richness_mean)) |> 
  full_join(species_info_df_agg) |> 
  na.omit() |> 
  mutate(richness_gain = richness_mean_2100 - richness_mean_start,
         gain_sign     = factor(sign(richness_gain))) |> 
  filter(y <= 50,
         y >= -50)

ldg_data <-
  ldg_data |> 
  
  # fake extinction data for the extra panel
  rbind.data.frame(ldg_data |> 
                     filter(kmeans_cluster_name == "Species expansion") |> 
                     
                     mutate(kmeans_cluster_name =
                              kmeans_clusters |>
                              filter(kmeans_cluster_name == "Global extinction") |> 
                              pull(kmeans_cluster_name) |> 
                              unique(),
                            
                            richness_mean_2100 = 0,
                            loess_fit = 0,
                            maximum = FALSE,
                            richness_gain = -richness_mean_start,
                            gain_sign = -1))

# plot LDG shift ---------------------------------------------------------------
# set common limits
max_richness <-
  max(ldg_data |>
        select(richness_gain,
               richness_mean_start,
               richness_mean_2100))

min_richness <-
  min(ldg_data |>
        select(richness_gain,
               richness_mean_start,
               richness_mean_2100))

# colours
barplot_colours <-
  viridisLite::mako(100,
                    direction = -1)[c(40,60)]

# absolute values
plot_absolute <-
  ggplot(ldg_data) +
  
  # equator
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = 0.25) +
  
  # initialisation
  geom_col(aes(x = y, y = richness_mean_start),
           colour = "black",
           fill = "transparent",
           linewidth = 0.9,
           alpha = 1,
           width = 0.99) +
  
  geom_col(aes(x = y, y = richness_mean_start),
           colour = "transparent",
           fill = "white",
           alpha = 1,
           width = 1) +
  
  # 2100 data
  geom_col(aes(y = richness_mean_2100,
               x = y,
               fill = gain_sign,
               colour = gain_sign),
           alpha = 0.7) +
  
  coord_flip() +
  
  lims(x = c(-50,
             50)) +
  
  labs(#title = str_wrap("Mean latitudinal species richness at 2100",wrap_characters),
    x = "",
    y = "Mean latitudinal\nspecies Richness") +
  
  scale_fill_manual(values = barplot_colours) +
  scale_colour_manual(values = barplot_colours) +
  
  facet_grid(rows = vars(kmeans_cluster_name)) +
  
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  theme_common

# gain plot
plot_gain <-
  ggplot(ldg_data) +
  
  # equator
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = 0.25) +
  
  # initialisation
  geom_col(aes(x = y, y = richness_gain,
               fill = gain_sign,
               colour = gain_sign),
           alpha = 1) +
  
  # baseline
  geom_hline(yintercept = 0,
             linewidth = 0.5,
             alpha = 1) +
  
  coord_flip() +
  
  lims(x = c(-50,
             50)) +
  
  labs(#title = str_wrap("Gain in species richness\nbetween 2013 and 2100", wrap_characters),
    x = "Latitude",
    y = "Species Richness") +
  
  scale_fill_manual(values = barplot_colours) +
  scale_colour_manual(values = barplot_colours) +
  
  facet_grid(rows = vars(kmeans_cluster_name)) +
  
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  theme_common

# plot jaccard -----------------------------------------------------------------
# cluster colours
cluster_colours_df <-
  kmeans_clusters |> 
  select(kmeans_cluster_name,
         kmeans_colour) |> 
  distinct()

cluster_colours <-
  cluster_colours_df$kmeans_colour

names(cluster_colours) <-
  cluster_colours_df$kmeans_cluster_name

# plot
jac_col_plot <-
  ggplot(jaccard_lat) +
  
  # equator
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = 0.25) +
  
  # columns
  geom_col(aes(x = y, y = jaccard,
               fill = kmeans_cluster_name,
               colour = kmeans_cluster_name)) +
  
  # facets
  facet_grid(rows = vars(kmeans_cluster_name)) +
  
  # layout
  coord_flip() +
  
  lims(#y = c(0,1),
       x = c(-50,
             50)) +
  
  # colours
  scale_fill_manual(values = cluster_colours) +
  scale_colour_manual(values = cluster_colours) +
  scale_y_continuous(breaks = c(0,0.5,1),
                     limits = c(0,1)) +
  
  # labs and theme
  labs(y = "Mean latitudinal\nJaccard similarity",
       x = "Latitude",
       fill = "Jaccard\nsimilarity",
       colour = "Jaccard\nsimilarity") +
  
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        # strip.background = element_blank(),
        # strip.text.y = element_text(face = "bold",
        #                             angle = 0),
        legend.position = "none") +
  theme_common

# plot richness maps -----------------------------------------------------------
params <-
  lat_metrics |> 
  dplyr::select(run_id,kmeans_cluster_name) |> 
  distinct()

# wrangle
map_data <-
  read_csv("./results/04_metrics_cell/04_metrics_cell_0.csv", #input
           show_col_types = F) |> 
  filter(year == 2100,
         y <= 50,
         y >= -50) |> 
  left_join(params) |> 
  group_by(x,y,kmeans_cluster_name) |> 
  reframe(sr = mean(richness)) |> 
  
  # fake extinction data
  rbind.data.frame(
    tibble(
      x = 0,
      y = 0.5,
      kmeans_cluster_name =
        kmeans_clusters |>
        filter(kmeans_cluster_name == "Global extinction") |> 
        pull(kmeans_cluster_name) |> 
        unique(),
      sr = 0
    ))

# plot
maps_plot <-
  ggplot(map_data) +
  
  # equator
  geom_hline(yintercept = 0,
             linetype = "dashed",
             alpha = 0.25) +
  
  # richness outline
  geom_tile(aes(x=x,y=y),
            colour = alpha("black",1),
            linewidth = 0.25) +
  
  # richness fill
  geom_tile(aes(x=x,y=y,
                fill = sr)) +
  
  # coastlines
  geom_spatvector(data = coastlines,
                  colour = alpha("black",0.1),
                  fill = alpha("#FFF4DB",1)) +
  
  # layout
  facet_wrap(~kmeans_cluster_name,
             ncol = 1,
             strip.position = "right") +
  
  labs(#title = str_wrap("Mean species richness at 2100",wrap_characters),
    y = "",
    x = "Mean species richness",
    fill = str_wrap("Species Richness",15)) +
  
  ylim(c(-50,50)) +
  
  coord_sf(expand = FALSE) +
  
  # colours
  # scale_fill_gradient(low = "white",
  #                     high = colours$map,
  #                     limits = c(0,850)) +
  
  scale_fill_viridis_c(direction = -1,
                       option = "mako") +
  
  # theme
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.ticks = element_line(colour = "transparent"),
        axis.text = element_text(colour = "transparent")) +
  
  theme_common

# plot cluster names -----------------------------------------------------------
# wrangle
cluster_names <-
  
  kmeans_clusters |> 
  select(kmeans_cluster_name,
         kmeans_colour) |> 
  distinct() |> 
  mutate(x=0,y=0) |> 
  arrange(kmeans_cluster_name)

# plot
plot_cluster_names <-
  ggplot(cluster_names) +
  
  geom_text(aes(x=x,
                y=y,
                label = kmeans_cluster_name),
            family = "serif") +
  
  facet_grid2(rows = vars(kmeans_cluster_name),
              strip = strip_themed(background_y = 
                                     elem_list_rect(fill = cluster_names$kmeans_colour)),
              switch = "y") +
  
  labs(#title = "Cluster names",
       x = "",
       y = "") +
  
  theme_common +
  theme(strip.text = element_text(colour = "transparent"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank())

# plot composite ---------------------------------------------------------------
plot_composite <-
  plot_cluster_names +
  maps_plot + 
  plot_absolute + 
  #plot_gain +
  jac_col_plot +
  
  plot_layout(widths = c(0.6,
                         0.9,
                         0.4,
                         0.4),
              axis_titles = "collect") + 
  
  plot_annotation(tag_levels = 'a')

# export -----------------------------------------------------------------------
# ggsave("./plots/latitudinal_diversity_gradient/categorised_LDG.jpg", #output
#        plot_composite,
#        height = 11*0.7,
#        width = 15*0.7)

ggsave("./plots/latitudinal_diversity_gradient/categorised_LDG.jpg", #output
       plot_composite,
       height = 90*1.5,
       width = 180*1.5,
       units = "mm",
       dpi = 300)


