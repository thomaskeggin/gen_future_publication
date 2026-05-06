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
        text = element_text(family = "serif"),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "pt"))

# plot formatting --------------------------------------------------------------
# list of plots
plots <-
  list()

# composite widths
w_map <- 1
w_col <- 0.1

comp_widths <-
  c(w_map,
    w_col,w_col)

# map aspect ratio
map_ar <- 100 / 360

col_ar <- map_ar * (w_map / w_col) 

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

# kmeans cluster order ---------------------------------------------------------
cluster_names <-
  
  kmeans_clusters |> 
  select(kmeans_cluster_name,
         kmeans_colour) |> 
  distinct() |> 
  mutate(x=0,y=0) |> 
  arrange(kmeans_cluster_name)

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
                    direction = -1)[c(30,80)]

# absolute plots
plots$absolute <-
  list()

for(group in cluster_names$kmeans_cluster_name){
  
  plots$absolute[[group]] <-
    ggplot(ldg_data |> 
             filter(kmeans_cluster_name == group)) +
    
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
    
    scale_y_continuous(breaks = c(0,500),
                       limits = c(0,600)) +
    
    lims(x = c(-50,
               50)) +
    
    labs(#title = str_wrap("Mean latitudinal species richness at 2100",wrap_characters),
      x = "",
      y = "") +
    
    scale_fill_manual(values = barplot_colours) +
    scale_colour_manual(values = barplot_colours) +
    
    facet_grid(rows = vars(kmeans_cluster_name)) +
    
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          aspect.ratio = col_ar) +
    theme_common
  
}

plots$absolute$`Species collapse` <-
  plots$absolute$`Species collapse` +
  labs(y = "SR") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text())

plots$absolute$`Global extinction` <-
  plots$absolute$`Global extinction` +
  labs(y = "SR") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text())


# plot jaccard -----------------------------------------------------------------

plots$jaccard <-
  list()

for(group in cluster_names$kmeans_cluster_name){
  
  # plot
  plots$jaccard[[group]] <-
    ggplot(jaccard_lat |> 
             filter(kmeans_cluster_name == group)) +
    
    # equator
    geom_vline(xintercept = 0,
               linetype = "dashed",
               alpha = 0.25) +
    
    # columns
    geom_col(aes(x = y, y = jaccard),
             fill = "#3F76A6", ##D9A86C
             colour = "#3F76A6") +
    
    # facets
    facet_grid(rows = vars(kmeans_cluster_name)) +
    
    # layout
    coord_flip() +
    
    lims(#y = c(0,1),
      x = c(-50,
            50)) +
    
    # colours
    #scale_fill_manual(values = kmeans_colour) +
    #scale_colour_manual(values = kmeans_colour) +
    scale_y_continuous(breaks = c(1),
                       limits = c(0,1)) +
    
    # labs and theme
    labs(y = "",
         x = "Latitude",
         fill = "Jaccard\nsimilarity",
         colour = "Jaccard\nsimilarity") +
    
    theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          # strip.background = element_blank(),
          # strip.text.y = element_text(face = "bold",
          #                             angle = 0),
          legend.position = "none",
          aspect.ratio = col_ar) +
    theme_common
}

plots$jaccard$`Species collapse` <-
  
  plots$jaccard$`Species collapse` +
  labs(y = "JS") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text())

plots$jaccard$`Global extinction` <-
  plots$jaccard$`Global extinction` +
  labs(y = "JS") +
  theme(axis.ticks.x = element_line(),
        axis.text.x = element_text())

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
plots$richness <-
  list()

for(group in cluster_names$kmeans_cluster_name){
  
  group_colour <-
    cluster_names |> 
    filter(kmeans_cluster_name == group) |> 
    pull(kmeans_colour)
  
  plots$richness[[group]] <-
    ggplot(map_data |> 
             filter(kmeans_cluster_name == group)) +
    
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
    facet_wrap(~str_wrap(kmeans_cluster_name,15),
               ncol = 1,
               strip.position = "left") +
    
    labs(#title = str_wrap("Mean species richness at 2100",wrap_characters),
      y = "",
      x = "") +
    
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
          strip.background = element_rect(fill = alpha(group_colour,0.5)),
          # strip.text = element_blank(),
          # axis.ticks = element_line(colour = "transparent"),
          # axis.text = element_text(colour = "transparent"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    
    theme_common 
  
}

plots$richness$`Species collapse` <-
  plots$richness$`Species collapse` +
  labs(x = "Mean species richness") +
  theme(axis.title.x = element_text(),
        axis.ticks.x = element_line(colour = "transparent"),
        axis.text.x = element_text(colour = "transparent"))

plots$richness$`Global extinction` <-
  plots$richness$`Global extinction` +
  labs(x = "Mean species richness") +
  theme(axis.title.x = element_text(),
        axis.ticks.x = element_line(colour = "transparent"),
        axis.text.x = element_text(colour = "transparent"))

# compile plots ----------------------------------------------------------------
plots$composite <-
  list()

# each group
for(group in 1:6){
  
  plots$composite[[group]] <-
    (plots$richness[[group]] +
       plots$absolute[[group]] +
       plots$jaccard[[group]]) +
    plot_layout(widths = comp_widths)
}

# each column
column_1 <-
  (plots$composite[[1]] /
     plots$composite[[3]] /
     plots$composite[[5]])

column_2 <-
  (plots$composite[[2]] /
     plots$composite[[4]] /
     plots$composite[[6]])

final_plot <-
  (column_1 | column_2) &
  plot_annotation(tag_levels = list("c","")) &
  theme(plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(0, "pt"))

# export -----------------------------------------------------------------------
scalar <-
  1.25

ggsave("./plots/latitudinal_diversity_gradient/categorised_LDG_compact.jpg", #output
       final_plot,
       height = 90 * scalar,
       width = 225 * scalar,
       units = "mm",
       dpi = 300)






