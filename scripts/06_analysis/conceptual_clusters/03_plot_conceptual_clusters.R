# set --------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)
library(ggnewscale)

# load -------------------------------------------------------------------------
# species information
species_information_evo <-
  readRDS("./results/conceptual_clusters/02_spp_info_evo.rds") #input

# dispersal polygons
dispersal_polygons <-
  vect("./results/conceptual_clusters/02_range_polygons.gpkg") #input

# coastlines
coastlines <-
  vect("./data/coastlines/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp") #input

# remove adaptive comparison?
species_information_evo <-
  species_information_evo |> 
  filter(adaptive_cat == "low adaptive rate") |> 
  mutate(dispersal_cat = gsub("high dispersal",
                              "High Dispersal",
                              dispersal_cat),
         dispersal_cat = gsub("low dispersal",
                              "Low Dispersal",
                              dispersal_cat))

dispersal_polygons <-
  dispersal_polygons |> 
  filter(adaptive_cat == "low adaptive rate") |> 
  mutate(dispersal_cat = gsub("high dispersal",
                              "High Dispersal",
                              dispersal_cat),
         dispersal_cat = gsub("low dispersal",
                              "Low Dispersal",
                              dispersal_cat))

# crop coastlines --------------------------------------------------------------
coastlines_cropped <-
  crop(coastlines,
       ext(dispersal_polygons))

# reformat species information for thermal space plots -------------------------
thermal_space_data <- 
  species_information_evo |> 
  pivot_longer(contains("thermal_optimum_"),
               names_to = "stage",
               values_to = "thermal_optimum_changed") |> 
  mutate(stage          = factor(stage,levels = c("thermal_optimum_start",
                                                  "thermal_optimum_adapted",
                                                  "thermal_optimum_homogenised")),
         
         # for the plot labels
         `Adaptive Stage` = as.character(stage),
         `Adaptive Stage` = gsub("thermal_optimum_start", "1. Start",`Adaptive Stage`),
         `Adaptive Stage` = gsub("thermal_optimum_adapted", "2. Local adaptation only",`Adaptive Stage`),
         `Adaptive Stage` = gsub("thermal_optimum_homogenised", "3. Local adaptation and trait homogenisation",`Adaptive Stage`),
         `Adaptive Stage` = factor(`Adaptive Stage`,
                                   levels = c("1. Start",
                                              "2. Local adaptation only",
                                              "3. Local adaptation and trait homogenisation"))
  )

# common plot elements ---------------------------------------------------------
# common elements
point_size <- 3
point_shape <- 21
start_alpha <- 0.5
null_fill <- "white"
limits <- c(26,28)
custom_theme <- theme_minimal()

# theme
theme_tk <-
  
  theme_classic() +
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent",
                                             colour = "transparent"),
        strip.background = element_rect(fill = "transparent"),
        text = element_text(family = "serif",
                            size = 10 * 1.5))

# colour scheme
flow_colours <-
  c("Causes maladaptation" = "#4D262D",
    "Inhibits adaptation" = "#CC6677",
    "No trait homogenisation" = "white",
    "Enhances adaptation" = "#88CCEE",
    
    "Local adaptation only" = "grey",
    "Initial trait value" = "transparent")

# plot start map ---------------------------------------------------------------
# plot
start_map <-
  ggplot() +
  
  geom_spatvector(data = coastlines_cropped) +
  
  geom_tile(data = species_information_evo,
            aes(x = x,
                y = y),
            fill = "darkgrey",
            colour = "black",
            width = 0.85,
            height = 0.85) +
  
  theme_tk

# plot trait homogenisation maps ----------------------------------------------------------
plot_gene_flow_map <-
  ggplot() +
  
  # dispersal polygons
  geom_spatvector(data = dispersal_polygons,
                  fill = alpha("white",0.5)) +
  
  # coastlines
  geom_spatvector(data = coastlines_cropped,
                  fill = "#FFF7E6",
                  colour = alpha("black",0.5)) +
  
  # habitat tiles
  geom_tile(data = species_information_evo,
            aes(x = x,
                y = y,
                fill = gene_flow_consequence),
            colour = "black",
            width = 0.85,
            height = 0.85) +
  
  # layout
  facet_grid(rows = vars(dispersal_cat)) +
  
  # colours and theme
  scale_fill_manual(values = flow_colours) +
  
  labs(x = "",
       y = "") +
  
  theme_tk +
  theme(legend.position = "none",
        strip.background = element_rect(colour = alpha("black",0.5),
                                        fill=NA,
                                        linewidth=0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(colour = alpha("black",0.1),
                                    fill=NA,
                                    linewidth=0.5))

# bar plots --------------------------------------------------------------------
# bar plot
plot_bar <-
  ggplot(species_information_evo) +
  
  geom_bar(aes(x = gene_flow_consequence_group,
               fill = gene_flow_consequence),
           colour = "black",
           position = "stack") +
  
  # layout
  facet_grid(rows = vars(dispersal_cat)) +
  
  coord_flip() +
  
  lims(y = c(0,15)) +
  
  # colours and theme
  scale_fill_manual(values = flow_colours) +
  
  theme_tk +
  
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) 

# pie chart
pie_data <-
  species_information_evo |> 
  group_by(dispersal_cat,gene_flow_consequence) |> 
  reframe(n = n())

plot_pie <-
  
  ggplot(pie_data,
         aes(x = "",
             y = n,
             fill = gene_flow_consequence)) +
  
  geom_bar(stat = "identity",
           width = 1,
           colour = "black") +
  
  coord_polar("y",
              start = 0) +
  
  facet_grid(rows = vars(dispersal_cat)) +
  
  # colours and theme
  scale_fill_manual(values = flow_colours) +
  
  theme_void() +
  
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank()) 

# plot cells vs thermal space --------------------------------------------------
# put the cells in thermal order
cell_order <- 
  species_information_evo |> 
  ungroup() |> 
  select(cell, sst_mean) |> 
  distinct() |> 
  arrange(sst_mean) |> 
  pull(cell)

# some wrangling (placeholder)
cell_thermal <-
  species_information_evo |> 
  
  # reorder the cells into thermal order
  mutate(cell = factor(cell,
                       levels = cell_order))

cell_thermal_stages <-
  cell_thermal |> 
  
  # pivot longer to Thermal trait and SST to legend
  pivot_longer(cols = c(thermal_optimum_start,
                        thermal_optimum_adapted),
               names_to = "Thermal trait",
               values_to = "thermal_trait_value") |> 
  mutate(`Thermal trait` = gsub("thermal_optimum_adapted",
                                    "Local adaptation only",
                                    `Thermal trait`),
         `Thermal trait` = gsub("thermal_optimum_start",
                                    "Initial trait value",
                                    `Thermal trait`),
         `Thermal trait` = factor(`Thermal trait`,
                                      levels = c("Initial trait value",
                                                 "Local adaptation only")))


# cell v thermal
plot_cell_thermal <-
  ggplot(cell_thermal) +
  
  # sst mean
  geom_line(aes(x = cell,
                y = sst_mean,
                group = cluster_id),
            alpha = 0.5,
            linetype = "dashed") +
  
  # start colours
  scale_fill_manual(values = flow_colours) +
  scale_colour_manual(values = flow_colours) +
  
  # start
  geom_point(data = cell_thermal_stages,
             aes(x = cell,
                 y = thermal_trait_value,
                 fill = `Thermal trait`),
             size = point_size-1,
             shape = point_shape,
             alpha = start_alpha) +
  
  # adaptation to trait homogenisation lines
  geom_segment(aes(x = cell,
                   y = thermal_optimum_adapted,
                   yend = thermal_optimum_homogenised,
                   colour = gene_flow_consequence)) +
  
  # start
  geom_point(data = cell_thermal_stages |> 
               filter(`Thermal trait` == "Local adaptation only"),
             aes(x = cell,
                 y = thermal_trait_value,
                 fill = `Thermal trait`),
             size = point_size-1,
             shape = point_shape) +
  
  # separate the legend for before and after trait homogenisation
  new_scale_fill() +
  scale_fill_manual(values = flow_colours) +
  
  # homogenised
  geom_point(aes(x = cell,
                 y = thermal_optimum_homogenised,
                 fill = gene_flow_consequence),
             shape = 22,
             size = point_size+1) +
  
  # layout
  facet_grid(rows = vars(dispersal_cat)) +
  
  scale_y_continuous(position = "left") +
  
  labs(x = "Habitat patch",
       y = "Temperature",
       fill = "Consequence of\ntrait homogenisation",
       colour = "Consequence of\ntrait homogenisation") +
  
  theme_tk +
  
  theme(axis.text.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.border = element_rect(colour = alpha("black",0.1),
                                    fill=NA,
                                    linewidth=0.5))

# composite plot ---------------------------------------------------------------
composite_plot <-
  plot_cell_thermal +
  plot_gene_flow_map +
  plot_layout(widths = c(2,2),
              guides = "collect") +
  plot_annotation(tag_levels = "a")

# export -----------------------------------------------------------------------
# main figure
ggsave("./plots/conceptual_clusters/conceptual_clusters.png", #output
       composite_plot,
       height = 180*(9/13)*1.4,
       width = 180*1.4,
       units = "mm",
       dpi = 300)

# pie charts
ggsave("./plots/conceptual_clusters/pie_chart.png", #output
       plot_pie,
       height = 7,
       width = 3)




