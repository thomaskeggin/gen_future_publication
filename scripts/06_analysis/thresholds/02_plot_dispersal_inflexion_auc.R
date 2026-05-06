# set --------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# plots
plots <- list()

# common theme
common <-
  scale_x_continuous(breaks = seq(0, 550, 50),
                     limits = c(0,551))

common_theme <-
  theme(#strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    text = element_text(family = "serif"))


# load -------------------------------------------------------------------------
# distance matrix
dist_mat <-
  readRDS("./data_processed/seascapes/distances_full/distances_full_0.rds") #input

# find lowest threshold
sim_metrics <-
  read_csv("./results/04_metrics_simulation/04_metrics_simulation_0.csv", #input
           show_col_types = F) |> 
  left_join(readRDS("./results/categorised_parameters.rds")) #input

# regions
regions <-
  read_csv("./data_processed/realms/01_realm_ids.csv", #input
           show_col_types = F)

# response thresholds
thresholds_response <-
  c(45,
    readRDS("./results/dispersal_thresholds.rds")) #input

# metric names
metric_names <-
  read_csv("./results/metric_descriptions.csv", #input
           show_col_types = F) |> 
  filter(grepl("sim",file))

# wrangle cell to cell distances -----------------------------------------------
# remove arctic
non_arctic_cells <-
  regions |> 
  filter(realm != "Arctic") |> 
  pull(cell) |> 
  as.character()

distances <-
  dist_mat[non_arctic_cells,non_arctic_cells]

# distances
distances <-
  distances[upper.tri(distances, diag = F)] / 1000

distances <-
  distances[distances <= 550] |> 
  as_tibble() |> 
  rename(dispersal_requirement = value)

thresholds_response_df <-
  tibble(threshold = thresholds_response[-1],
         rank = thresholds_response[-1] |> factor())

# AUC calculations -------------------------------------------------------------
dispersal_barriers <-
  thresholds_response[-1]

probabilities <-
  list()

for(barr in dispersal_barriers){
  
  probabilities[[paste0("b_",barr)]] <-
    tibble(barrier = barr,
           dispersal_range = 1:550) |> 
    
    mutate(probability = pweibull(barr,
                                  shape = 2,
                                  scale = dispersal_range,
                                  lower.tail = F))
  
}

probs_df <-
  do.call(rbind.data.frame,
          probabilities) |> 
  mutate(barrier = factor(barrier))

# plot cell to cell histogram --------------------------------------------------
# dispersal threshold
vline_alpha <- 0.5

# distance histogram
plots$histogram <-
  
  ggplot(distances) +
  
  # thresholds
  geom_vline(data = thresholds_response_df,
             aes(xintercept = threshold,
                 linetype = rank,
                 colour = rank),
             linewidth = 1) +
  
  # minimum
  geom_vline(xintercept = 35.42,
             colour = alpha("black",0.5),
             linewidth = 0.5) +
  
  # histogram
  geom_histogram(aes(x = dispersal_requirement),
                 bins = 200,
                 fill = alpha("black",0.5),
                 colour = "transparent") +
  
  # labels
  labs(y = str_wrap("Count of required distances in seascape",
                    15),
       x = "Cell to cell\ndistances (km)",
       colour = "Cell to cell\ndistances (km)",
       linetype = "Cell to cell\ndistances (km)") +
  
  # common scale
  common +
  
  # theme
  theme_bw() +
  theme(legend.position = "none") +
  common_theme


# plot auc ---------------------------------------------------------------------
plots$auc <-
  ggplot(probs_df) +
  
  geom_line(aes(x = dispersal_range,
                y = probability,
                group = barrier,
                colour = barrier,
                linetype = barrier),
            linewidth = 1) +
  
  
  labs(x = "Dispersal range (km)\nas scale parameter of the Weibull distribution",
       y = "Probability of sucessful\ndispersal attempt",
       colour = "Cell to cell\ndistances (km)",
       linetype = "Cell to cell\ndistances (km)") +
  
  common +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  
  theme_bw() +
  common_theme

# plot responses ---------------------------------------------------------------
# wrangle
sim_metrics_long <-
  sim_metrics |> 
  pivot_longer(cols = c(#abundance_cell_mean,
    #abundance_global_total,
    abundance_species_mean,
    #betweenness_mean,
    #cohesion_mean,
    #fragmentation_mean,
    #occupied_cells,
    range_size_mean,
    species_richness_cell_mean,
    species_richness_global
  ),
  names_to = "column",
  values_to = "response_value") |> 
  left_join(metric_names)

# plot
plots$response <-
  ggplot(sim_metrics_long) +
  
  # scatter plot
  geom_point(aes(x = dispersal_range,
                 y = response_value,
                 colour = `Adaptive rate (°C / year)`)) +
  
  # layout
  facet_grid(rows = vars(clean_name |> str_wrap(10)),
             scales = "free_y") +
  
  # theme
  scale_colour_gradient(high = "#0FC2C0",
                        low = "#023535") +
  labs(y = "",
       x = "Dispersal range (km / year)\nas scale parameter of the Weibull distribution") +
  
  common +
  theme_bw() +
  common_theme

# composite plot ---------------------------------------------------------------
plots$composite <-
  
  plots$histogram /
  #plot_spacer() /
  
  plots$auc /
  #plot_spacer() /
  
  plots$response  +
  
  plot_layout(
    #guides = "collect",
    axes = "collect",
    heights = c(0.25, ##0.05,
                0.75, #0.05,
                1)) +
  plot_annotation(tag_levels = "a")


# export -----------------------------------------------------------------------
saveRDS(plots,
        "./plots/thresholds/02_dispersal_inflexion.rds" #output
        )


# ggsave("./plots/thresholds/dispersal_thresholds.jpg", #output
#        plots$composite,
#        height = 11.7,
#        width = 8.3)




