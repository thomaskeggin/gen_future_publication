# set --------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)
library(igraph)
library(ggraph)

caribbean <-
  ext(-99.5,-50.5,0.5,39.5)

# load -------------------------------------------------------------------------
# seascape
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] #input

# distances
distances <-
  readRDS("./data_processed/seascapes/distances_full/distances_full_0.rds") #input

# dispersal thresholds
dispersal_thresholds <-
  readRDS("./results/dispersal_thresholds.rds")*1000 #input

# coastlines
coastlines <-
  vect("./data/coastlines/gshhg-shp-2.3.7/GSHHS_shp/c/GSHHS_c_L1.shp") |>  #input
  crop(caribbean)

# wrangle ----------------------------------------------------------------------
cell_subset <-
  seascape |>
  as_tibble(rownames = "cell") |> 
  select(cell,x,y) |> 
  
  # caribbean
  filter(x > caribbean[1],
         x < caribbean[2],
         y > caribbean[3],
         y < caribbean[4]) |> 
  pull(cell)

# convert distances to data frame
distances_df <-
  distances |> 
  
  # convert to tibble
  as_tibble(rownames = "cell") |> 
  
  # subset
  filter(cell %in% cell_subset) |> 
  
  #pivot longer to allow easy filtering
  pivot_longer(cols = -cell,
               names_to = "cell_2",
               values_to = "m")


# choose a peak buffer
buffer = 25000

peaks <-
  graphs <-
  graph_coords <- 
  habitat <-
  plots <-
  list()

for(i in 1:length(dispersal_thresholds)){
  
  peaks[[i]] <-
    distances_df |> 
    
    # apply peak filters
    filter(
      m > (dispersal_thresholds[i] - buffer) &
        m < (dispersal_thresholds[i] + buffer)
    ) |> 
    
    mutate(peak = dispersal_thresholds[i])

  # create graph
  graphs[[i]] <-
    graph_from_data_frame(peaks[[i]],
                          directed = FALSE)
  
  # add coordinates
  graph_coords[[i]] <-
    seascape |> 
    select(x,y) |> 
    as.matrix()
  
  graph_coords[[i]] <-
    graph_coords[[i]][V(graphs[[i]])$name,]
  
  # cells 
  habitat[[i]] <-
    seascape[,1:3] |>
    as_tibble(rownames = "cell") |> 
    na.omit() |> 
    # caribbean
    filter(x > caribbean[1],
           x < caribbean[2],
           y > caribbean[3],
           y < caribbean[4]) |> 
    mutate(peaked = cell %in% unique(peaks[[i]]$cell))
  
  # layout
  layout <-
    create_layout(graphs[[i]],
                  layout = graph_coords[[i]])
  
  # plot
  plots[[i]] <-
    ggraph(graphs[[i]], layout = layout) +
    
    # coasts
    geom_spatvector(data = coastlines) +
    
    # cells
    geom_node_point(data = habitat[[i]],
                    aes(colour = peaked),
                    size = 0.5) +
    
    # edges
    geom_edge_fan(strength = 2,
                  alpha = 0.5,
                  edge_width = 0.25) +
    
    # theme
    labs(x = "",
         y = "") +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(family = "serif")) +
    scale_colour_manual(values = c("#CC6677",
                                   "#44AA99")) 
  
  # ggsave(paste("./plots/thresholds/", ##output
  #              dispersal_thresholds[i],
  #              ".png"),
  #        height = 5,
  #        width = 5)
}

# export -----------------------------------------------------------------------
saveRDS(plots,
        "./plots/thresholds/02_dispersal_maps.rds") #output






















