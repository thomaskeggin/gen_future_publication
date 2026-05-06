# set --------------------------------------------------------------------------
# package
library(tidyverse)
library(patchwork)
library(ggpmisc)

# bins for histogram
distance_bins <-
  500

# functions
dir_function <- c("./scripts/functions/")
for(file in list.files(dir_function)){
  
  source(paste0(dir_function,"",file))
}

# load -------------------------------------------------------------------------
# distance matrices
dist_mat <-
  readRDS("./data_processed/seascapes/distances_full/distances_full_0.rds") #input

# results
results <-
  readRDS("./results/categorised_parameters.rds") |>  #input 
  left_join(read_csv("./results/04_metrics_simulation/04_metrics_simulation_0.csv", #input
                     show_col_types = F))

# regions
regions <-
  read_csv("./data_processed/realms/01_realm_ids.csv", #input
           show_col_types = F)

# wrangle dispersal ------------------------------------------------------------
# remove arctic
non_arctic_cells <-
  regions |> 
  filter(realm != "Arctic") |> 
  pull(cell) |> 
  as.character()

distances <-
  dist_mat[non_arctic_cells,non_arctic_cells]

# convert distances into a data frame and into km
distances <-
  distances[upper.tri(distances, diag = F)] / 1000

distances <-
  distances[distances <= max(results$dispersal_range)] |> 
  as_tibble() |> 
  rename(dispersal_requirement = value)

# Find peaks
# To fit a LOESS model to a distribution, we need to manually bin distances to 
# provide distance as a predictor variable, and n observations as a response.
distances_binned <-
  distances |> 
  
  # group distances into 500 bins 
  mutate(bin = cut(distances$dispersal_requirement,
                   breaks = distance_bins)) |> 
  group_by(bin) |> 
  reframe(n=n()) |> 
  
  # find the midpoint distance value 
  mutate(midpoint = unlist(lapply(bin,get_midpoint)),
         
         # find peaks in cell-to-cell distances
         smoothed = predict(loess(n ~ midpoint,
                                  span = 0.1)),
         maxima = find_peaks(smoothed),
         maxima = ifelse(smoothed < 100, FALSE, maxima))

dispersal_thresholds <-
  distances_binned |> 
  filter(maxima == T) |> 
  pull(midpoint)

# export -----------------------------------------------------------------------
# dispersal thresholds
saveRDS(dispersal_thresholds,
        "./results/dispersal_thresholds.rds") #output