# How does species richness change latitudinally with climate change?

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(progress)
library(data.table)
library(ggpmisc)

# colour scheme
source("C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/colour_palettes.R") #input


# functions
dir_function <- c("./scripts/functions/")
for(file in list.files(dir_function)){
  
  source(paste0(dir_function,"",file))
}


# load -------------------------------------------------------------------------
# species initialisation
species_info <-
  readRDS("./data_processed/species/species_initialisation_information.rds") #input

# parameters
params <-
  readRDS("./results/categorised_parameters.rds")  #input

# seascape
sea <-
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] |> #input
  select(x,y) |> 
  as_tibble(rownames = "cell") |> 
  mutate(cell = as.numeric(cell))

# initial species richness -----------------------------------------------------
initial_sr_sim_list <-
  list()

for(sp in names(species_info)){
  
  initial_sr_sim_list[[sp]] <-
    tibble(species = sp,
           cell    = as.numeric(names(species_info[[sp]]$abundance)),
           present = 1)
  
}

initial_sr_sim <-
  do.call(rbind.data.frame,
          initial_sr_sim_list) |> 
  group_by(cell) |> 
  reframe(species_richness = sum(present)) |> 
  left_join(sea) |> 
  group_by(y) |> 
  reframe(richness_initial = mean(species_richness))

# calculate cell metrics -------------------------------------------------------
cell_metric_dir <-
  "./results/04_metrics_cell/" #input

cell_metric_files <-
  list.files(cell_metric_dir)

latitudinal_metrics <-
  list()

pb <-
  progress_bar$new(total = length(cell_metric_files),
                   format = ":current of :total :percent [:bar] :eta")
# loop
for(file in cell_metric_files){
  
  pb$tick()
  
  latitudinal_metrics[[file]] <-
    
    fread(paste0(cell_metric_dir,file)) |> 
    
    # group by latitude
    group_by(run_id,year,y) |> 
    
    # mean richness per degree
    reframe(richness_mean = mean(richness),
            richness_median = median(richness))
  
  gc()
}

latitudinal_metrics_df <-
  do.call(rbind.data.frame,latitudinal_metrics)

latitudinal_metrics_df <-
  latitudinal_metrics_df |> 
  left_join(params) |> 
  mutate(hemisphere = sign(y)) |> 
  right_join(initial_sr_sim)

# calculate LDG metrics --------------------------------------------------------
# latitudinal peak richness and variance of richness across occupied cells.
peak_metrics <-
  latitudinal_metrics_df |>
  group_by(run_id,year,hemisphere) |> 
  
  mutate(
    
    # count the number of occupied latitudinal degrees 
    n = n(),
    
    # fit a loess curve to find hemisphere peaks
    smoothed = ifelse(n <10, max(richness_mean),
                      
                      loess(richness_mean ~ y,
                            span = 0.5) |> predict()),
    
    
    # standard deviation of richness (peak strength)
    sd = sd(richness_mean),
    
    # find maximum per hemisphere
    maxima = ifelse(smoothed == max(smoothed),
                    TRUE,
                    FALSE)
    
  ) |> 
  
  filter(maxima == TRUE)

# export -----------------------------------------------------------------------
# cell metrics
saveRDS(latitudinal_metrics_df,
          "./results/latitudinal_diversity_gradient/latitudinal_metrics.rds") #output

# LDG metrics
saveRDS(peak_metrics,
          "./results/latitudinal_diversity_gradient/peak_metrics.rds") #output

# initial latitudinal richness
saveRDS(initial_sr_sim,
        "./results/latitudinal_diversity_gradient/initial_latitudinal_richness.rds") #output





