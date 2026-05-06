# This script takes the per species per cell metrics and aggregates them into
# per cell metrics.

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(parallel)

# directories
dir_input <-
  "/storage/gen_future/results/01_species_dfs/" #input

dir_output <-
  "/storage/gen_future/results/02_aggregate_metrics_species/" #output

# if not present, create a results sub-folder
if(!file.exists(dir_output)){
  dir.create(dir_output)
}

# load -------------------------------------------------------------------------
# timesteps to years
years_timesteps <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F) |> 
  mutate(year = parse_number(year))

# mean sst values with cell ID and coordinates
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds") #input

seascape <-
  seascape[["sst_mean"]] |> 
  as_tibble(rownames = "cell") |> 
  pivot_longer(cols = contains("y_"),
               names_to = "year",
               values_to = "sst_mean") |> 
  filter(!is.na(sst_mean)) |> 
  mutate(cell = cell,
         year = parse_number(year))

# per cell ---------------------------------------------------------------------
spdfs <-
  list.files(dir_input)


mclapply(spdfs,
         mc.cores = 85, # low memory computations so can crank up the CPU
         function(spdf_file){
           
           # extract run and time steps from file name
           indices <-
             spdf_file |> 
             gsub(pattern     = "spdf_|.rds",
                  replacement = "") |> 
             strsplit("_") |> 
             unlist()
           
           run_id <-
             indices[2]
           
           timestep <-
             indices[3]
           
           # read species data frame
           spdf <-
             readRDS(paste0(dir_input,spdf_file))
           
           # check to see if there is global extinction
           if(dim(spdf)[1] > 0){
             
             # cluster metrics
             cluster_metrics <-
               spdf |> 
               group_by(run_id,
                        timestep,
                        species,
                        cluster_id) |> 
               
               reframe(
                 cluster_size  = n(), # the size of each cluster
                 cluster_thermal_mean = mean(thermal_optimum), # mean thermal optimum in a cluster
                 cluster_thermal_variance = var(thermal_optimum) # the variance in thermal optima in a cluster
               ) |> 
               group_by(run_id,
                        timestep,
                        species) |> 
               
               reframe(
                 cluster_size_max  = max(cluster_size), # the largest cluster size per species
                 cluster_size_mean = mean(cluster_size), # the mean cluster size per species
                 cluster_size_min  = min(cluster_size), # the minimum cluster size per species
                 
                 cluster_count     = n(), # the number of clusters per species
                 
                 cluster_thermal_variance_mean = mean(cluster_thermal_variance) # mean thermal variance in clusters across a species
               )
             
             # species metrics
             spp_metrics <-
               spdf |> 
               group_by(run_id,
                        timestep,
                        species) |> 
               
               # add environment
               left_join(years_timesteps, by = "timestep") |> 
               left_join(seascape, by = c("cell","year")) |> 
               
               
               # aggregate primary metrics
               # do this before the join to reduce object size
               reframe(
                 
                 # per species abundance
                 abundance_total = sum(abundance), # total abundance per species
                 abundance_mean  = mean(abundance), # mean abundance per species
                 
                 # thermal optima per species
                 thermal_opt_mean = mean(thermal_optimum), # the mean thermal optimum per species
                 thermal_sd_mean = sd(thermal_optimum), # the sd of the thermal optima per species
                 thermal_range_global = max(thermal_optimum)-min(thermal_optimum), # range of thermal optima values per species
                 thermal_variance_global = var(thermal_optimum), # variance of thermal optima per species
                 
                 # range
                 range_size = n(), # number of cells occupied by a species
                 
                 # calculate mean thermal mismatch per species
                 tolerance_mismatch = mean(abs(sst_mean-thermal_opt_mean)) # mean difference between the thermal optima and local SST
                 
               ) |> 
               
               # add cluster metrics
               left_join(cluster_metrics,
                         by = c("run_id","timestep","species")) |> 
               
               # fragmentation
               mutate(fragmentation = cluster_count/range_size) # the number of cells per cluster, if clusters were all the same size
             
             
             # export -----------------------------------------------------------------------
             saveRDS(spp_metrics,
                     paste0(dir_output,
                            "02_aggregate_metrics_species_",
                            run_id,"_",
                            timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(spdf,
                     paste0(dir_output,
                            "02_aggregate_metrics_species_",
                            run_id,"_",
                            timestep,".rds"))
             
           }
           
           
         }) # end of mclapply