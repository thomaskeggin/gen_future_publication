# this script takes the species object outputs and aggregates them to give a 
# value for each simulation.

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(parallel)

# directories
dir_spdf <-
  "/storage/gen_future/results/01_species_dfs/" #input

dir_cell <-
  "/storage/gen_future/results/02_aggregate_metrics_cell/" #input

dir_species <-
  "/storage/gen_future/results/02_aggregate_metrics_species/" #input

dir_output <-
  "/storage/gen_future/results/02_aggregate_metrics_simulation/" #output

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

# per simulation ---------------------------------------------------------------------
spdfs <-
  list.files(dir_spdf)

agg_cell <-
  list.files(dir_cell)

agg_spp <-
  list.files(dir_species)


mclapply(1:length(spdfs),
         mc.cores = 85, # low memory computations so can crank up the CPU
         function(iteration){
           
           spdf_file <- spdfs[iteration]
           cell_file <- agg_cell[iteration]
           spp_file  <- agg_spp[iteration]
           
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
             readRDS(paste0(dir_spdf,spdf_file))
           
           # check to see if there is global extinction
           if(dim(spdf)[1] > 0){
             
             # primary simulation metrics ----------------------------------------
             simulation_metrics_primary <-
               
               # read species data frame
               spdf |> 
               
               # group by run and timestep to keep columns
               group_by(run_id,
                        timestep) |> 
               
               # add environment
               left_join(years_timesteps, by = "timestep") |> 
               left_join(seascape, by = c("cell","year")) |> 
               
               # aggregate and calculate metrics
               # do this before the join to reduce object size
               reframe(
                 
                 # per species abundance
                 abundance_global_total = sum(abundance), # total global abundance
                 abundance_cell_mean    = mean(abundance), # mean cell abundance
                 
                 # thermal optima per species
                 thermal_opt_mean = mean(thermal_optimum),
                 thermal_opt_sd = sd(thermal_optimum),
                 
                 # range
                 occupied_cells = length(unique(cell)),
                 
                 # calculate mean thermal mismatch per species
                 tolerance_mismatch = mean(abs(sst_mean-thermal_opt_mean)),
                 
                 # species richness
                 species_richness_global = length(unique(species))
                 
               )
             
             
             # aggregate species simulation metrics (group by species first) -----
             simulation_metrics_species <-
               
               # read in species metrics
               readRDS(paste0(dir_species,spp_file))  |>
               
               # group by time step and simulation
               group_by(run_id,
                        timestep) |> 
               
               reframe(
                 
                 # abundance per species
                 abundance_species_mean = mean(abundance_mean),
                 
                 # range size per species
                 range_size_mean = mean(range_size),
                 
                 fragmentation_mean = mean(fragmentation),
                 
                 # cluster metrics
                 cluster_size_mean_mean  = mean(cluster_size_mean), # mean of the mean cluster sizes across species
                 cluster_size_max_mean   = mean(cluster_size_max), # mean of the max cluster sizes across species
                 cluster_size_min_mean   = mean(cluster_size_min), # mean of the min cluster sizes across species
                 cluster_count_mean      = mean(cluster_count), # mean number of clusters across species
                 
                 cluster_thermal_variance_mean_mean = mean(cluster_thermal_variance_mean) # mean variance across species of the mean variance of thermal optima of clusters withing a species
                 
               ) |> 
               
               left_join(simulation_metrics_primary,
                         by = c("run_id","timestep"))
             
             
             # aggregate cell simulation metrics (group by cell first) -----------
             simulation_metrics_cell <-
               
               # read in cell metrics
               readRDS(paste0(dir_cell,cell_file)) |>
               
               # group by time step and simulation
               group_by(run_id,
                        timestep) |> 
               
               reframe(
                 
                 # mean species richness per cell
                 species_richness_cell_mean = mean(richness)
                 
               ) |> 
               
               left_join(simulation_metrics_species,
                         by = c("run_id","timestep"))
             
             # export ------------------------------------------------------------
             saveRDS(simulation_metrics_cell,
                     paste0(dir_output,
                            "02_aggregate_metrics_simulation_",
                            run_id,"_",
                            timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(spdf,
                     paste0(dir_output,
                            "02_aggregate_metrics_simulation_",
                            run_id,"_",
                            timestep,".rds"))
           }
           
         }) # end of mclapply loop





