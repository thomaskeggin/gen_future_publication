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
  "/storage/gen_future/results/02_aggregate_metrics_cell/" #output

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
             gsub(pattern     = "01_spdf_|.rds",
                  replacement = "") |> 
             strsplit("_") |> 
             unlist()
           
           run_id <-
             indices[1]
           
           timestep <-
             indices[2]
           
           # read species data frame
           spdf <-
             readRDS(paste0(dir_input,spdf_file))
           
           # check to see if there is global extinction
           if(dim(spdf)[1] > 0){
             
             # aggregate metrics
             cell_metrics <-
               
               # read species data frame
               spdf |> 
               
               # group by cell (run_id and timestep so they are retained)
               group_by(run_id,timestep,cell) |> 
               
               # aggregate and calculate metrics
               # do this before the join to reduce object size
               reframe(
                 
                 # species richness
                 richness = n(),
                 
                 # per cell abundance
                 abundance_total = sum(abundance),
                 abundance_mean  = mean(abundance),
                 
                 # thermal optima mean per cell
                 thermal_opt_mean = mean(thermal_optimum)
               ) |> 
               
               # add year information
               left_join(years_timesteps,
                         by = "timestep") |> 
               
               # add environment
               left_join(seascape, by = c("cell","year")) |> 
               
               # calculate mean suitability per cell
               mutate(tolerance_mismatch = abs(sst_mean-thermal_opt_mean))
             
             # export ------------------------------------------------------------
             saveRDS(cell_metrics,
                     paste0(dir_output,
                            "02_aggregate_metrics_cell_",
                            run_id,"_",
                            timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(spdf,
                     paste0(dir_output,
                            "02_aggregate_metrics_cell_",
                            run_id,"_",
                            timestep,".rds"))
             
           }
           
         }) # end of mclapply


