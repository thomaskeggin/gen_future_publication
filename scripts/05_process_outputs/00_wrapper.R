# This script iteratively runs the pipeline, appending time steps to the final 
# metric tables. This is due to the large intermediate files sizes.
all_timesteps <-
  c(0:86)

for(timesteps_i in 86){
  
  # 01 from raw outputs --------------------------------------------------------
  # extract information from the species object outputs
  source("./scripts/05_process_outputs/01_extract_species_output.R")
  
  # calculate the network metrics
  source("./scripts/05_process_outputs/01_network_metrics.R")
  
  # 02 aggregated metrics ------------------------------------------------------
  # aggregate to cell and species
  source("./scripts/05_process_outputs/02_aggregate_metrics_cell.R")
  source("./scripts/05_process_outputs/02_aggregate_metrics_species.R")
  
  # aggregate to each time step in each simulation
  # dependent on cell and species aggregation outputs
  source("./scripts/05_process_outputs/02_aggregate_metrics_simulation.R")
  
  # 03 phylogenetic metrics -------------------------------------------------------
  # per simulation
  #source("./scripts/05_process_outputs/03_phylogenetic_metrics_simulation.R")

  # per cell
  #source("./scripts/05_process_outputs/03_phylogenetic_metrics_cell.R")

  # 04 compile levels ----------------------------------------------------------
  # compile all cell and species aggregated metrics
  source("./scripts/05_process_outputs/04_compile_metrics_cell.R")
  source("./scripts/05_process_outputs/04_compile_metrics_species.R")

  # compile all simulation aggregated metrics
  # dependent on cell and species metric compilations
  source("./scripts/05_process_outputs/04_compile_metrics_simulation.R")

  # remove intermediate files --------------------------------------------------
  # results directory
  results_dir <-
    "/storage/gen_future/results/"

  # list all results directories
  results_dirs <-
    list.files(results_dir)

  # target non-final results
  destroy_me <-
    results_dirs[!grepl("04",results_dirs)]

  # destroy intermediate files
  for(destroy_dir in destroy_me){

    unlink(paste0(results_dir,destroy_dir),
           recursive = TRUE)
  }
  
}

# calculate delta metrics (final timestep vs initial timestep)
# source("./scripts/05_process_outputs/05_delta_metrics_cell.R")
# source("./scripts/05_process_outputs/05_delta_metrics_species.R")
# source("./scripts/05_process_outputs/05_delta_metrics_simulation.R")





