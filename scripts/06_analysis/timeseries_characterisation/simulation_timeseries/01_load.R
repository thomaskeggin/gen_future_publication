# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# timesteps to years
t2y <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F) |> 
  mutate(year = gsub("y_","",year) |> as.numeric())

# simulation metrics
dir_sim_metrics <-
  "./results/04_metrics_simulation/" #input

files_sim_metrics <-
  list.files(dir_sim_metrics)

sim_metrics <-
  list(dir_sim_metrics)

for(i in 1:length(files_sim_metrics)){
  
  # read in metrics
  sim_i <-
    read_csv(paste0(dir_sim_metrics,
                    files_sim_metrics[i]),
             show_col_types = F) 
  
  # bind into data frame with parameters
  sim_metrics[[i]] <-
    parameters |> 
    left_join(sim_i,
              by = "run_id") |> 
    mutate(timestep = unique(sim_i$timestep))
    
}

sim_metrics_df <-
  do.call(rbind.data.frame,
          sim_metrics) |> 
  left_join(t2y, by = "timestep")

# export -----------------------------------------------------------------------
saveRDS(sim_metrics_df,
        "./results/timeseries_characterisation/simulation_timeseries/01_simulation_metrics.rds") #output
