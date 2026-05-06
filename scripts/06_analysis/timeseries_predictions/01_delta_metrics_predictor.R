# This script calculates the change in metric values over each time step

# set --------------------------------------------------------------------------
library(tidyverse)
library(progress)

# load -------------------------------------------------------------------------
# load delta sst for the time windows
windows <-
  read_csv("./results/timeseries/delta_temperature_windows.csv", #input
           show_col_types = F) |> 
  pull(window) |> 
  unique()

# delta metrics list
delta_list <-
  list()

# results directory
dir_res <-
  "./results/04_metrics_simulation/" #input

# timesteps
timesteps <-
  gsub("04_metrics_simulation_","",list.files(dir_res)) |> 
  parse_number() |> 
  sort()

# table of all metrics and runs to fill in extinct runs
all_metrics <-
  read_csv(paste0(dir_res,"04_metrics_simulation_86.csv"),
           show_col_types = FALSE) |> 
  select(-c(run_id,timestep)) |> 
  colnames()

all_runs <-
  expand.grid(1:500,all_metrics) |> 
  as_tibble() |> 
  rename(run_id = Var1,
         metric = Var2) |> 
  mutate(metric_value = NA)

pb <-
  progress_bar$new(total = length(windows),
                   format = ":percent [:bar]")

# loop through time windows
for(w in windows){
  pb$tick()
  
  # loop through each delta timstep
  for(t in timesteps[1:(length(timesteps)-w)]){
    
    # filler data frames
    runs_latest   <- all_runs |> mutate(timestep = t)
    runs_previous <- all_runs |> mutate(timestep = t+w)
    
    # latest timestep
    t_latest <-
      read_csv(paste0(dir_res,"04_metrics_simulation_",t,".csv"),
               show_col_types = FALSE) |> 
      pivot_longer(cols = -c(run_id,timestep),
                   names_to = "metric",
                   values_to = "metric_value")
    
    t_latest_missing <-
      runs_latest |>
      filter(run_id %in% which(!1:500 %in% unique(t_latest$run_id)))
    
    # previous timestep
    t_previous <-
      read_csv(paste0(dir_res,"04_metrics_simulation_",t+w,".csv"),
               show_col_types = FALSE) |> 
      pivot_longer(cols = -c(run_id,timestep),
                   names_to = "metric",
                   values_to = "metric_value")
    
    t_previous_missing <-
      runs_previous |> 
      filter(run_id %in% which(!1:500 %in% unique(t_previous$run_id)))
    
    
    # bind them all together for the delta calculation
    delta_metrics <-
      do.call(rbind.data.frame,list(t_latest,
                                    t_latest_missing,
                                    t_previous,
                                    t_previous_missing)) |> 
      pivot_wider(names_from = timestep,
                  values_from = metric_value)
    
    delta_metrics$delta <-
      unlist(delta_metrics[,3]) - unlist(delta_metrics[,4])
    
    
    delta_list[[paste0("w_",w)]][[paste0("t_",t)]] <-
      delta_metrics |> 
      select(run_id,metric,delta) |> 
      mutate(timestep = t,
             window = w)
  }
  
  delta_list[[paste0("w_",w)]] <-
    do.call(rbind.data.frame,delta_list[[paste0("w_",w)]])
  
}

delta_df <-
  do.call(rbind.data.frame,delta_list)
  

# export -----------------------------------------------------------------------
saveRDS(delta_df,
          "./results/timeseries/delta_metrics_simulation_windows.rds") #output






