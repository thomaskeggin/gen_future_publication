# This script converts all metrics into "delta metrics" - the difference between
# 2014 and 2100.

# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# years to timestep
years <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F)


# load in final time step
sim_metrics_0 <-
  read_csv("/storage/gen_future/results/04_metrics_simulation/04_metrics_simulation_0.csv", #input
           show_col_types = F) |> 
  select(-timestep) |> 
  pivot_longer(cols = -c(run_id),
               names_to = "metric",
               values_to = "y_2100")

# load in first time step
sim_metrics_86 <-
  read_csv("/storage/gen_future/results/04_metrics_simulation/04_metrics_simulation_86.csv", #input
           show_col_types = F) |> 
  select(-timestep) |> 
  pivot_longer(cols = -c(run_id),
               names_to = "metric",
               values_to = "y_2014")

# wrangle ----------------------------------------------------------------------
delta_metrics <-
  left_join(sim_metrics_0,
            sim_metrics_86,
            by = c("run_id","metric")) |> 
  
  mutate(delta_metric_value = y_2100 - y_2014)

# export -----------------------------------------------------------------------
write_csv(delta_metrics,
          "/storage/gen_future/results/05_delta_metrics_simulation.csv") #output

