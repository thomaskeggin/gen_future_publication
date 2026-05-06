# set --------------------------------------------------------------------------
library(tidyverse)

# load and wrangle -------------------------------------------------------------
step_years <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = FALSE)

delta_metrics <-
  read_csv("./results/timeseries/delta_metrics_simulation.csv", #input
           show_col_types = FALSE) |> 
  left_join(step_years, by = "timestep") |> 
  mutate(year = gsub("y_","",year) |> as.numeric())

delta_sst <-
  read_csv("./results/timeseries/delta_temperature_windows.csv", #input
           show_col_types = FALSE)

# wrangle ----------------------------------------------------------------------
# join sst to metrics
delta_model <-
  delta_metrics |>
  left_join(delta_sst,
            relationship = "many-to-many")

# aggregate across years, metrics, and windows
export_me <-
  delta_model |> 
  ungroup() |> 
  group_by(year,metric,window) |> 
  reframe(delta     = mean(delta,na.rm = T),
          delta_sst = mean(delta_sst,na.rm = T))

# export -----------------------------------------------------------------------
saveRDS(export_me,
        "./results/timeseries/delta_wrangled_for_modelling.rds") #output
