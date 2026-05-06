# This script calculates the change in temperature per year, and in increasing 
# time windows (up to 40 years)

# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds") #input

steps_to_years <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F)

# wrangle the sst info ---------------------------------------------------------
sea <-
  seascape$sst_mean |> 
  na.omit() |> 
  as_tibble() |> 
  pivot_longer(cols = -c(x,y),
               names_to = "year",
               values_to = "sst") |> 
  mutate(year = as.numeric(gsub("y_","",year))) |> 
  group_by(year) |> 
  reframe(mean_sst = mean(sst)) |> 
  arrange(desc(year))

# find the maximum time window for our time series -----------------------------
# first year that is simulated
first_sim_year <-
  steps_to_years |> 
  mutate(year = as.numeric(gsub("y_","",year))) |> 
  filter(timestep == 86) |> 
  pull(year)

# the earliest year for which we have temperature data
first_temp_step <-
  min(sea$year)

# the greatest time window we can use without differences in sample size
maximum_window <-
  first_sim_year - first_temp_step

# calculate delta temperature for different windows ----------------------------
for(window in 1:maximum_window){
  
  sea[,paste0("delta_sst_",window)] <-
    NA
  
  for(row in 1:dim(sea)[1]){
    
    sea[row,paste0("delta_sst_",window)] <-
      sea$mean_sst[row] - sea$mean_sst[row+window]
    
  }
}

# compile
sea_delta <-
  sea |> 
  pivot_longer(cols = contains("delta"),
               names_to = "window",
               values_to = "delta_sst") |> 
  mutate(window = parse_number(window))

# export -----------------------------------------------------------------------
write_csv(sea_delta,
          "./results/timeseries/delta_temperature_windows.csv") #output
