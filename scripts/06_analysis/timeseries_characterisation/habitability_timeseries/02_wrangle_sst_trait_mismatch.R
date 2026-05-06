# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# thermal traits
mismatch_v_time <-
  readRDS("./results/timeseries_characterisation/habitability_timeseries/01_mismatch_v_time.rds") #input

# wrangle ----------------------------------------------------------------------
# aggregate traits to year
mismatch_year <-
  mismatch_v_time |> 
  group_by(year) |> 
  reframe(thermal_optimum_mean = mean(thermal_optimum),
          thermal_optimum_sd   = sd(thermal_optimum),
          thermal_optimum_max  = max(thermal_optimum),
          thermal_optimum_min  = min(thermal_optimum),
          thermal_sd = mean(thermal_sd),
          sst_sd   = sd(sst_mean),
          sst_mean = mean(sst_mean),
          mismatch = mean(mismatch))

# yearly traits, long
mismatch_year_long_mean <-
  mismatch_year |> 
  
  # pivot longer
  pivot_longer(cols = contains("mean"),
               names_to = "mean_metrics",
               values_to = "mean_values") |> 
  
  select(year,mean_metrics,mean_values) |> 
  
  mutate(variable = gsub("_.*","",mean_metrics))

# mean values
mismatch_year_long_mean <-
  mismatch_year |> 
  
  # pivot longer
  pivot_longer(cols = contains("mean"),
               names_to = "mean_metrics",
               values_to = "mean_values") |> 
  
  select(year,mean_metrics,mean_values) |> 
  
  mutate(variable = gsub("_.*","",mean_metrics)) |> 
  select(-mean_metrics)

# sd values
mismatch_year_long_sd <-
  mismatch_year |> 
  select(-thermal_sd) |> 
  
  # pivot longer
  pivot_longer(cols = contains("sd"),
               names_to = "sd_metrics",
               values_to = "sd_values") |> 
  
  select(year,sd_metrics,sd_values) |> 
  
  mutate(variable = gsub("_.*","",sd_metrics)) |> 
  select(-sd_metrics)

# combine means and deviations
mismatch_year_long <-
  left_join(mismatch_year_long_mean,
            mismatch_year_long_sd) |> 
  mutate(variable = ifelse(variable == "thermal",
                           "Thermal\noptimum",
                           "Sea surface\ntemperature"))

# export -----------------------------------------------------------------------
saveRDS(mismatch_year_long,
        "./results/timeseries_characterisation/habitability_timeseries/02_sst_trait_mismatch.rds") #output
