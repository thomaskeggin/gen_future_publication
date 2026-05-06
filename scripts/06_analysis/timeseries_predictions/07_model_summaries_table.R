# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# extinctions
extinctions <-
  readRDS("./results/timeseries/model_extinctions.rds") #input

# extinctions by pop metrics
extinctions_by_popmetrics <-
  readRDS("./results/timeseries/model_extinction_by_popmetrics.rds") #input

# occupied cells
occupied_cells <-
  readRDS("./results/timeseries/model_occupied_cells.rds") #input

# cohesion
cohesion <-
  readRDS("./results/timeseries/model_cohesion.rds") #input

# range size
range_size_mean <-
  readRDS("./results/timeseries/model_range_size_mean.rds") #input

# fragementation
fragmentation_mean <-
  readRDS("./results/timeseries/model_fragmentation_mean.rds") #input

# betweenness
betweenness_mean <-
  readRDS("./results/timeseries/model_betweenness_mean.rds") #input

# combine model summaries ------------------------------------------------------
# clean linear models
summaries <-
  list(extinctions_by_popmetrics$model_information |> 
         mutate(model_fit = "Extinctions from ecological metrics"),
       occupied_cells$model_information |> 
         mutate(model_fit = "Occupied cells from SST"),
       cohesion$model_information |> 
         mutate(model_fit = "Cohesion from SST"),
       range_size_mean$model_information |> 
         mutate(model_fit = "Range size from SST"),
       fragmentation_mean$model_information |> 
         mutate(model_fit = "Metapopulation fragmentation from SST"),
       betweenness_mean$model_information |> 
         mutate(model_fit = "Betweenness from SST")) |> 
  bind_rows() |> 
  relocate(model_fit, .before = everything()) |> 
  
  # clean column names
  rename(Model = model_fit,
         `Time Window` = time_window,
         `Adj. R Squared` = r_adj,
         `P-value` = p,
         `Degrees of Freedom`= df,
         `Estimate` = est) |> 
  
  select(-c(significant,r))

# clean segmented models
seg_summaries <-
  
  extinctions$model_information |> 
  
  mutate(model_fit = "Extinctions from SST") |> 
  
  relocate(model_fit, .before = everything()) |>
  
  # clean column names
  rename(Model = model_fit,
         `Time Window` = time_window,
         `Break Estimate` = seg_break_est,
         `Break Estimate SE` = seg_break_err,
         `First Segment Estimate` = seg_est_1,
         `Second Segment Estimate` = seg_est_2,
         `First Segment P-value` = seg_p_1,
         `Second Segment P-value` = seg_p_2,
         `Adj. R Squared` = seg_r_adj,
         `Degrees of Freedom` = df) |> 
  
  select(-c(seg_r))

# export -----------------------------------------------------------------------
write_csv(seg_summaries |> 
            mutate(model_fit = "Extinctions from SST"),
          "./results/timeseries/model_summaries_seg_lin.csv") #output

write_csv(summaries,
          "./results/timeseries/model_summaries_lin.csv") #output
