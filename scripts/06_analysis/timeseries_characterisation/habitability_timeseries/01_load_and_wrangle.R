# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# species information
species <-
  readRDS("./data_processed/species/species_initialisation_information.rds") #input

sp_list <- list()

for(sp in names(species)){
  
  sp_list[[sp]] <-
    tibble(species_name = sp,
           cell = species[[sp]]$cells,
           thermal_optimum = species[[sp]]$traits |>
             as_tibble() |>
             pull(thermal_optimum),
           thermal_sd = species[[sp]]$traits |>
             as_tibble() |>
             pull(thermal_sd))
}

sp_df <-
  do.call(rbind.data.frame,
          sp_list)

# sea surface temperature
sst <- 
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] |> #input
  as_tibble(rownames = "cell") |> 
  na.omit() |> 
  
  pivot_longer(cols = y_2100:y_2014,
               names_to = "year",
               values_to = "sst_mean") |> 
  
  select(cell,year,sst_mean) |> 
  
  mutate(year = gsub("y_","",year) |> as.numeric())

# wrangle ----------------------------------------------------------------------
mismatch_v_time <-
  sp_df |>
  left_join(sst,
            relationship = "many-to-many") |> 
  
  mutate(mismatch = sst_mean - thermal_optimum)

# export -----------------------------------------------------------------------
saveRDS(mismatch_v_time,
        "./results/timeseries_characterisation/habitability_timeseries/01_mismatch_v_time.rds") #output

