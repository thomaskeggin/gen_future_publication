# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
mismatch_v_time <-
  readRDS("./results/timeseries_characterisation/habitability_timeseries/01_mismatch_v_time.rds") #input

# apply ecology function -------------------------------------------------------
# apply the ecology function given maximum starting abundance to see at which 
# year species would be driven to extinction.

# extinction_times
extinction_times <-
  mismatch_v_time |> 
  
  mutate(
    
    # start abundance
    start_abundance = 1, 
    
    # the distribution density if the species' niche perfectly fits the environment
    optimal_density = dnorm(thermal_optimum,
                            mean = thermal_optimum,
                            sd = thermal_sd),
    
    # the distribution density given the distance between species niche and environment
    species_density = dnorm(thermal_optimum,
                            mean = sst_mean,
                            sd = thermal_sd),
    
    # adjust the carrying capacity based on thermal suitability
    carrying_capacity = species_density/optimal_density,
    
    # set the density dependence term for the logistic growth model
    density_dependence = 1-(start_abundance/carrying_capacity),
    
    # set the growth rate (this is the same as the carrying capacity)
    growth_rate = carrying_capacity,
    
    # density dependent logistic growth function
    growth = growth_rate * start_abundance * density_dependence,
    
    # modify abundance
    end_abundance = start_abundance + growth,
    
    end_abundance = ifelse(end_abundance > carrying_capacity,
                           carrying_capacity,
                           end_abundance),
    
    # drive to (local) extinction if abundance is below 10%
    end_abundance = ifelse(end_abundance < 0.1,
                           0,
                           end_abundance),
    
    # drive to (local) extinction if the environment is completely unsuitable
    end_abundance = ifelse(species_density == 0,
                           0,
                           end_abundance)
  ) |> 
  
  # remove extinct cell / years
  filter(end_abundance > 0) |> 
  
  # just say whether or not the species is extant
  select(species_name,year) |> 
  distinct() |> 
  arrange(species_name,year) |> 
  
  # keep only continuously surviving species
  group_by(species_name) |> 
  mutate(extant = 1,
         years_since_start = year - 2013,
         years_extant = cumsum(extant),
         continuously_extant = years_since_start == years_extant) |> 
  
  filter(continuously_extant == T) |> 
  filter(year == max(year)) |> 
  select(species_name,
         year) |> 
  rename(species = species_name)

# export -----------------------------------------------------------------------
saveRDS(extinction_times,
        "./results/timeseries_characterisation/habitability_timeseries/02_extinction_times.rds") #output
