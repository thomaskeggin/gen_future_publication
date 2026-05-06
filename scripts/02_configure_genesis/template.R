# Meta -------------------------------------------------------------------------
# author: Thomas Keggin

# Parameter values -------------------------------------------------------------
dispersal_range   <- parameter_table$dispersal_range*1000 # convert from km to m
adaptive_rate     <- parameter_table$adaptive_rate
seed              <- parameter_table$seed 
start             <- 86 # 2015

# General settings -------------------------------------------------------------
# set the random seed for the simulation
random_seed = seed

# set the starting time step or leave NA to use the earliest/highest timestep
start_time = start

# set the end time step or leave as NA to use the lates/lowest timestep (0)
end_time = NA

# maximum total number of species in the simulation before it is aborted
max_number_of_species = 2000

# maximum number of species within one cell before the simulation is aborted
max_number_of_coexisting_species = 2000

# a list of traits to include with each species
# a "dispersion" trait is implictly added in any case
trait_names = c("cluster_id",
                "dispersal",
                "thermal_optimum",
                "thermal_sd",
                "neutral_1",
                "neutral_2",
                "neutral_3",
                "neutral_4")

# ranges to scale the input environments with:
# not listed variable:         no scaling takes place
# listed, set to NA:           the environmental variable will be scaled from [min, max] to [0, 1]
# listed with a given range r: the environmental variable will be scaled from [r1, r2] to [0, 1]
environmental_ranges = list()

# a place to inspect the internal state of the simulation and collect additional information if desired
end_of_timestep_observer = function(data, vars, config){
  save_richness()
  save_species()
}

# Initialisation ---------------------------------------------------------------
# the initial abundance of a newly colonized cell, both during setup and later when colonizing a cell during the dispersal
initial_abundance = 0.1

# place species within rectangle:
create_ancestor_species <- function(landscape, config) {
  
  #browser()
  
  # load in pre-compiled species information
  species_information <-
    readRDS("./data_processed/species/species_initialisation_information.rds")
  
  # extract species' names
  species_names <-
    names(species_information)
  
  # initialise species object
  species_object <-
    list()
  
  # loop through all species
  for(sp in 1:length(species_information)){
    
    # create species object with pre-compiled range information
    species_object[[sp]] <- 
      create_species(initial_cells = species_information[[sp]]$cells,
                     config = config)
    
    # generate mean thermal niche and standard deviation
    species_object[[sp]]$traits[,"thermal_optimum"] <-
      species_information[[sp]]$traits$thermal_optimum
    
    species_object[[sp]]$traits[,"thermal_sd"] <-
      species_information[[sp]]$traits$thermal_sd
    
    # neutral traits
    species_object[[sp]]$traits[ , "neutral_1"]   <- 0
    species_object[[sp]]$traits[ , "neutral_2"]   <- 0
    species_object[[sp]]$traits[ , "neutral_3"]   <- 0
    species_object[[sp]]$traits[ , "neutral_4"]   <- 0
    
    # assign common dispersal range to all species
    species_object[[sp]]$traits[ , "dispersal"] <-
      dispersal_range
    
    # assign species name
    species_object[[sp]]$species_name <-
      species_names[sp]
    
    # assign abundances
    species_object[[sp]]$abundance <-
      species_information[[sp]]$abundance
    
  }
  
  # output species object
  return(species_object)
  
}

# Dispersal --------------------------------------------------------------------
# returns n dispersal values
get_dispersal_values <- function(num_draws, species, landscape, config) {
  
  return(
    rweibull(num_draws,
             shape = 2,
             scale = dispersal_range)
  )
}

# Speciation -------------------------------------------------------------------
# threshold for genetic distance after which a speciation event takes place.
# speciation after every timestep : 0.9.
# we are removing the speciation dynamic by setting the threshold to infinity.
divergence_threshold = Inf

# factor by which the genetic distance is increased between geographically isolated population of a species
# can also be a matrix between the different population clusters
get_divergence_factor <- function(species, cluster_indices, landscape, config) {
  
  return(1)
}


# Evolution --------------------------------------------------------------------
# mutate the traits of a species and return the new traits matrix
apply_evolution <- function(species, cluster_indices, landscape, config){
  
  #browser()
  
  # load and wrangle -----------------------------------------------------------
  # extract trait names
  trait_names <-
    config$gen3sis$general$trait_names
  
  # neutral traits
  neutral_traits <-
    trait_names[grepl("neutral",trait_names)]
  
  # local environment
  local_environment <-
    dplyr::as_tibble(landscape$environment,rownames = "cell")
  
  # extract traits
  traits <-
    dplyr::as_tibble(species[["traits"]], rownames="cell") |> 
    
    # add cluster and abundance information
    dplyr::mutate(cluster_id = cluster_indices,
                  abundance  = species$abundance)
  
  # drift and thermal adaptation -----------------------------------------------
  # skip adaptive calculations if adaptive rate is 0
  if(adaptive_rate == 0){
  }else{
    
    traits <- 
      traits |> 
      
      # neutral drift
      dplyr::mutate(dplyr::across(all_of(neutral_traits),
                                  ~ .x + rnorm(dim(traits)[1],
                                               mean = 0,
                                               sd   = adaptive_rate))) |> 
      
      # add environmental values
      dplyr::left_join(local_environment,
                       by = "cell") |> 
      
      # thermal adaptation
      dplyr::mutate(
        
        # find difference between environment and trait
        mismatch = sst_mean - thermal_optimum,
        
        # direction of selection
        direction = sign(mismatch),
        
        # move the trait towards the environment, with noise added by a normal
        # probability distribution abs() and direction ensure direction of
        # adaptation
        thermal_trait_change = 
          abs(
            rnorm(dim(traits)[1],
                  mean=0,
                  sd=adaptive_rate)
          ) * direction,
        
        # prevent adaptation past the environment
        thermal_trait_change_cropped = 
          ifelse(abs(thermal_trait_change) > abs(mismatch),
                 mismatch,
                 thermal_trait_change),
        
        # apply directional adaptation
        thermal_optimum = thermal_optimum + thermal_trait_change_cropped
      )
  }
  
  # cluster homogenisation for all traits
  traits <-
    traits |> 
    
    # group by cluster ID
    dplyr::group_by(cluster_id) |> 
    
    # calculate the weighted mean of each cluster for each trait, then move
    # the trait values towards that mean by 50% of the difference.
    dplyr::mutate(dplyr::across(c(thermal_optimum,contains("neutral")),
                                ~ .x + ((weighted.mean(.x,abundance)-.x)*0.5))) |> 
    
    # trim back columns
    dplyr::select(cell,cluster_id,all_of(trait_names))
  
  # convert back to matrix
  traits_mat <-
    as.matrix(traits[,2:dim(traits)[2]])
  
  # reassign row names
  rownames(traits_mat) <-
    traits$cell
  
  return(traits_mat)
}

# Ecology ----------------------------------------------------------------------
# called for every cell with all occuring species, this function calculates who survives in the current cells
# returns a vector of abundances
# set the abundance to 0 for every species supposed to die
apply_ecology <- function(abundance, traits, local_environment, config) {
  
  #browser()
  
  new_abundance <-
    
    # trait information
    dplyr::as_tibble(traits) |> 
    
    # environmental information
    cbind(dplyr::as_tibble(local_environment)) |> 
    
    # ecology calculations
    dplyr::mutate(
      
      # start abundance
      start_abundance = abundance, 
      
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
    
    # extract end abundance only
    dplyr::pull(end_abundance)
  
  # assign cell names
  names(new_abundance) <- names(abundance)
  
  # fin
  return(new_abundance)
}




