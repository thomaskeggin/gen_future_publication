# Function to find the maximum thermal range of a species from the trait data
# frame in the species object.
# Input: single species in a species object
# output: vector with the upper and lower bounds

find_thermal_bounds <-
  function(target_species,
           extinction_threshold){
    
    # Return NA if the species is extinct
    if(length(target_species$traits) == 0){
      return(NA)
    }else{
    
    # common standard deviation
    sd_trait <- 
      unique(target_species$traits[,"thermal_sd"])
    
    # upper thermal limit
    max_trait <-
      max(target_species$traits[,"thermal_optimum"])
    
    # Calculate the maximum density of the normal distribution
    max_density <-
      dnorm(max_trait, mean = max_trait, sd = sd_trait)
    
    # Calculate the density threshold
    density_threshold <-
      max_density * extinction_threshold
    
    # Find the values at which the normal distribution has the desired density
    upper_bound <-
      c(qnorm(density_threshold, mean = max_trait, sd = sd_trait,lower.tail = F))
    
    # lower thermal limit
    min_trait <-
      min(target_species$traits[,"thermal_optimum"])
    
    # Calculate the maximum density of the normal distribution
    max_density <-
      dnorm(min_trait, mean = min_trait, sd = sd_trait)
    
    # Calculate the density threshold
    density_threshold <-
      max_density * extinction_threshold
    
    # Find the values at which the normal distribution has the desired density
    lower_bound <-
      c(qnorm(density_threshold, mean = min_trait, sd = sd_trait,lower.tail = T))
    
    bounds <-
      c(lower = lower_bound,
        upper = upper_bound)
    
    return(bounds)
    }
  }
