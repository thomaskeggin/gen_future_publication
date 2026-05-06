#
# Create an abundance tibble
# Extract abundances from a species object and collate them into a data frame with
# species | cell | abundance columns
# requires tydr

createAbundanceDF <-
  function(species_object,
           species_id = "id",
           cluster_id = FALSE){
    
    # dependencies
    require(tidyr)
    
    # list for the abundances of each species
    abundances <-
      list()
    
    # loop across species
    for(sp in 1:length(species_object)){
      
      # skip globally extinct species
      if(length(species_object[[sp]]$abundance) > 0){
        
        # data frame per species with species identifier, cell, and abundance
        abundances[[sp]] <-
          data.frame(species     = species_object[[sp]][[species_id]],
                     cell        = as.numeric(names(species_object[[sp]]$abundance)),
                     abundance   = species_object[[sp]]$abundance,
                     row.names   = NULL)
        
        # add cluster ID if the option is checked
        if(cluster_id == TRUE){
          abundances[[sp]] <-
            abundances[[sp]] |> 
            mutate(cluster = species_object[[sp]]$clusters)
          
        }
        
        
      }
    }
    
    # return a collated data frame with all species
    return(do.call(rbind.data.frame,abundances))
    
  }
