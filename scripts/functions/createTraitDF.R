#
# Create a trait table for all extant species
# Input is a species object and an option for the species ID
# Output is a data frame with:
# cell | species | trait1...n
# Requires extantSpecies() to remove extinct species

createTraitDF <-
  function(species_object,
           species_id = "id"){
    
    species_extant <-
      as.numeric(speciesExtant(species_object))
    
    # number of extant species in the object
    n_sp <-
      length(species_extant)
    
    # create a list structure to put the trait data into
    trait_list <-
      vector(mode = "list",
             length = n_sp)
    
    # loop through all the species to extract the trait matrices
    for(sp in 1:n_sp){
      
      sp_id <-
        species_extant[sp]
      
      trait_list[[sp]] <-
        data.frame(
          
          # species name
          species = species_object[[sp_id]][[species_id]],
          
          # cell ID
          cell = as.numeric(rownames(species_object[[sp_id]]$traits)),
          
          # trait information
          species_object[[sp_id]]$traits
          
        )
    }
    
    # collate into single data frame
    trait_df <-
      do.call(rbind.data.frame,trait_list)
    
    return(trait_df)
    
    
  }
