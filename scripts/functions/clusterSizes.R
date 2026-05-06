#
# Function to return the size of clusters for each species in a species object
# input: species object
# output: data frame of species | cluster | size
# dependent on speciesExtant() and speciesIDs

clusterSizes <-
  function(species_object,
           id = "id"){
    
    spp <-
      species_object[speciesExtant(species_object)]
    
    spp_ids <-
      speciesIDs(spp,id)
    
    cluster_sizes <-
      list()
    
    for(sp in 1:length(spp)){
      
      # check if the species object has clusters
      if(is.null(spp[[sp]]$clusters)){
        print(
          paste("No clusters found in species:",
                spp[[sp]][[id]]))
        stop()
      }
      
      cluster_sizes[[sp]] <-
        data.frame(species      = spp_ids[sp],
                   cluster_id   = names(table(spp[[sp]]$clusters)),
                   cluster_size = as.numeric(table(spp[[sp]]$clusters)))
      
    }
    
    return(
      do.call(rbind.data.frame,cluster_sizes)
    )
    
    
  }
