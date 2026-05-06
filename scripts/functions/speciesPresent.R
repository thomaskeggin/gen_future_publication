#
# make a list of the species present from each cell from a landscape and a species object
# returns a list of cells with their constituent species
# Thomas Keggin
#


speciesPresent <-
  function(pa_matrix){
  
  # the number of cells
  no_cells <-
    dim(pa_matrix)[1]
  
  # create a list to store cell species list
  species_present <- 
    list()
  
  # loop through each cell
  for(i in rownames(pa_matrix)){
    
    spp_vector <-
      names(which(pa_matrix[i,-c(1:2)] == 1))
    
    if(length(spp_vector) > 0){
      
      species_present[[i]] <-
        data.frame(cell         = i,
                   species_name = spp_vector)
    }
  }
  
  species_present <-
    do.call(rbind.data.frame,
            species_present)
  
  return(species_present)
}



