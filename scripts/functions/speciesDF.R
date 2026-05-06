#
# Function to extract species object information and turn it into a data frame
# for easier analysis.
# Input: species object
# Output: species data frame
#

speciesDF <-
  function(species_object,
           id = "id"){
    
    # ignore extinct species
    spp_obj <-
      species_object[speciesExtant(species_object)]
    
    no_spp <-
      length(spp_obj)
    
    # if all species are extinct, return NA
    if(no_spp == 0){
      
      return(data.frame())
    } else {
      
      # to store the variables
      spp_df_list <-
        vector("list",no_spp)
      
      # loop through extant species
      for(sp in 1:no_spp){
        
        # if there are clusters in the object
        if("clusters" %in% names(spp_obj[[sp]])){
          spp_df_list[[sp]] <-
            data.frame(cell      = names(spp_obj[[sp]]$abundance),
                       species   = spp_obj[[sp]][[id]],
                       abundance = spp_obj[[sp]]$abundance,
                       cluster   = spp_obj[[sp]]$clusters,
                       spp_obj[[sp]]$traits)
        } 
        
        # if there are no clusters in the object
        else {
          
          spp_df_list[[sp]] <-
            data.frame(cell      = names(spp_obj[[sp]]$abundance),
                       species   = spp_obj[[sp]][[id]],
                       abundance = spp_obj[[sp]]$abundance,
                       spp_obj[[sp]]$traits)
        }
      } # end of species loop
      
      # compile into single data frame and return
      return(
        do.call(rbind.data.frame,spp_df_list)
      )
    } # end of else statement
  } # end of function
