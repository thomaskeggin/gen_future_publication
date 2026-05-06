#
# identify extant species from a species object
# returns a vector TRUE/FALSE
# Thomas Keggin
#

speciesExtant <- 
  function(species){
  
  species_extant <- 
    rep(NA,length(species))
  
  for(sp in 1:length(species)){
    
    ifelse(length(species[[sp]]$abundance) > 0,
           species_extant[sp] <- TRUE,
           species_extant[sp] <- FALSE)
 
  }
  
  return(species_extant)
}
