#
# Function to extract species' IDs (or custom identifier) from a species object
# input: species object
# output: vector of IDs
#

speciesIDs <-
  function(species_object,
           id = 'id'){
    
    sapply(species_object,
           '[[',
           id)
    
  }


