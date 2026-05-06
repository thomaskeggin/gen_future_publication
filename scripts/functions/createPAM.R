#
# function to create a presence/absence matrix from a species object and a
# landscape object.
# rows are cells, columns are xy and species
# Thomas Keggin
#

createPAM <- function(species,
                      scape,
                      distance_matrix){
  
  # extract coordinates from the scape object
  coords <-
    scape[[1]][rownames(distance_matrix), # keep only habitable cells
               c("x","y")] |>             # and xy coordinates
    as.matrix()
  
  # create an empty presence/absence data frame
  pa_matrix_empty <-
    matrix(
      0,
      nrow = dim(coords)[1], # row per cell
      ncol = length(species) # column per species
    )
  
  # add the coordinate information
  pa_matrix <-
    cbind(coords,
          pa_matrix_empty)
  
  # set column names as species IDs
  colnames(pa_matrix)[3:dim(pa_matrix)[2]] <-
    unlist(lapply(species, FUN=function(x){x$species_name}))
  
  # fill out the pa_matrix using abundance values
  for(i in 3:length(pa_matrix[1,])){
    
    pa_matrix[names(species[[i-2]]$abundance),i] <-
      1
  }
  
  return(pa_matrix)
}

