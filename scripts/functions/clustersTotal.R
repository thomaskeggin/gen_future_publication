#
# calculate the total number of clusters for each species in a species object
# returns a vector of summed clusters per species
# requires species objects to have a clusters attribute
# Thomas Keggin
#

clustersTotal <- function(species,
                          id = "id"){
  clusters_total <- c()
  for(sp in 1:length(species)){
    x        <- species[[sp]]$clusters
    clusters_total <- c(clusters_total,length(unique(x)))
  }
  names(clusters_total) <- unlist(sapply(species, FUN=function(x){x[[id]]})) # assign species IDs
  return(clusters_total)
}
