#
# calculate cluster PD within a species
# returns a PD value for a single species
# requires picante
# requires tidyr
# Thomas Keggin
#

# find pd instead of mean divergence -------------------------------------------

clusterPD <-
  function(species_single){
    
    # dependencies
    require(picante)
    require(tidyr)
    
    # check if there is a cluster object
    if(is.null(species_single$traits[,"cluster_id"])){
      print("No clusters found in species object.")
      stop()
    }
    
    # find cluster IDs
    cluster_ids <-
      species_single$traits[,"cluster_id"]
    
    # set to 0 if there are no comparisons
    if(length(unique(cluster_ids)) < 2){
      cluster_PD <- 0
    } else {
      
      # find all comparisons
      comparisons <-
        combn(unique(cluster_ids),
              2) |> 
        t()
      
      # find mean of each comparison
      cluster_distances <-
        clusterDistances(species_single)
      
      # If there are only 2 clusters (unrooted),
      # then the PD is the cell-cell distance
      if(length(cluster_distances) == 1){
        cluster_PD <- cluster_distances
      }else{
        
        # create cluster distance matrix
        comparisons            <- as.data.frame(comparisons)
        comparisons$divergence <- cluster_distances
        
        dist_obj <-
          xyToDistmat(comparisons) |> 
          as.dist()
        
        # create phylogeny
        pop_phylo <-
          as.phylo(hclust(dist_obj))
        
        # create dummy community matrix
        com_mat <- matrix(1,ncol = length(pop_phylo$tip.label))
        colnames(com_mat) <- 1:length(pop_phylo$tip.label)
        
        # calculate pd
        cluster_PD <- pd(com_mat,pop_phylo)[,1]
      }
      
      
    }
    
    return(cluster_PD)
  }

