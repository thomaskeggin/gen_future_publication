#
# This script produces a genetic distance tree between cluster within a single
# species object.
# Requires cluster IDs to be in the traits matrix.
# Requires gen3sis
# Thomas Keggin
#

clusterDistances <-
  function(species_single){
    
    # check if there is a population object
    if(is.null(species_single$traits[,"cluster_id"])){
      print("No cluster IDs found in species object.")
      stop()
    }
    
    # cluster IDs for cells
    cluster_ids <-
      species_single$traits[,"cluster_id"]
    
    # extract genetic distance matrix
    divergence <-
      species_single$divergence
    
    n <- 
      length(species_single$divergence$index)
    
    decompressed_matrix <- 
      matrix(0, nrow = n, ncol = n)
    
    gen_dist_mat <-
      divergence$compressed_matrix[species_single$divergence$index, 
                                   species_single$divergence$index, drop = FALSE]
    
    dimnames(gen_dist_mat) <-
      list(names(species_single$divergence$index),
           names(species_single$divergence$index))
    
    
    
    # set to 0 if there are no comparisons
    if(length(unique(cluster_ids)) < 2){
      cluster_distances <- NA
    } else {
      
      # pairwise cluster combinations
      cluster_combos <-
        combn(unique(cluster_ids),
              2) |> 
        t()
      
      # run loop
      cluster_distances <-
        rep(NA,dim(cluster_combos)[1])
      
      for(combo in 1:dim(cluster_combos)[1]){
        
        # cluster IDs
        cluster_i <-
          cluster_combos[combo,1]
        cluster_j <-
          cluster_combos[combo,2]
        
        # cell IDs
        cells_i <-
          names(cluster_ids[cluster_ids == cluster_i])
        cells_j <-
          names(cluster_ids[cluster_ids == cluster_j])
        
        gen_dist_sub <-
          gen_dist_mat[cells_i,
                       cells_j]
        
        # find mean cell-cell distances per cluster comparion
        cluster_distances[combo] <-
          mean(gen_dist_sub)
        
      } # end of cluster combo loop
    }
    return(cluster_distances)
    
  }



