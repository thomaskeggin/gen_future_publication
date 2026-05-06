#
# calculate cluster divergences within a species
# returns a vector of the means of all divergences between all clusters in a single species
# dependent on species edited by clusterDetermine()
# requires data.table
# Thomas Keggin
#

# find pd instead of mean divergence -------------------------------------------

clusterMPD <-
  function(species_single){
    
    # dependencies
    require(data.table)
    
    # check if there is a cluster object
    if(is.null(species_single$clusters)){
      print("No clusters found in species object.")
      stop()
    }
    
    # decompress divergence matrices
    decompressed <-
      species_single$divergence$compressed_matrix[as.character(species_single$divergence$index),
                                                  as.character(species_single$divergence$index),
                                                  drop = F]
    
    cell_ids <-
      names(species_single$divergence$index)
    
    dimnames(decompressed) <-
      list(cell_ids,cell_ids)
    
    # find mean of each inter-cluster comparison
    # find cluster IDs
    clusters <-
      unique(species_single$clusters)
    
    # set mean to 0 if there are no comparisons
    if(length(clusters) < 2){
      cluster_mpd <- 0
    } else {
      
      # find all cluster-cluster comparisons
      comparisons <-
        combn(clusters,
             2) |> 
        t() |> 
        as.data.table()
      
      # matrix to store divergence values (faster than vector building)
      cluster_divergences <-
        matrix(data=rep(NA,dim(comparisons)[1]),
               ncol=1)
      
      # all cluster ids
      pop_all <- species_single$clusters
      
      for(comp in 1:dim(comparisons)[1]){
        
        pop_a <- comparisons[[comp,1]] # first pop id
        pop_b <- comparisons[[comp,2]] # second pop id
        
        # subset cells by clusters
        pop_a_cells <- names(pop_all[pop_all == pop_a])
        pop_b_cells <- names(pop_all[pop_all == pop_b])
        
        # create asymmetrical distance matrix
        decompressed_subset <-
          decompressed[pop_a_cells,pop_b_cells,
                       drop=F]
        
        # the mean of the cell-cell distances between clusters
        cluster_divergences[comp,1] <-
          mean(decompressed_subset)
      }
      
      cluster_mpd <-
        mean(cluster_divergences)
      
    }
    
    return(cluster_mpd)
  }

