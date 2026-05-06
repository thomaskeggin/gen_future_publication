#
# Function to return the mean least-cost distances between species clusters in
# a species object.
#
# input: single species from a species object with clusters assigned.
#        distance matrix
# output: data frame with cluster-cluster distances
#         cluster_i | cluster_j | distance
#
# NA values are given where there is only 1 cluster.

# for a single species within a species object
clusterPairwiseGeographicDistances <-
  function(species_single,
           distance_matrix){
    
    # check to see if there are clusters
    if(is.null(species_single$clusters)){
      print("No clusters found in species object.")
      stop()
    }
    
    # see if there are any pairwise comparisons
    if(length(unique(species_single$cluster)) == 1){
      return(NA)
    }else{
      
      
      # extract cluster information
      cluster_information <-
        data.frame(cell = names(species_single$cluster),
                   cluster = species_single$cluster)
      
      # pairwise cluster comparisons
      cluster_pairwise <-
        combn(unique(species_single$cluster),2) |> 
        t()
      
      # run loop
      cluster_distances <-
        rep(NA,dim(cluster_pairwise)[1])
      
      for(combo in 1:dim(cluster_pairwise)[1]){
        
        # cluster IDs
        cluster_i <-
          cluster_pairwise[combo,1]
        cluster_j <-
          cluster_pairwise[combo,2]
        
        # cell IDs
        cells_i <-
          names(
            species_single$clusters[species_single$clusters == cluster_i])
        cells_j <-
          names(species_single$clusters[species_single$clusters == cluster_j])
        
        dist_mat_sub <-
          distance_matrix[cells_i,
                          cells_j]
        
        
        cluster_distances[combo] <-
          mean(dist_mat_sub)
        
      }
      
      return(
        data.frame(cluster_i = cluster_pairwise[,1],
                   cluster_j = cluster_pairwise[,2],
                   distance = cluster_distances)
      )
    }
    
  }

# the mean pairwise distance for each species in a species object.
clusterPairwiseGeographicDistancesMean <-
  function(species_object,
           distance_matrix,
           id = "id"){
    
    # vectors to hold values
    mean_distances <-
      rep(NA,length(species_object))
    
    species_ids <-
      rep(NA,length(species_object))
    
    # start species loop
    for(sp in 1:length(species_object)){
      
      # pairwise
      pairwise_distances_sp <-
        clusterPairwiseGeographicDistances(
          species_single = species_object[[sp]],
          distance_matrix = distance_matrix)
      
      if(is.data.frame(pairwise_distances_sp)){
        mean_distances[sp] <-
          mean(pairwise_distances_sp$distance)
      }
      
      species_ids[sp] <-
        species_object[[sp]][[id]]
      
    }
    
    return(
      data.frame(species_id = species_ids,
                 distance   = mean_distances)
    )
    
    
  }
