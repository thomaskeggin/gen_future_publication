#
# This function converts a pairwise distance data frame to a distance matrix.
# Thomas Keggin / chatGPT4
#

xyToDistmat <-
  function(xydf){
    
    froms <- xydf[,1]
    tos   <- xydf[,2]
    
    points <- unique(c(froms,tos))
    
    # Create an empty distance matrix
    dist_matrix <- matrix(NA, nrow = length(points), ncol = length(points),
                          dimnames = list(points, points))
    
    # Fill in the distance matrix
    for (i in 1:nrow(xydf)) {
      from <- as.character(xydf[i,1])
      to   <- as.character(xydf[i,2])
      dist <- xydf[i,3]
      
      dist_matrix[from, to] <- dist
      dist_matrix[to, from] <- dist  # because the distance matrix is symmetric
    }
    
    diag(dist_matrix) <- 0
    
    return(dist_matrix)
    
  }
