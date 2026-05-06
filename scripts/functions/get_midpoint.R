# return the midpoint of a numeric bin factor

get_midpoint <-
  function(x){
    
    x1 <- 
      x |> as.character() |> str_split(",")
    
    x2 <-
      lapply(x1,
             readr::parse_number)
    
    x3 <-
      lapply(x2,mean) |> 
      unlist()
    
    return(x3)
  }
