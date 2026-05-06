# function to return all file sizes in a directory into a data frame

fileSizes <-
  function(directory,
           unit = "Mb"){
    
    units <-
      c(B = 1,
        Kb = 1024,
        Mb = 1024^2,
        Gb = 1024^3)
    
    files <- 
      list.files(directory)
    
    file_paths <-
      paste(directory,
            files,
            sep = "/")
    
    return(
      
      dplyr::tibble(file = files,
                    size = sapply(file_paths,file.size)/units[unit])
    )
    
  }
