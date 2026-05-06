# This function loads in all the species ranges from Conor's data and formats
# them into a data frame.

load_species_ranges <-
  function(range_directory,
           range_type,
           display_progress = TRUE){
    
    # list all range files
    files_range <- 
      list.files(range_directory)
    
    # read in all files in to a list
    list_range <- 
      list()
    
    # set progress bar
    if(display_progress == TRUE){
      print("loading range files")
      progress_bar <- 
        txtProgressBar(min=0, max=length(files_range), style = 3, char="-")
    }
    
    # start load loop
    for(file in 1:length(files_range)){
      
      # read rds
      list_range[[file]] <-
        readRDS(paste0(range_directory,files_range[file]))[[range_type]] |>
        
        # convert to data.frame
        data.frame()
      
      # add species name
      list_range[[file]]$species <-
        gsub("\\..*","",files_range[file])
      
      # update progress bar
      if(display_progress == TRUE){
        setTxtProgressBar(progress_bar, value = file)
      }
    }
    
    if(display_progress == TRUE){
      close(progress_bar)
      print("range files loaded")
    }
    
    
    # combine into single data frame
    df_range <-
      do.call(rbind.data.frame,
              list_range)
    
    return(df_range)
  }

