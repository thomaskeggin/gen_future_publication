# This script loops through each time step of each simulation and
# extracts a data frame for each species object for a per cell results table.
# The output is a data frame where each row is a single cell for a species in
# a time step for a simulation.
# Thomas Keggin 

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(progress)

# functions
dir_fun <-
  "./scripts/functions/" #input

for(f in list.files(dir_fun)){
  
  source(paste0(dir_fun,f))
}

# simulation directory
dir_batch <-
  "/storage/gen_future/output/partial_homogenisation/" #input

# output directory
dir_output <-
  "/storage/gen_future/results/01_species_dfs/" #output

# if not present, create a results sub-folder
if(!file.exists(dir_output)){
  dir.create(dir_output)
}

# runs
runs <-
  list.files(dir_batch)

# compile the paths of every species object to iterate over
species_paths <-
  c()

for(run in runs){
  
  # find all the species files in a simulation
  species_files <-
    list.files(paste0(dir_batch,run,"/species/"))
  
  # subset by time step subset
  species_files_sub <-
    tibble(file     = species_files) |> 
    mutate(timestep = parse_number(file)) |> 
    filter(timestep %in% timesteps_i) |> 
    pull(file)
  
  # add path to vector
  species_paths <-
    c(species_paths,
      paste0(dir_batch,run,"/species/",species_files_sub))
}

# loop ---------------------------------------------------------------------
parallel::mclapply(species_paths,
                   mc.cores = 10,
                   function(species_path){
                     
                     # extract run and time step from species path
                     run      <- parse_number(species_path)
                     timestep <- gsub(".*species","",species_path) |> parse_number()
                     
                     # load in and apply species data frame function
                     species_df <-
                       speciesDF(species_object = readRDS(species_path),
                                 id             = "species_name")
                     
                     # skip if all species are extinct
                     if(dim(species_df)[1] > 0){
                       
                       species_df <-
                         species_df |> 
                         mutate(run_id   = run,
                                timestep = timestep,
                                .before  = 1)
                       
                     } # end of if statement
                     
                     # output timestep
                     saveRDS(species_df,
                             paste0(dir_output,"01_spdf_",run,"_",timestep,".rds"))
                     
                     gc(verbose = FALSE)
                   } # end of species object loop
) # end of mclapply
