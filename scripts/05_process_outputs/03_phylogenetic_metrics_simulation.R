# This script calculates the phylogenetic metrics per simulation at the species
# level.

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(fishtree)
library(picante)
library(progress)

# directories
dir_input <-
  "/storage/gen_future/results/02_aggregate_metrics_species/" #input

file_output <-
  "/storage/gen_future/results/03_phylogenetic_metrics_simulation.csv" #output

# subset the fishtree phylogeny
spp_names <-
  readRDS("./data_processed/species/species_initialisation_information.rds") |> #input
  names()

phylo_full <-
  fishtree_phylogeny(spp_names)

# set up assemblage indexing ---------------------------------------------------
input_files <-
  list.files(dir_input)

# list for each unique species assemblage
spp_assemblages <-
  vector("list",length(input_files))

# index to match to the assemblage list
assemblage_indices <-
  c(1,rep(NA,length(input_files)-1))

# metrics container
indices <-
  input_files |> 
  gsub(pattern     = "02_aggregate_metrics_species_|.rds",
       replacement = "") |> 
  strsplit("_")

run_ids <- timesteps <-
  rep(NA,length(indices))

for(i in 1:length(indices)){
  
  run_ids[i] <- indices[[i]][1]
  timesteps[i] <- indices[[i]][2]
  
}

phylo_metrics <-
  tibble(serial = 1:length(input_files),
         run_id = run_ids,
         timestep = timesteps)

# find unique species assemblages ----------------------------------------------
# initiate assemblages
assemblage <- 1

spp_assemblages[[1]] <-
  readRDS(paste0(dir_input,input_files[1])) |> 
  pull(species)

# loop through all files
for(target_file in 2:length(input_files)){
  
  agg_metrics <-
    readRDS(paste0(dir_input,input_files[target_file]))
  
  # check for global extinction
  if(dim(agg_metrics)[1] > 0){
    
    spp <-
      readRDS(paste0(dir_input,input_files[target_file])) |> 
      pull(species) |> 
      unique()
    
    # check if this assemblage already exists
    quit_loop <- FALSE
    match     <- FALSE
    i         <- 1
    while(!quit_loop){
      
      # match is TRUE if all species are present
      match <-
        !(FALSE %in% (spp %in% spp_assemblages[[i]]))
      
      # stop loop if there is a match or we run out of comparisons
      quit_loop <-
        match == TRUE | i >= assemblage
      
      # increase i if we keep going
      if(!quit_loop){i <- i + 1}
    } # end of assemblage check while loop
    
    # if a matching assemblage is found, assign the corresponding index
    if(match == TRUE){
      
      assemblage_indices[target_file] <-
        i
      
      # if it is a new assemblage, add to the unique assemblage list
    } else {
      
      assemblage <-
        assemblage + 1
      
      spp_assemblages[[assemblage]] <-
        spp
      
      assemblage_indices[target_file] <-
        assemblage
    } # end of assemblage saving if statement
  } # end of extinction check if statement
} # end of file loop

# trim the assemblages
spp_assemblages <-
  spp_assemblages[1:max(assemblage_indices,na.rm = T)]

# calculate phylogenetic metrics -----------------------------------------------
# container
assemblage_metrics <-
  tibble(assemblage_index = 1:length(spp_assemblages),
         pd = NA,
         mpd = NA,
         vpd = NA)

pb <- 
  progress_bar$new(total = length(assemblage_metrics$assemblage_index),
                   format = ":current of :total :eta [:bar] calc. phylo metrics")

for(i in assemblage_metrics$assemblage_index){
  
  pb$tick()
  
  # community matrix for picante package
  pa_mat <-
    matrix(data = 1,
           ncol = length(spp_assemblages[[i]]),
           nrow = 1,
           dimnames = list(c("global"),
                           c(spp_assemblages[[i]])
           ))
  
  # subset phylogeny
  phylo_sub <-
    keep.tip(phylo_full,spp_assemblages[[i]])
  
  # phylogenetic diversity
  assemblage_metrics$pd[i] <- 
    pd(tree = phylo_sub,
       samp = pa_mat)[1,1]
  
  # skip if there is only one species
  if(length(spp_assemblages[[i]]) > 1){
    
    # distance matrix of phylogeny
    phylo_dist <-
      cophenetic.phylo(phylo_sub)
    
    # mean_pairwise distance
    assemblage_metrics$mpd[i] <-
      mpd(dis = phylo_dist,
          samp = pa_mat)
    
    # variance in pairwise distance
    assemblage_metrics$vpd[i] <-
      var(phylo_dist[upper.tri(phylo_dist)])
    
  }else{
    assemblage_metrics$mpd[i] <- NA
    assemblage_metrics$vpd[i] <- NA
    
  }
} # end of assemblage loop

# collate results
phylo_results <-
  tibble(serial           = phylo_metrics$serial,
         assemblage_index = assemblage_indices) |> 
  left_join(assemblage_metrics, by = "assemblage_index") |> 
  right_join(phylo_metrics, by = "serial") |> 
  select(-c(assemblage_index,serial)) |> 
  relocate(c(run_id,timestep))

# export ----------------------------------------------------------
write_csv(phylo_results,
          file_output)


