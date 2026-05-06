# This script calculates the phylogenetic metrics per cell at the species
# level.

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(fishtree)
library(picante)
library(parallel)

# directories
dir_input <-
  "/storage/gen_future/results/01_species_dfs/" #input

dir_output <-
  "/storage/gen_future/results/03_phylogenetic_metrics_cell/" #output

# if not present, create a results sub-folder
if(!file.exists(dir_output)){
  dir.create(dir_output)
}

# per cell ---------------------------------------------------------------------
spdfs <-
  list.files(dir_input)

mclapply(spdfs,
         mc.cores = 80,
         function(spdf_file){
           
           # set up object structure -------------------------------------------
           
           # extract run and time steps from file name
           indices <-
             spdf_file |> 
             gsub(pattern     = "01_spdf_|.rds",
                  replacement = "") |> 
             strsplit("_") |> 
             unlist()
           
           run_id <-
             indices[1]
           
           timestep <-
             indices[2]
           
           # load in species data frame
           spp_occ <-
             readRDS(paste0(dir_input,spdf_file))
           
           # check for global extinction
           if(dim(spp_occ)[1] > 0){
             
             # extract species occurrences
             spp_occ <-
               spp_occ |> 
               select(run_id,timestep,cell,species)
             
             # create a results matrix to store the metric values
             phylo_metrics <-
               spp_occ |> 
               group_by(run_id,timestep,cell) |> 
               
               # calculate metrics
               reframe(
                 
                 placeholder = NA
                 
               ) |> 
               rowid_to_column("serial")
             
             cells <-
               phylo_metrics$cell
             
             # skip if only one species remains
             if(length(unique(spp_occ$species)) > 2){
               
               # phylogeny
               phylo_full <-
                 fishtree::fishtree_phylogeny(species = unique(spp_occ$species))
               
               # extract species assemblages per cell ------------------------------

               # list for each unique species assemblage
               spp_assemblages <-
                 vector("list",dim(phylo_metrics)[1])
               
               # index to match to the assemblage list
               assemblage_indices <-
                 c(1,rep(NA,dim(phylo_metrics)[1]-1))
               
               # initiate assemblages
               assemblage <- 1
               
               spp_assemblages[[1]] <-
                 spp_occ |> 
                 filter(cell == cells[1]) |> 
                 pull(species) |> 
                 unique()
               
               # loop through all cells
               for(target_cell in phylo_metrics$serial[-1]){
                 
                 spp <-
                   spp_occ |> 
                   filter(cell == cells[target_cell]) |> 
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
                   
                   assemblage_indices[target_cell] <-
                     i
                   
                   # if it is a new assemblage, add to the unique assemblage list
                 } else {
                   
                   assemblage <-
                     assemblage + 1
                   
                   spp_assemblages[[assemblage]] <-
                     spp
                   
                   assemblage_indices[target_cell] <-
                     assemblage
                 } # end of assemblage saving if statement
                 
               } # end of cell loop
               
               # trim off empty assemblage slots
               spp_assemblages <-
                 spp_assemblages[1:max(assemblage_indices,na.rm = T)]
               
               # calculate phylogenetic metrics ------------------------------------
               # container
               assemblage_metrics <-
                 tibble(assemblage_index = 1:length(spp_assemblages),
                        pd = NA,
                        mpd = NA,
                        vpd = NA)
               
               for(i in assemblage_metrics$assemblage_index){
                 
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
                 tibble(serial = phylo_metrics$serial,
                        assemblage_index = assemblage_indices) |> 
                 left_join(assemblage_metrics, by = "assemblage_index") |> 
                 right_join(phylo_metrics, by = "serial") |> 
                 select(-c(placeholder,assemblage_index,serial)) |> 
                 relocate(c(run_id,timestep,cell))
               
               # if there is only 1 species remaining
             }else{
               phylo_results <-
                 phylo_metrics |> 
                 select(-c(serial,placeholder)) |> 
                 mutate(pd  = NA,
                        mpd = NA,
                        vpd = NA)
               
             }
             
             # export ----------------------------------------------------------
             saveRDS(phylo_results,
                     paste0(dir_output,
                            "03_phylogenetic_metrics_cell_",
                            run_id,"_",
                            timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(spp_occ,
                     paste0(dir_output,
                            "03_phylogenetic_metrics_cell_",
                            run_id,"_",
                            timestep,".rds"))
           }
           
         }) # end of mclapply loop
