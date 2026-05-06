# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(picante)
library(parallel)

# directories
dir_gen_out <-
  "/storage/gen_future/output/partial_homogenisation/" #input

dir_spdf <-
  "/storage/gen_future/results/01_species_dfs/" #input

dir_output <-
  "/storage/gen_future/results/03_phylogenetic_metrics_species/" #output

# if not present, create a results sub-folder
if(!file.exists(dir_output)){
  dir.create(dir_output)
}

# functions
dir_fun <-
  "./scripts/functions/"

for(f in list.files(dir_fun)){
  
  source(paste0(dir_fun,f))
}

# load -------------------------------------------------------------------------
spdfs <- 
  list.files(dir_spdf)

mclapply(spdfs,
         mc.cores = 40,
         function(spdf){
           
           # read species data frame
           spdf_file <-
             readRDS(paste0(dir_spdf,spdf))
           
           # extract run and time steps from file name
           indices <-
             spdf |> 
             gsub(pattern     = "01_spdf_|.rds",
                  replacement = "") |> 
             strsplit("_") |> 
             unlist()
           
           run_id <-
             indices[1]
           
           timestep <-
             indices[2]
           
           # check to see if there is global extinction
           if(dim(spdf_file)[1] > 0){
             
             # load in species object
             sp_object <-
               readRDS(paste0(dir_gen_out,run_id,"/species/species_t_",timestep,".rds"))
             
             # remove extinct species
             sp_object <- 
               sp_object[speciesExtant(sp_object)]
             
             phylo_metrics <-
               tibble(run_id = run_id,
                      timestep = timestep,
                      species = speciesIDs(sp_object,id = "species_name"),
                      pd = NA,
                      mpd = NA,
                      vpd = NA)
             
             for(sp in 1:length(sp_object)){
               
               target_species <-
                 sp_object[[sp]]
               
               phylo_metrics$pd[sp] <-
                 clusterPD(target_species)
               
               phylo_metrics$mpd[sp] <-
                 mean(clusterDistances(target_species))
               
               phylo_metrics$vpd[sp] <-
                 var(clusterDistances(target_species))
               
             } # end of species for loop
             
             # export ----------------------------------------------------------
             saveRDS(phylo_metrics,
                     paste0(dir_output,
                            "03_phylogenetic_metrics_species_",
                            run_id,"_",
                            timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(spdf_file,
                     paste0(dir_output,
                            "03_phylogenetic_metrics_cell_",
                            run_id,"_",
                            timestep,".rds"))
           }
           
         }) # end of mclapply











