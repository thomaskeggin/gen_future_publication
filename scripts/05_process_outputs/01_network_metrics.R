# This script calculates network-based metrics for sims, species, clusters, and
# cells.

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(igraph)
library(parallel)

# functions
dir_fun <-
  "./scripts/functions/" #input

for(f in list.files(dir_fun)){
  
  source(paste0(dir_fun,f))
}

# directories
dir_batch <-
  "/storage/gen_future/output/partial_homogenisation/" #input

dir_betweenness <-
  "/storage/gen_future/results/01_betweenness/" #output

dir_cohesion <-
  "/storage/gen_future/results/01_cohesion/" #output

dir_icd <-
  "/storage/gen_future/results/01_inter_cluster_distances/" #output

# if not present, create results sub-folders
if(!file.exists(dir_betweenness)){dir.create(dir_betweenness)}
if(!file.exists(dir_cohesion)){dir.create(dir_cohesion)}
if(!file.exists(dir_icd)){dir.create(dir_icd)}

# load -------------------------------------------------------------------------
# configuration parameters
parameters <-
  read_csv("./data_processed/configs/partial_homogenisation/config_parameters.csv") #input

# distance matrix
distance_matrix <-
  readRDS("./data_processed/seascapes/distances_full/distances_full_0.rds")/1000 #input

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

# loop over simulations --------------------------------------------------------
mclapply(species_paths,
         mc.cores = 30,
         function(species_path){
           
           # extract run and time step from species path
           run      <- parse_number(species_path)
           timestep <- gsub(".*species","",species_path) |> parse_number()
           
           # pull out dispersal capacity
           dispersal_capacity <-
             parameters |> 
             filter(run_id == run) |> 
             pull(dispersal_range)
           
           
           # load ---------------------------------------------------------------------
           # load species object
           species_object <-
             readRDS(species_path)
           
           # remove extinct species
           species_object <-
             species_object[speciesExtant(species_object)]
           
           # check to see if any species are extant
           if(length(species_object) > 0){
           
           # betweenness container
           betweenness <-
             vector("list", length = length(species_object))
           
           # cohesion container
           cohesion <-
             betweenness
           
           # inter-cluster container
           inter_cluster_distances_km <-
             rep(NA,length(species_object))
           
           names(inter_cluster_distances_km) <- 
             speciesIDs(species_object,
                        "species_name")
           
           # loop over species -------------------------------------------------
           for(sp in 1:length(species_object)){
             
             # wrangle species network -----------------------------------------
             # extract occupied cells
             occupied_cells <-
               names(species_object[[sp]]$abundance)
             
             # skip species with only one cell and give NA network metric values
             if(length(occupied_cells) == 1){
               
               betweenness[[sp]] <-
                 tibble(run_id      = run,
                        timestep    = timestep,
                        species     = species_object[[sp]]$species_name,
                        cell        = occupied_cells,
                        betweenness = NA)
               
               cohesion[[sp]] <-
                 tibble(run_id   = run,
                        timestep = timestep,
                        species  = species_object[[sp]]$species_name,
                        cluster  = NA,
                        cohesion = NA)
               
               inter_cluster_distances_km[sp] <-
                 NA
               
             }else{
               
               # subset the distance matrix into 2 matrices, for intra- and inter-cluster distances
               dist_intra <- dist_inter <-
                 distance_matrix[occupied_cells,occupied_cells]
               
               # remove inter-cluster edges 
               cluster_ids <-
                 species_object[[sp]]$traits[,"cluster_id"]
               
               dist_intra[outer(cluster_ids, cluster_ids, "!=")] <- 0
               
               # remove edges larger than the dispersal capacity
               dist_intra[dist_intra > dispersal_capacity] <- 0
               
               # convert to graph
               sp_graph <-
                 graph_from_adjacency_matrix(dist_intra,
                                             weighted = TRUE,
                                             mode = "upper")
               
               # add cluster ids
               sp_graph <-
                 set_vertex_attr(sp_graph,
                                 "cluster",
                                 value = cluster_ids)
               
               # betweenness (per cell) -------------------------------------------------
               betweenness[[sp]] <-
                 tibble(run_id      = run,
                        timestep    = timestep,
                        species     = species_object[[sp]]$species_name,
                        cell        = occupied_cells,
                        betweenness = betweenness(sp_graph))
               
               
               # node cohesion (per cluster) --------------------------------------------
               # the minimum number of cells you can remove before you disconnect the graph.
               # you get 0 if the graph is already disconnected.
               unique_clusters <- unique(cluster_ids)
               
               cohesions <-
                 rep(NA,length(unique_clusters))
               
               for(target_cluster in 1:length(unique_clusters)){
                 
                 cohesion_graph <-
                   delete_vertices(sp_graph,
                                   which(cluster_ids != unique(cluster_ids)[target_cluster]))
                 
                 cohesions[target_cluster] <-
                   cohesion(cohesion_graph)
                 
               }
               
               cohesion[[sp]] <-
                 tibble(run_id   = run,
                        timestep = timestep,
                        species  = species_object[[sp]]$species_name,
                        cluster  = unique(cluster_ids),
                        cohesion = cohesions)
               
               
               # cluster to cluster distances (per species) -----------------------------
               # skip species with only one cluster
               if(length(unique(cluster_ids)) > 1){
                 
                 # the shortest distance between clusters in a species
                 dist_inter[outer(cluster_ids, cluster_ids, "==")] <- NA
                 
                 # combinations
                 cluster_combos <-
                   combn(unique(cluster_ids),
                         2) |> 
                   t()
                 
                 # run loop
                 cluster_distances <-
                   rep(NA,dim(cluster_combos)[1])
                 
                 for(combo in 1:dim(cluster_combos)[1]){
                   
                   # cluster IDs
                   cluster_i <-
                     cluster_combos[combo,1]
                   cluster_j <-
                     cluster_combos[combo,2]
                   
                   # cell IDs
                   cells_i <-
                     names(cluster_ids[cluster_ids == cluster_i])
                   cells_j <-
                     names(cluster_ids[cluster_ids == cluster_j])
                   
                   dist_mat_sub <-
                     dist_inter[cells_i,
                                cells_j]
                   
                   
                   cluster_distances[combo] <-
                     min(dist_mat_sub)
                   
                 } # end of cluster combo loop
                 
                 inter_cluster_distances_km[sp] <-
                   mean(cluster_distances)
                 
               } # end of one cluster if statement
               
               gc(verbose = FALSE)
               
             } # end of single cell occupancy if-else statement
             
           } # end of species loop
           
           # compile -----------------------------------------------------------
           # betweenness (per cell)
           betweenness <-
             do.call(rbind.data.frame,betweenness)
           
           # cohesion (per cluster)
           cohesion <-
             do.call(rbind.data.frame,cohesion)
           
           # inter cluster distances (per species)
           inter_cluster_distances_km <-
             tibble(run_id                     = run,
                    timestep                   = timestep,
                    species                    = names(inter_cluster_distances_km),
                    inter_cluster_distances_km = inter_cluster_distances_km)
           
           # export ------------------------------------------------------------
           # betweenness
           saveRDS(betweenness,
                   paste0(dir_betweenness,"01_betweenness_",run,"_",timestep,".rds"))
           
           # cohesion
           saveRDS(cohesion,
                   paste0(dir_cohesion,"01_cohesion_",run,"_",timestep,".rds"))
           
           # inter cluster distances
           saveRDS(inter_cluster_distances_km,
                   paste0(dir_icd,"01_inter_cluster_distances_",run,"_",timestep,".rds"))
           
           # end of extant species if statement
           # else return empty data frames
           }else{
             # betweenness
             saveRDS(data.frame(),
                     paste0(dir_betweenness,"01_betweenness_",run,"_",timestep,".rds"))
             
             # cohesion
             saveRDS(data.frame(),
                     paste0(dir_cohesion,"01_cohesion_",run,"_",timestep,".rds"))
             
             # inter cluster distances
             saveRDS(data.frame(),
                     paste0(dir_icd,"01_inter_cluster_distances_",run,"_",timestep,".rds"))
             
           } 
           
         }) # end of mclapply



