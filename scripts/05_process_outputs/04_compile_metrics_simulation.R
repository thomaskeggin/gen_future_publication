# This script compiles all species-level metrics into a single table

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(parallel)
library(progress)

# directories
dir <- list()

dir$agg_metrics <-
  "/storage/gen_future/results/02_aggregate_metrics_simulation/" #input

dir$sp_metrics <-
  "/storage/gen_future/results/04_metrics_species_intermediate/" #input

dir_simulation_metrics_intermediate <-
  "/storage/gen_future/results/04_metrics_simulation_intermediate/" # input

# file_phylo <-
#   "/storage/gen_future/results/03_phylogenetic_metrics_simulation.csv" #input

# if not present, create results sub-folders
if(!file.exists(dir_simulation_metrics_intermediate)){dir.create(dir_simulation_metrics_intermediate)}

dir_csv <-
  "/storage/gen_future/results/04_metrics_simulation/" #output

# if not present, create results sub-folders
if(!file.exists(dir_csv)){dir.create(dir_csv)}

# list files for each input type
files <-
  lapply(dir,list.files)

# check to see that there are no missing files
if(length(files$agg_metrics) != mean(lengths(files))){
  stop("Mismatch between numbers of input files from each source.")
}

# join simulation metrics ---------------------------------------------------------
mclapply(1:length(files$agg_metrics),
         mc.cores = 10,
         function(iteration){
           
           # extract run and time steps from file name
           indices <-
             files$agg_metrics[iteration] |> 
             gsub(pattern     = "02_aggregate_metrics_simulation_|.rds",
                  replacement = "") |> 
             strsplit("_") |> 
             unlist()
           
           run_id <-
             indices[1]
           
           timestep <-
             indices[2]
           
           # load in aggregated metrics
           aggregate_metrics_simulation <-
             readRDS(paste0(dir$agg_metrics,files$agg_metrics[iteration]))
           
           # check to see if there is global extinction
           if(dim(aggregate_metrics_simulation)[1] > 0){
             
             # extract matching metric files
             file_sp_metric <-
               files$sp_metrics[
                 grepl(paste0("_",run_id,"_"),files$sp_metrics) &
                   grepl(paste0("_",timestep,".rds"),files$sp_metrics)]
             
             # load in compiled species metrics
             metrics_species <-
               readRDS(paste0(dir$sp_metrics,files$sp_metrics[iteration])) |> 
               
               group_by(run_id,timestep) |> 
               
               # aggregate
               reframe(inter_cluster_distance_mean = mean(inter_cluster_distances_km,na.rm = T),
                       inter_cluster_distance_min  = min(inter_cluster_distances_km,na.rm = T),
                       inter_cluster_distance_max  = max(inter_cluster_distances_km,na.rm = T),
                       
                       cohesion_mean = mean(cohesion_mean,na.rm = T),
                       cohesion_min  = min(cohesion_min,na.rm = T),
                       cohesion_max  = max(cohesion_max,na.rm = T),
                       
                       betweenness_mean = mean(betweenness_mean,na.rm = T),
                       betweenness_min  = min(betweenness_min,na.rm = T),
                       betweenness_max  = max(betweenness_max,na.rm = T))
             
             # join metrics
             metrics_simulation <-
               left_join(aggregate_metrics_simulation,
                         metrics_species,
                         by = c("run_id","timestep"))
             
             # export file
             saveRDS(metrics_simulation,
                     paste0(dir_simulation_metrics_intermediate,
                            "04_metrics_simulation_",run_id,"_",timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(aggregate_metrics_simulation,
                     paste0(dir_simulation_metrics_intermediate,
                            "04_metrics_simulation_",run_id,"_",timestep,".rds"))
           }
           
         })

# compile to csv ---------------------------------------------------------------
file_csv <-
  paste0(dir_csv,"04_metrics_simulation_",timesteps_i,".csv")

# list all simulation metric files
metric_files <-
  list.files(dir_simulation_metrics_intermediate)

# convert first file to csv to initiate
first_file <-
  paste0(dir_simulation_metrics_intermediate,
         metric_files[1])

write_csv(x    = readRDS(first_file),
          file = file_csv)

# set up progress bar
pb <-
  progress_bar$new(total = length(metric_files[-1]),
                   format = ":current of :total :percent [:bar] :eta")

# loop through the rest of the files
for(file in metric_files[-1]){
  
  pb$tick()
  
  # append file contents to csv compilation file
  file_path <-
    paste0(dir_simulation_metrics_intermediate,
           file)
  
  write_csv(x      = readRDS(file_path),
            file   = file_csv,
            append = TRUE,
            progress = FALSE)
  
}

# add phylogenetic metrics -----------------------------------------------------
# # join
# all_metrics <-
#   left_join(read_csv(file_csv, show_col_types = FALSE),
#             read_csv(file_phylo, show_col_types = FALSE))
# 
# # re-export
# write_csv(all_metrics,
#           file_csv)

# clean up ---------------------------------------------------------------------
# remove single files compilation directories for species and simulations
unlink(dir_simulation_metrics_intermediate, recursive = TRUE)
unlink(dir$sp_metrics, recursive = TRUE)