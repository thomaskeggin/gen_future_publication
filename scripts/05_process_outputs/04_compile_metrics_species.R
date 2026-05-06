# This script compiles all species-level metrics into a single table

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(parallel)
library(progress)

# directories
dir <- list()

dir$agg_metrics <-
  "/storage/gen_future/results/02_aggregate_metrics_species/" #input

dir$betweenness <-
  "/storage/gen_future/results/01_betweenness/" #input

dir$cohesion <-
  "/storage/gen_future/results/01_cohesion/" #input

dir$icd <-
  "/storage/gen_future/results/01_inter_cluster_distances/" #input

dir_species_metrics_intermediate <-
  "/storage/gen_future/results/04_metrics_species_intermediate/" #output

# if not present, create results sub-folders
if(!file.exists(dir_species_metrics_intermediate)){dir.create(dir_species_metrics_intermediate)}

dir_csv <-
  "/storage/gen_future/results/04_metrics_species/" #output

# if not present, create results sub-folders
if(!file.exists(dir_csv)){dir.create(dir_csv)}

# list files for each input type
files <-
  lapply(dir,list.files)

# check to see that there are no missing files
if(length(files$agg_metrics) != mean(lengths(files))){
  stop("Mismatch between numbers of input files from each source.")
}

# join species metrics ---------------------------------------------------------
mclapply(1:length(files$agg_metrics),
         mc.cores = 80,
         function(iteration){
           
           # extract run and time steps from file name
           indices <-
             files$agg_metrics[iteration] |> 
             gsub(pattern     = "02_aggregate_metrics_species_|.rds",
                  replacement = "") |> 
             strsplit("_") |> 
             unlist()
           
           run_id <-
             indices[1]
           
           timestep <-
             indices[2]
           
           # load in aggregated metrics
           aggregate_metrics_species <-
             readRDS(paste0(dir$agg_metrics,files$agg_metrics[iteration]))
           
           # check to see if there is global extinction
           if(dim(aggregate_metrics_species)[1] > 0){
             
             # extract matching metric files
             file_betweenness <-
               files$betweenness[
                 grepl(paste0("_",run_id,"_"),files$betweenness) &
                   grepl(paste0("_",timestep,".rds"),files$betweenness)]
             
             # extract matching cohesion file
             file_cohesion <-
               files$cohesion[
                 grepl(paste0("_",run_id,"_"),files$cohesion) &
                   grepl(paste0("_",timestep,".rds"),files$cohesion)]
             
             # extract matching icd file
             file_icd <-
               files$icd[
                 grepl(paste0("_",run_id,"_"),files$icd) &
                   grepl(paste0("_",timestep,".rds"),files$icd)]
             
             # load and wrangle betweenness metrics
             betweenness <-
               readRDS(paste0(dir$betweenness,file_betweenness)) |> 
               
               # group by simulation and time step and species
               group_by(run_id,timestep,species) |> 
               
               # aggregate
               reframe(betweenness_mean = mean(betweenness,na.rm = T),
                       betweenness_min  = min(betweenness,na.rm = T),
                       betweenness_max  = max(betweenness,na.rm = T))
             
             # load and wrangle betweenness metrics
             cohesion <-
               readRDS(paste0(dir$cohesion,file_cohesion)) |> 
               
               # group by run_id, time step, and species
               group_by(run_id,timestep,species) |> 
               
               # aggregate
               reframe(cohesion_mean = mean(cohesion,na.rm = T),
                       cohesion_min  = min(cohesion,na.rm = T),
                       cohesion_max  = max(cohesion,na.rm = T))
             
             # load in inter cluster distances
             inter_cluster_distances <-
               readRDS(paste0(dir$icd,file_icd))
             
             # join metrics
             metrics_species <-
               left_join(aggregate_metrics_species,
                         inter_cluster_distances,
                         by = c("run_id","timestep","species")) |> 
               left_join(cohesion,
                         by = c("run_id","timestep","species"))  |> 
               left_join(betweenness,
                         by = c("run_id","timestep","species")) 
             
             # export file
             saveRDS(metrics_species,
                     paste0(dir_species_metrics_intermediate,
                            "04_metrics_species_",run_id,"_",timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(aggregate_metrics_species,
                     paste0(dir_species_metrics_intermediate,"04_metrics_species_",run_id,"_",timestep,".rds"))
             
           }
           
         })

# compile to csv ---------------------------------------------------------------
# list all species metric files
metric_files <-
  list.files(dir_species_metrics_intermediate)

# convert first file to csv to initiate
first_file <-
  paste0(dir_species_metrics_intermediate,
         metric_files[1])

species_csv_output <-
  paste0(dir_csv,"04_metrics_species_",timesteps_i,".csv")
  
write_csv(x    = readRDS(first_file),
          file = species_csv_output,
          progress = FALSE)

# set up progress bar
pb <-
  progress_bar$new(total = length(metric_files[-1]),
                   format = ":current of :total :percent [:bar] :eta")

# loop through the rest of the files
for(file in metric_files[-1]){
  
  pb$tick()
  
  # append file contents to csv compilation file
  file_path <-
    paste0(dir_species_metrics_intermediate,
           file)
  
  write_csv(x      = readRDS(file_path),
            file   = species_csv_output,
            append = TRUE,
            progress = FALSE)
  
}










