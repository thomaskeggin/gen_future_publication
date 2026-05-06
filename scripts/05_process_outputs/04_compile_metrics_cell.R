# This script compiles all species-level metrics into a single table

# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(parallel)
library(progress)

# directories ------------------------------------------------------------------
dir_agg_metrics <-
  "/storage/gen_future/results/02_aggregate_metrics_cell/" #input

dir_betweenness <-
  "/storage/gen_future/results/01_betweenness/" #input

# dir_phylo <-
#   "/storage/gen_future/results/03_phylogenetic_metrics_cell/" #input

dir_cell_metrics_intermediate <-
  "/storage/gen_future/results/04_metrics_cell_intermediate/" # not output - deleted at end.

# if not present, create results sub-folders
if(!file.exists(dir_cell_metrics_intermediate)){dir.create(dir_cell_metrics_intermediate)}

dir_csv <-
  "/storage/gen_future/results/04_metrics_cell/" #output

# if not present, create results sub-folders
if(!file.exists(dir_csv)){dir.create(dir_csv)}

# list files for each input type
files_agg_metrics <-
  list.files(dir_agg_metrics)

files_betweenness <-
  list.files(dir_betweenness)

# files_phylo <-
#   list.files(dir_phylo)

# check to see that there are no missing files
if(length(files_agg_metrics) != length(files_betweenness)){
  stop("Mismatch between number of aggregated metric files and number of 
       network metric files")
}

# join cell metrics ------------------------------------------------------------
mclapply(1:length(files_agg_metrics),
         mc.cores = 80,
         function(iteration){
           
           # extract run and time steps from file name
           indices <-
             files_agg_metrics[iteration] |> 
             gsub(pattern     = "02_aggregate_metrics_cell_|.rds",
                  replacement = "") |> 
             strsplit("_") |> 
             unlist()
           
           run_id <-
             indices[1]
           
           timestep <-
             indices[2]
           
           # load in aggregated metrics ----------------------------------------
           aggregate_metrics_cell <-
             readRDS(paste0(dir_agg_metrics,files_agg_metrics[iteration]))
           
           # check to see if there is global extinction
           if(dim(aggregate_metrics_cell)[1] > 0){
             
             # extract matching betweenness and phylogeny files
             file_betweenness <-
               files_betweenness[
                 grepl(paste0("_",run_id,"_"),files_betweenness) &
                   grepl(paste0("_",timestep,".rds"),files_betweenness)]
             
             # file_phylo <-
             #   files_phylo[
             #     grepl(paste0("_",run_id,"_"),files_phylo) &
             #       grepl(paste0("_",timestep,".rds"),files_phylo)]
             
             # load and wrangle betweenness metrics ----------------------------
             betweenness <-
               readRDS(paste0(dir_betweenness,file_betweenness)) |> 
               
               # group by simulation and time step and species
               group_by(run_id,timestep,cell) |> 
               
               # aggregate
               reframe(betweenness_mean = mean(betweenness,na.rm = T),
                       betweenness_min  = min(betweenness,na.rm = T),
                       betweenness_max  = max(betweenness,na.rm = T))
             
             # join metrics together
             metrics_cell <-
               left_join(aggregate_metrics_cell,
                         betweenness,
                         by = c("run_id","timestep","cell"))
             
             # load and join phylogenetic metrics ------------------------------
             # phylo_metrics <-
             #   readRDS(paste0(dir_phylo,file_phylo))
             # 
             # # join metrics together
             # metrics_cell <-
             #   left_join(metrics_cell,
             #             phylo_metrics,
             #             by = c("run_id","timestep","cell"))
             
             # export file -----------------------------------------------------
             saveRDS(metrics_cell,
                     paste0(dir_cell_metrics_intermediate,"04_metrics_cell_",run_id,"_",timestep,".rds"))
             
             # output an empty data frame if globally extinct
           }else{
             saveRDS(aggregate_metrics_cell,
                     paste0(dir_cell_metrics_intermediate,"04_metrics_cell_",run_id,"_",timestep,".rds"))
             
           }
         })

# compile to csv ---------------------------------------------------------------
# list all cell metric files
metric_files <-
  list.files(dir_cell_metrics_intermediate)

# convert first file to csv to initiate
first_file <-
  paste0(dir_cell_metrics_intermediate,
         metric_files[1])

csv_output_file <-
  paste0(dir_csv,"04_metrics_cell_",timesteps_i,".csv")

write_csv(x    = readRDS(first_file),
          file = csv_output_file,
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
    paste0(dir_cell_metrics_intermediate,
           file)
  
  write_csv(x      = readRDS(file_path),
            file   = csv_output_file,
            append = TRUE,
            progress = FALSE)
  
}

# remove empty directory
unlink(dir_cell_metrics_intermediate, recursive = TRUE)







