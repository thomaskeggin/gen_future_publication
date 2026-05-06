# set --------------------------------------------------------------------------
# packages
library(tidyverse)
library(terra)
library(tidyterra)
library(data.table)
library(progress)

# load regions -----------------------------------------------------------------
regions <-
  read_csv("./data_processed/realms/01_realm_ids.csv") #input

realm_zones <-
  read_csv("./data_processed/seascapes/realm_zones.csv") #input

# load and aggregate cell metrics ----------------------------------------------
cell_metrics <-
  list()

cell_dir <-
  "G:/gen_future/results/04_metrics_cell/" #input

cell_files <-
  list.files(cell_dir)

# progress bar
lw_pb <-
  progress_bar$new(total = length(cell_files),
                   format = ":current of :total :percent [:bar] :eta")

# loop across time steps / years
for(file in cell_files){
  
  lw_pb$tick()
  
  cell_metrics[[file]] <-
    
    # load in cell metrics
    fread(paste0(cell_dir,"/",file)) |> 
    
    # remove x and y
    select(-c(x,y)) |> 
    
    # join the regions
    left_join(regions,
              by = "cell") |> 
    
    # group by realm
    group_by(year,
             run_id,
             realm) |> 
    
    # aggregate
    reframe(across(.cols = c(richness:thermal_opt_mean,
                             sst_mean:vpd),
                   ~ mean(.x)),
            occupied_cells = n())
}

# compile metrics
realm_metrics <-
  do.call(rbind.data.frame,cell_metrics)

# add latitudinal "zone" -------------------------------------------------------
realm_metrics_zones <-
  realm_metrics |> 
  left_join(realm_zones)

# export -----------------------------------------------------------------------
write_csv(realm_metrics_zones,
          "G:/gen_future/results/04_metrics_realm.csv") #output
