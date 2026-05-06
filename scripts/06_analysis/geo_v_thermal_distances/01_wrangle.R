# set --------------------------------------------------------------------------
library(tidyverse)
library(progress)

# load -------------------------------------------------------------------------
# geographic distances
dist_geo <-
  readRDS("./data_processed/seascapes/distances_full/distances_full_0.rds") #input

# seascape
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] |>  #input
  as_tibble(rownames = "cell") |> 
  filter(!is.na(y_2100)) |> 
  select(cell,y) |> 
  arrange(y)

# generate latitudinal combinations --------------------------------------------
yes <- 
  unique(seascape$y)

lat_combinations <-
  combn(yes,2) |> 
  t() |> 
  as_tibble(.name_repair = "universal")

colnames(lat_combinations) <- c("y1","y2")

# calculate mean geographical distances between latitudes ----------------------
# limit to 550 km as that's the limit of the model
# each combination of latitude vs latitude
pb <-
  progress_bar$new(total = dim(lat_combinations)[1],
                   format = ":current of :total [:bar] :eta")

geo_lat_distances <-
  c()

for(lat_comb in 1:dim(lat_combinations)[1]){
  
  pb$tick()
  
  # target latitudes
  lat_1 <-
    lat_combinations[[lat_comb,1]]
  
  lat_2 <-
    lat_combinations[[lat_comb,2]]
  
  # target cells
  lat_1_cells <-
    seascape |> 
    filter(y == lat_1) |> 
    pull(cell)
  
  lat_2_cells <-
    seascape |> 
    filter(y == lat_2) |> 
    pull(cell)
  
  # subset distance matrix
  dist_geo_sub <-
    dist_geo[lat_1_cells,
             lat_2_cells]
  
  # remove distances above 550 km as this is the limit of the simulation
  dist_geo_sub[dist_geo_sub > (550 * 1000)] <- NA
  
  # calculate mean inter-latitudinal distances
  lat_distance <-
    mean(dist_geo_sub,na.rm = T)
  
  # set to NA if all distances are filtered out
  if(is.nan(lat_distance)){
    lat_distance <- NA
  }
  
  # add to vector
  geo_lat_distances <-
    c(geo_lat_distances,
      lat_distance)
  
}

geo_lat_distances_df <-
  tibble(lat_combinations,
         geo_lat_distances) |> 
  filter(!is.na(geo_lat_distances))

# calculate thermal distances --------------------------------------------------
dir_thermal <-
  "./data_processed/seascapes/distances_full_thermal/" #input

thermal_files <-
  list.files(dir_thermal)

timesteps <-
  parse_number(thermal_files) |> sort()

pb <-
  progress_bar$new(total = length(thermal_files),
                   format = ":current of :total [:bar] :eta")

thermal_lat_distances <-
  list()

for(timestep in timesteps){
  
  pb$tick()
  
  # load in thermal distances
  dist_therm <-
    readRDS(paste0(dir_thermal,
                   "distances_full_thermal_",timestep,".rds"))
  
  # loop through latitudes
  thermal_lat_distances[[paste0("t_",timestep)]] <-
    c()
  
  for(lat_comb in 1:dim(geo_lat_distances_df)[1]){
    
    # target latitudes
    lat_1 <-
      geo_lat_distances_df[[lat_comb,1]]
    
    lat_2 <-
      geo_lat_distances_df[[lat_comb,2]]
    
    # target cells
    lat_1_cells <-
      seascape |> 
      filter(y == lat_1) |> 
      pull(cell)
    
    lat_2_cells <-
      seascape |> 
      filter(y == lat_2) |> 
      pull(cell)
    
    # subset distance matrix
    dist_therm_sub <-
      dist_therm[lat_1_cells,
                 lat_2_cells]
    
    # calculate mean inter-latitudinal distances
    lat_distance <-
      mean(dist_therm_sub,na.rm = T)
    
    thermal_lat_distances[[paste0("t_",timestep)]] <-
      c(thermal_lat_distances[[paste0("t_",timestep)]],
        lat_distance)
    
  } # end latitudinal combinations loop
  
  thermal_lat_distances[[paste0("t_",timestep)]] <-
    tibble(geo_lat_distances_df,
           thermal_distance = thermal_lat_distances[[paste0("t_",timestep)]]) |> 
    select(-geo_lat_distances) |> 
    mutate(timestep = timestep)
  
}# end of time step loop

thermal_lat_distances_df <-
  do.call(rbind.data.frame,
          thermal_lat_distances)

# combine geographic and thermal distances -------------------------------------
lat_distances <-
  left_join(geo_lat_distances_df,
            thermal_lat_distances_df)

# export -----------------------------------------------------------------------
saveRDS(lat_distances,
        "./results/geo_v_thermal_distances/01_pairwise_distance_df.rds") #output

