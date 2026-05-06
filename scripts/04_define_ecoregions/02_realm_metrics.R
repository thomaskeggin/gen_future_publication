# set session ------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)

# load data --------------------------------------------------------------------
# spalding polygons
spalding <-
  vect("./data/ecoregions/Marine_Ecoregions_Of_the_World_(MEOW)-shp/Marine_Ecoregions_Of_the_World__MEOW_.shp") #input

# distance matrix
dist_mat <-
  readRDS("./data_processed/seascapes/distances_full/distances_full_0.rds") #input

# seascape
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds") #input

seascape <-
  seascape$sst_mean |> 
  as_tibble(rownames = "cell") |> 
  na.omit() |> 
  mutate(cell = as.numeric(cell))

# region information
region_IDs <-
  read_csv("./data_processed/realms/01_realm_ids.csv", #input
           show_col_types = F) |>
  mutate(global = "global") |>
  pivot_longer(cols = c(ecoregion,province,realm,global),
               names_to = "scale",
               values_to = "region")

# ecoregions
ecoregions <-
  region_IDs |> 
  filter(scale == "ecoregion") |> 
  pull(region) |> 
  unique()

# provinces
provinces <-
  region_IDs |> 
  filter(scale == "province") |> 
  pull(region) |> 
  unique()

# realms
realms <-
  region_IDs |> 
  filter(scale == "realm") |> 
  pull(region) |> 
  unique()

# calculate mean distances for each region -------------------------------------
# ecoregions
dist_ecoregion <- c()
size_ecoregion <- c()
for(ecoregion in ecoregions){
  
  # subset by ecoregion to find cell IDs
  id_subset <- 
    region_IDs |> 
    filter(scale == "ecoregion",
           region == ecoregion)
  
  cell_subset <-
    id_subset$cell
  
  # region size
  cell_number <-
    length(cell_subset)
  
  size_ecoregion <-
    c(size_ecoregion,
      cell_number)
  
  # skip if smaller than 5
  if(cell_number > 1){
  
  # subset the distance matrix
  dist_mat_ecoregion <-
    dist_mat[rownames(dist_mat) %in% cell_subset,
             colnames(dist_mat) %in% cell_subset]
  
  # remove self comparisons from symmetrical matrix
  dist_mat_upper <-
    dist_mat_ecoregion[upper.tri(dist_mat_ecoregion)]
  
  # calculate mean
  dist_ecoregion <-
    c(dist_ecoregion,
      mean(dist_mat_upper))

  }else{
    
    # calculate mean
    dist_ecoregion <-
      c(dist_ecoregion,
        NA)
    }
}

# provinces
dist_province <- c()
size_province <- c()
for(province in provinces){
  
  # subset by province to find cell IDs
  id_subset <- 
    region_IDs |> 
    filter(scale == "province",
           region == province)
  
  cell_subset <-
    id_subset$cell
  
  # region size
  cell_number <-
    length(cell_subset)
  
  size_province <-
    c(size_province,
      cell_number)
  
  # skip if smaller than 5
  if(cell_number > 1){
    
    # subset the distance matrix
    dist_mat_province <-
      dist_mat[rownames(dist_mat) %in% cell_subset,
               colnames(dist_mat) %in% cell_subset]
    
    # remove self comparisons from symmetrical matrix
    dist_mat_upper <-
      dist_mat_province[upper.tri(dist_mat_province)]
    
    # calculate mean
    dist_province <-
      c(dist_province,
        mean(dist_mat_upper))
    
  }else{
    
    # calculate mean
    dist_province <-
      c(dist_province,
        NA)
  }
}

# realms
dist_realm <- c()
size_realm <- c()
for(realm in realms){
  
  # subset by realm to find cell IDs
  id_subset <- 
    region_IDs |> 
    filter(region == realm)
  
  cell_subset <-
    id_subset$cell
  
  # subset the distance matrix
  dist_mat_realm <-
    dist_mat[rownames(dist_mat) %in% cell_subset,
             colnames(dist_mat) %in% cell_subset]
  
  # remove self comparisons from symmetrical matrix
  dist_mat_upper <-
    dist_mat_realm[upper.tri(dist_mat_realm)]
  
  # calculate mean
  dist_realm <-
    c(dist_realm,
      mean(dist_mat_upper))
  
  # region size
  size_realm <-
    c(size_realm,
      dim(dist_mat_realm)[1])
}

# global
dist_global <-
  mean(dist_mat[upper.tri(dist_mat)])

size_global <-
  dim(dist_mat)[1]

# centroid positions -----------------------------------------------------------
# ecoregion
ecoregion_positions <-
  spalding |> 
  project(crs(rast())) |> 
  aggregate(by = "ECOREGION") |> 
  tidyterra::select(ECOREGION) |> 
  centroids()

ecoregion_positions <-
  as.data.frame(ecoregion_positions,
                geom = "xy") |> 
  rename(region = ECOREGION)

# province
province_positions <-
  spalding |> 
  project(crs(rast())) |> 
  aggregate(by = "PROVINCE") |> 
  tidyterra::select(PROVINCE) |> 
  centroids()

province_positions <-
  as.data.frame(province_positions,
                geom = "xy") |> 
  rename(region = PROVINCE)

# realm
realm_positions <-
  spalding |> 
  project(crs(rast())) |> 
  aggregate(by = "REALM") |> 
  tidyterra::select(REALM) |> 
  centroids()

realm_positions <-
  as.data.frame(realm_positions,
                geom = "xy") |> 
  rename(region = REALM)

# thermal change 2013 to 2100 --------------------------------------------------
# per cell
delta_seascape <-
  seascape|> 
  mutate(delta_temp = y_2100 - y_2013) |> 
  select(cell, delta_temp)

var_seascape <-
  seascape |> 
  pivot_longer(cols = contains ("y_")) |> 
  group_by(cell) |> 
  reframe(var_temp = var(value))

seascape_metrics <-
  delta_seascape |> 
  left_join(var_seascape, by = "cell") |> 
  left_join(region_IDs, by = "cell")

# aggregate across regions
delta_thermal <-
  list()

# ecoregion
delta_thermal$ecoregion <-
  seascape_metrics |> 
  filter(scale == "ecoregion") |> 
  group_by(region) |> 
  reframe(delta_sst = mean(delta_temp),
          var_sst = mean(var_temp))

# province
delta_thermal$province <-
  seascape_metrics |> 
  filter(scale == "province") |> 
  group_by(region) |> 
  reframe(delta_sst = mean(delta_temp),
          var_sst = mean(var_temp))

# realm
delta_thermal$realm <-
  seascape_metrics |> 
  filter(scale == "realm") |> 
  group_by(region) |> 
  reframe(delta_sst = mean(delta_temp),
          var_sst = mean(var_temp))

# global
delta_thermal$global <-
  seascape_metrics |> 
  filter(scale == "global") |> 
  group_by(region) |> 
  reframe(delta_sst = mean(delta_temp),
          var_sst = mean(var_temp))

# compile results --------------------------------------------------------------
# ecoregion
metrics_ecoregion <-
  data.frame(scale = "ecoregion",
             region = ecoregions,
             distance = dist_ecoregion,
             size = size_ecoregion) |> 
  left_join(ecoregion_positions, by = "region") |> 
  left_join(delta_thermal$ecoregion, by = "region") |> 
  as_tibble()

# province
metrics_province <-
  data.frame(scale = "province",
             region = provinces,
             distance = dist_province,
             size = size_province) |> 
  left_join(province_positions, by = "region") |> 
  left_join(delta_thermal$province, by = "region") |> 
  as_tibble()

# realm
metrics_realm <-
  data.frame(scale = "realm",
             region = realms,
             distance = dist_realm,
             size = size_realm) |> 
  left_join(realm_positions, by = "region") |> 
  left_join(delta_thermal$realm, by = "region") |> 
  as_tibble()

# global
metrics_global<-
  data.frame(scale = "global",
             region = "global",
             distance = dist_global,
             size = size_global,
             x = NA,
             y = NA) |> 
  left_join(delta_thermal$global, by = "region") |> 
  as_tibble()

region_metrics <-
  do.call(rbind.data.frame,
          list(metrics_ecoregion,
               metrics_province,
               metrics_realm,
               metrics_global))

# output results ---------------------------------------------------------------
write_csv(region_metrics,
          "./data_processed/realms/02_region_metrics.csv") #output













