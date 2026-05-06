# This script takes the ecoregions as defined by Spalding et al. 2007 and
# uses them to assign ecoregions to each cell in the genesis simulation
# outputs.

# set session ------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)

# load and wrangle data --------------------------------------------------------
# ecoregions from Spalding et al (2007)
data_spalding <-
  vect("./data/ecoregions/Marine_Ecoregions_Of_the_World_(MEOW)-shp/Marine_Ecoregions_Of_the_World__MEOW_.shp") #input

# project to wgs84
data_spalding <-
  project(data_spalding,
          rast(crs = "+proj=longlat +datum=WGS84"))

# habitable cells from simulation input
data_coord <-
  readRDS("./data_processed/seascapes/landscapes.rds") #input

data_coord <-
  data_coord[["sst_mean"]] |> 
  select(x,y,y_2100)

data_coord <-
  data_coord |>
  
  # add cell IDs
  mutate(cell = rownames(data_coord)) |>
  
  # filter out unused cells
  filter(!is.na(y_2100)) |>
  select(-y_2100)

# assign realm IDs to cells -------------------------------------------------
data_rast <-
  rast(data_coord,
       crs = crs(data_spalding))

# realms ----
realm_cells <-
  list()

for(realm in unique(data_spalding$REALM)){
  
  # extract realm only cells
  poly_realm <-
    data_spalding |> 
    filter(REALM == realm)
  
  # mask habitable cells by the target realm
  rast_realm <-
    mask(data_rast,
         poly_realm,
         touches = FALSE)
  
  # assign those values to 
  realm_cells[[realm]] <-
    as.data.frame(rast_realm,
                  xy = T) |>
    mutate(realm = realm)
}

# provinces ----
province_cells <-
  list()

for(province in unique(data_spalding$PROVINCE)){
  
  # extract realm only cells
  poly_province <-
    data_spalding |> 
    filter(PROVINCE == province)
  
  # mask habitable cells by the target realm
  rast_province <-
    mask(data_rast,
         poly_province,
         touches = FALSE)
  
  # assign those values to 
  province_cells[[province]] <-
    as.data.frame(rast_province,
                  xy = T) |>
    mutate(province = province)
}

# ecoregions
ecoregion_cells <-
  list()

for(ecoregion in unique(data_spalding$ECOREGION)){
  
  # extract realm only cells
  poly_ecoregion <-
    data_spalding |> 
    filter(ECOREGION == ecoregion)
  
  # mask habitable cells by the target realm
  rast_ecoregion <-
    mask(data_rast,
         poly_ecoregion,
         touches = FALSE)
  
  # assign those values to 
  ecoregion_cells[[ecoregion]] <-
    as.data.frame(rast_ecoregion,
                  xy = T) |>
    mutate(ecoregion = ecoregion)
}

# compile into single data frame
realm <-
  do.call(rbind.data.frame,realm_cells) |>
  tibble()

province <-
  do.call(rbind.data.frame,province_cells) |> 
  select(-c(x,y))

eco <-
  do.call(rbind.data.frame,ecoregion_cells)|> 
  select(-c(x,y))

region_ids <-
  realm_df |> 
  left_join(province) |> 
  left_join(eco)

# output -----------------------------------------------------------------------
write_csv(region_ids,
          "./data_processed/realms/01_realm_ids.csv") #output
