# set --------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)

# load -------------------------------------------------------------------------
# sdm outputs
sdm_predictions <-
  list(
    y_2100 = readRDS("./data/Thomas Keggin Mechanistic Climate Change Project/sdm-run-june2021/final-outputs/wide-matrix/SSP585_dispersal-limitation_novel-sst/2071_2100.RDS") #input
  )

# environmental grid
env_grid <-
  terra::rast("./data/Thomas Keggin Mechanistic Climate Change Project/environmental-grid/global_mask_v2.nc") #input

# species info
species_info <-
  readRDS("./data_processed/species/species_initialisation_information.rds") #input

# seascape
sea <-
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] |> #input
  select(x,y,y_2100) |> 
  na.omit() |> 
  as_tibble(rownames = "cell") |> 
  mutate(cell = as.numeric(cell),
         xy = paste0(x,"_",y))

# wrangle ----------------------------------------------------------------------
# environmental grid to data frame
env_grid_df <-
  as.data.frame(env_grid,
                xy=T) |> 
  as_tibble(rownames = "cell") |> 
  mutate(cell = as.numeric(cell)) |> 
  select(-layer)

# convert SDM 
sdm_output_list <-
  list()

for(i in names(sdm_predictions)){
  
  sdm_output_list[[i]] <-
    
    # sdm predictions for simulation species
    sdm_predictions[[i]][,c("cell",names(species_info))] |> 
    
    # subset for development
    #select(cell,contains("Zebrasoma")) |> 
    
    # pivot longer
    pivot_longer(cols = -cell,
                 names_to = "species",
                 values_to = "suitability") |> 
    na.omit() |>
    group_by(species) |> 
    
    # add coordinates
    left_join(env_grid_df, by = "cell") |> 
    
    # aggregate to 1 degree
    # round to nearest 0.5 (there are no integer coordinates)
    mutate(x = as.numeric(gsub("\\..*",".5",as.character(x))),
           y = as.numeric(gsub("\\..*",".5",as.character(y)))) |>
    
    # create composite coordinates for joining
    mutate(xy = paste0(x,"_",y)) |>
    
    # remove redundant x, y, and cell columns (the cell is for .25 res and no longer valid)
    select(-c(cell,x,y)) |>
    
    # get gen cells
    right_join(sea) |> 
    select(-xy) |> 
    # find mean suitability for new cells
    group_by(species,cell) |>
    reframe(suitability = mean(suitability)) |> 
    mutate(year = gsub("y_","",i) |> as.numeric())
  
}

# convert to a pa matrix
sdm_output_df <-
  sdm_output_list$y_2100 |> 
  select(cell,species) |> 
  mutate(value = 1) |>
  pivot_wider(names_from = species,
              values_from = value,
              values_fill = 0) |> 
  select(-"NA")

sdm_output_mat <-
  sdm_output_df |> 
  select(-cell) |> 
  as.matrix()

row.names(sdm_output_mat) <- sdm_output_df$cell

# export -----------------------------------------------------------------------
saveRDS(sdm_output_mat,
        "./results/sdm_gen_pa_fit/01_sdm_pa_mat.rds") #output


