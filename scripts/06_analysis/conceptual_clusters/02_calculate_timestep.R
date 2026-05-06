# set --------------------------------------------------------------------------
library(tidyverse)
library(terra)
library(tidyterra)

# load -------------------------------------------------------------------------
species_information <-
  read_csv("./results/conceptual_clusters/01_spp_info.csv", #input
           show_col_types = F)

parameters <-
  species_information |> 
  select(category,
         dispersal_cat,
         adaptive_cat) |> 
  distinct()

# evolution function -----------------------------------------------------------
species_information_evo <-
  species_information |> 
  
  rowwise() |> 
  
  # thermal adaptation
  dplyr::mutate(
    
    # record old thermal optima
    thermal_optimum_start = thermal_optimum,
    
    # find difference between environment and trait
    mismatch = sst_mean - thermal_optimum,
    
    # direction of selection
    direction = sign(mismatch),
    
    # move the trait towards the environment, with noise added by a normal
    # probability distribution abs() and direction ensure direction of
    # adaptation
    thermal_trait_change = 
      
      # since this is taken from a normal distribution, find the mean pick
      mean(abs(rnorm(10000,
                     mean=0,
                     sd=adaptive_rate))) * direction,
    
    # prevent adaptation past the environment
    thermal_trait_change_cropped = 
      ifelse(abs(thermal_trait_change) > abs(mismatch),
             mismatch,
             thermal_trait_change),
    
    # apply directional adaptation
    thermal_optimum_adapted = thermal_optimum + thermal_trait_change_cropped
  ) |> 
  
  # cluster homogenisation
  # group by cluster ID
  dplyr::group_by(category,cluster_id) |> 
  
  # retain old thermal optimum
  mutate(thermal_optimum_homogenised = thermal_optimum_adapted) |> 
  
  # calculate the weighted mean of each cluster for each trait, then move
  # the trait values towards that mean by 50% of the difference.
  dplyr::mutate(dplyr::across(c(thermal_optimum_homogenised,contains("neutral")),
                              ~ .x + ((mean(.x)-.x)*0.5))) |> 
  
  # the impact of trait homogenisation
  mutate(
    
    # increase or decrease
    gene_flow_consequence =
      ifelse(abs(thermal_optimum_homogenised - sst_mean) < # difference between trait homogenisation traits and SST
               abs(thermal_optimum_adapted - sst_mean), # difference between adaptation only and SST
             "Enhances adaptation",
             "Inhibits adaptation"),
    
    # neutral
    gene_flow_consequence =
      ifelse(abs(thermal_optimum_homogenised - sst_mean) == # difference between trait homogenisation traits and SST
               abs(thermal_optimum_adapted - sst_mean), # difference between adaptation only and SST
             "No trait homogenisation",
             gene_flow_consequence),
    
    # maladaptation
    gene_flow_consequence =
      ifelse(abs(thermal_optimum_homogenised - sst_mean) > # difference between trait homogenisation traits and SST
               abs(thermal_optimum_start - sst_mean), # difference between start trait and SST
             "Causes maladaptation",
             gene_flow_consequence),
    
    # trait homogenisation consequence as factor
    gene_flow_consequence = factor(gene_flow_consequence,
                                   levels = c("Enhances adaptation",
                                              "No trait homogenisation",
                                              "Inhibits adaptation",
                                              "Causes maladaptation")),
    
    # trait homogenisation consequence grouped
    gene_flow_consequence_group = ifelse(gene_flow_consequence == "Enhances adaptation",
                                         "Positive",
                                         "Negative"),
    gene_flow_consequence_group = ifelse(gene_flow_consequence == "No trait homogenisation",
                                         "Neutral",
                                         gene_flow_consequence_group)
  )

# create cluster polygons ------------------------------------------------------
cluster_rast <-
  list()

cluster_polygons <-
  list()

for(cat in unique(species_information$category)){
  
  # filter by run
  sp_range <-
    species_information |> 
    filter(category == cat)
  
  # create a raster object
  cluster_rast[[cat]] <-
    sp_range |> 
    select(x,y,cluster_id) |> 
    mutate(cluster_id = as.numeric(factor(cluster_id))) |> 
    rast(crs = crs(rast()))
  
  # convert to points vector
  sp_points <-
    as.points(cluster_rast[[cat]]$cluster_id)
  
  # buff by dispersal distance
  sp_poly_buffed <-
    buffer(sp_points,
           width = (unique(sp_range$dispersal_range)*1000)-10000)
  
  # aggregate into clusters
  cluster_polygons[[cat]] <-
    aggregate(sp_poly_buffed,by = "cluster_id") |> 
    mutate(category = cat) |> 
    left_join(parameters)
}

dispersal_polygons <-
  vect(cluster_polygons)

# export -----------------------------------------------------------------------
# species information
saveRDS(species_information_evo,
          "./results/conceptual_clusters/02_spp_info_evo.rds") #output

# dispersal polygons
writeVector(dispersal_polygons,
            "./results/conceptual_clusters/02_range_polygons.gpkg", #output
            overwrite = T)
