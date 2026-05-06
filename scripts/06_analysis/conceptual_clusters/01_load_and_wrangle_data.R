# set --------------------------------------------------------------------------
# packages
library(terra)
library(tidyterra)
library(tidyverse)
library(gen3sisExtra)

# simulation
target_runs <-
  c(sim_hdha = 469,
    sim_ldha = 23,
    sim_ldla = 328,
    sim_hdla = 362)

target_timestep <- 75 # 2025 (74 = 2026)

# target species
target_species <-
  "Macropharyngodon_bipartitus" # wio

# output directory
dir_simulation <-
  "./output/partial_homogenisation/" #input

# load parameters --------------------------------------------------------------
parameters <-
  readRDS("./results/categorised_parameters.rds") |>  #input 
  filter(run_id %in% target_runs) |> 
  
  # explicit categories
  mutate(dispersal_cat = ifelse(grepl("hd",category),
                                  "high dispersal",
                                  "low dispersal"),
         adaptive_cat = ifelse(grepl("ha",category),
                                "high adaptive rate",
                                "low adaptive rate")) |> 
  select(-c(run_id,seed)) |> 
  
  # set category traits to be the same
  group_by(dispersal_cat) |> 
  mutate(dispersal_range = mean(dispersal_range)) |> 
  group_by(adaptive_cat) |> 
  mutate(adaptive_rate = mean(adaptive_rate))

# load and wrangle species information -----------------------------------------
spp_list <-
  list()

for(target_run in target_runs){
  
  # read in species object
  sp_tmp <-
    readRDS(paste0(dir_simulation,target_run,
                   "/species/species_t_",target_timestep,".rds"))
  
  # rename by species IDs
  names(sp_tmp) <-
    speciesIDs(sp_tmp,
               "species_name")
  
  # extract target species
  sp_tmp <-
    sp_tmp[target_species] |> 
    speciesDF("species_name") |> 
    as_tibble() |> 
    mutate(run_id = target_run)
  
  # add to list
  spp_list[[paste0("run_",target_run)]] <-
    sp_tmp
}

# combine species information
spp_df <-
  do.call(rbind.data.frame,spp_list) |> 
  select(run_id, cell,species,
         cluster_id,dispersal, thermal_optimum)

# load environmental information -----------------------------------------------
# years
years <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F)

# seascape
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds")[[1]] |> #input
  as_tibble(rownames = "cell") |> 
  filter(!is.na(y_2100)) |> 
  pivot_longer(cols = contains("y_"),
               names_to = "year",
               values_to = "sst_mean") |> 
  left_join(years) |> 
  mutate(year = gsub("y_","",year)) |> 
  filter(timestep %in% c(#target_timestep,
                         target_timestep - 1)) |> 
  select(-timestep)

# mean change through simulation
delta_sst_mean <-
  read_csv("./data_processed/seascapes/delta_sst_mean.csv",
           show_col_types = F) |> 
  pull(delta_sst) |> 
  abs() |> 
  mean()

# add environmental information ------------------------------------------------
spp_env <-
  spp_df |> 
  left_join(seascape) |> 
  filter(x > 30, x < 62,
         y > -20, y < -5)

# set the cells manually -------------------------------------------------------
manual_clusters <-
  list(
    # African coastline, south
    # africa_south = tibble(cell = c(36582,36942,37302
    #                                ),
    #                       cluster_id = "africa_south"),
    
    # African coastline, north
    # africa_north = tibble(cell = c(34420,34421,34780#,34781,35141,35500,35501,35861
    #                                ),
    #                       cluster_id = "africa_north"),
    
    # Scattered Islands
    scattered_islands = tibble(cell = c(36584,36944,36945,36946
                                        ),
                               cluster_id = "scattered_islands"),
    
    # Scattered Islands
    scattered_islands_solo = tibble(cell = 35867,
                               cluster_id = "scattered_islands_solo"),
    
    # Madagascar, south
    madagascar_south = tibble(cell = c(39104,38744,38384
                                       ),
                              cluster_id = "madagascar_south"),
    
    # Madagascar, south
    madagascar_south_solo = tibble(cell = 38026,
                              cluster_id = "madagascar_south_solo"),
    
    # Madagascar, north
    madagascar_north = tibble(cell = c(36949,36590
                                       ),
                              cluster_id = "madagascar_north"),
    
    # Mauritius, Renunion, and Vingt Cinq
    east_isles = tibble(cell = c(38395
                                 ),
                        cluster_id = "east_isles")
  )

manual_clusters <-
  do.call(rbind.data.frame,manual_clusters) |> 
  mutate(cell = as.character(cell))

# set cluster IDs --------------------------------------------------------------
cluster_ids_list <-
  list(
    
    # high dispersal
    hd_ha = tibble(manual_clusters,
                   category = "hd_ha") |> 
      mutate(cluster_id = "all"),
    
    hd_la = tibble(manual_clusters,
                   category = "hd_la") |> 
      mutate(cluster_id = "all"),
    
    # low dispersal
    ld_ha = tibble(manual_clusters,
                   category = "ld_ha"),
    
    ld_la = tibble(manual_clusters,
                   category = "ld_la")
  )

cluster_ids <-
  do.call(rbind.data.frame,cluster_ids_list)

# set the thermal optima -------------------------------------------------------
# all start with the same thermal niche
 thermal_optima <-
   seascape |>
   filter(cell %in% manual_clusters$cell) |> 
   mutate(thermal_optimum = sst_mean - delta_sst_mean) |> 
   select(cell,thermal_optimum)

# combine species information --------------------------------------------------
species_information <-
  parameters |>
  left_join(cluster_ids) |> 
  left_join(thermal_optima) |> 
  left_join(seascape) |> 
  relocate(c(x,y), .before = everything())

# export -----------------------------------------------------------------------
write_csv(species_information,
          "./results/conceptual_clusters/01_spp_info.csv") #output
