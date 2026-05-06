# set --------------------------------------------------------------------------
library(tidyverse)
library(gen3sisExtra)
library(progress)

# load meta --------------------------------------------------------------------
# parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") |> #input
  mutate(run_id = factor(run_id))

# regions
regions <-
  read_csv("./data_processed/realms/01_realm_ids.csv", #input
           show_col_types = F) |> 
  mutate(cell = as.character(cell)) |> 
  select(-c(x,y))

# seascape
seascape <-
  readRDS("./data_processed/seascapes/landscapes.rds") #input

seascape <-
  seascape[["sst_mean"]] |> 
  select(x,y,y_2100) |> 
  na.omit() |> 
  as_tibble(rownames = "cell")

# load cell metrics ------------------------------------------------------------
output_dir <-
  "./output/partial_homogenisation/" #input

output_sims <-
  list.files(output_dir) |> 
  as.numeric() |> 
  sort()

pb <- 
  progress_bar$new(total = length(output_sims),
                   format = ":current of :total [:bar] :eta")

peri <-
  list()

start_time <-
  Sys.time()

for(sim in output_sims){
  
  pb$tick()
  
  target_file <-
    paste0(output_dir,
           sim,
           "/species/species_t_0.rds")
  
  # process if file exists
  if(file.exists(target_file)){
    
    species_object <-
      readRDS(target_file)
    
    # process if no global extinction
    extant_species <-
      which(speciesExtant(species_object) == TRUE) |>
      length()
    
    if(extant_species > 0){
      peri[[sim]] <-
        species_object |> 
        speciesDF("species_name") |> 
        as_tibble() |> 
        
        # remove clusters less than 2 cells large
        group_by(species,cluster_id) |> 
        
        mutate(cluster_size = n()) |> 
        
        filter(cluster_size > 2) |> 
        
        left_join(seascape, by  = "cell") |> 
        
        mutate(
          
          # mismatch between thermal optimum and the environment
          thermal_fitness = (abs(y_2100 - thermal_optimum)),
          
          
          # ranked measure of environmental peripheralness of the population in thermal space
          env_peripheralness = abs(mean(y_2100)-y_2100),
          
          env_peripheralness_ranked = env_peripheralness |> rank()
          
        ) |> 
        
        select(-contains("neutral")) |> 
        
        mutate(hemisphere = sign(y),
               run_id = factor(sim, levels = 1:500))
      
      # subset - or this file will be too large to process
      sim_size <-
        dim(peri[[sim]])[1]
      
      if(sim_size > 20000){
        
        peri[[sim]] <-
          peri[[sim]][sample(1:sim_size,
                             20000),]
      }
     
      # if everyone is dead 
    }else{
      peri[[sim]] <- NA
    }
    
    # if the output file doesn't exist..
  }else{
    peri[[sim]] <- NA
  }
  
}

end_time <-
  Sys.time() # 22 min with 100k sim_size

peri_df <-
  do.call(rbind.data.frame,
          peri) |> 
  ungroup() |> 
  na.omit() |> 
  left_join(parameters, by = "run_id") |> 
  left_join(regions, by = "cell")

# export -----------------------------------------------------------------------
saveRDS(peri_df,
        "./results/range_periphery_effect/01_peripheralness.rds") #output




