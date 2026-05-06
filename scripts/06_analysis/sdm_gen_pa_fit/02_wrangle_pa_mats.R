# set --------------------------------------------------------------------------
library(tidyverse)
library(gen3sisExtra)
library(progress)

# load main --------------------------------------------------------------------
# species initialisation
initial_species <-
  readRDS("./data_processed/species/species_initialisation_information.rds") |> #input
  names()

# presence/absence matrices
# SDM
sdm_pa_mat_raw <-
  readRDS("./results/sdm_gen_pa_fit/01_sdm_pa_mat.rds") #input

# distance matrix
dist_mat <-
  readRDS("./data_processed/seascapes/distances_full/distances_full_0.rds") #input

# simulation output batch
out_dir <-
  "./output/partial_homogenisation/" #input

# wrangle SDM PA matrix --------------------------------------------------------
sdm_pa_mat <- sdm_pa_mat_raw

# fill out missing species
sdm_missing_species <-
  initial_species[which(!initial_species %in% colnames(sdm_pa_mat))]

fill_mat_sdm <-
  matrix(nrow = dim(sdm_pa_mat)[1],
         ncol = length(sdm_missing_species),
         data = 0)

colnames(fill_mat_sdm) <- sdm_missing_species

sdm_pa_mat <-
  cbind(sdm_pa_mat,
        fill_mat_sdm)

# align row and column names (cell and species)
sdm_pa_mat <-
  sdm_pa_mat[rownames(dist_mat),
             initial_species]

# load and wrangle genesis outputs ---------------------------------------------
runs <-
  list.files(out_dir)

gen_pa_mats <-
  vector(mode = "list",
         length = length(runs))

pb <-
  progress_bar$new(total = length(runs),
                   format = ":eta :percent")


for(i in 1:length(runs)){
  
  pb$tick()
  
  # NA if simulation is missing
  if(length(list.files(paste0(out_dir,i,"/species/"))) == 0){
    gen_pa_mats[[i]] <-
      NA
  }else{
    
    # if 2100 exists
    if(file.exists(paste0(out_dir,
                          i,
                          "/species/species_t_0.rds"))){
      
      # load in corresponding landscape object
      landscape_object <-
        readRDS(paste0("./output/partial_homogenisation/",i,"/landscapes/landscape_t_0.rds"))
      
      # generate the distance matrix
      gen_pa_mats[[i]] <-
        createPAM(species = readRDS(paste0("./output/partial_homogenisation/",i,"/species/species_t_0.rds")),
                  landscape = landscape_object,
                  species_id = "species_name")
      
      # fill out missing species
      gen_missing_species <-
        initial_species[which(!initial_species %in% colnames(gen_pa_mats[[i]]))]
      
      fill_mat_gen <-
        matrix(nrow = dim(gen_pa_mats[[i]])[1],
               ncol = length(sdm_missing_species),
               data = 0)
      
      colnames(fill_mat_gen) <- sdm_missing_species
      
      gen_pa_mats[[i]] <-
        cbind(gen_pa_mats[[i]],
              fill_mat_gen)
      
      # align row and column names (cell and species)
      gen_pa_mats[[i]] <-
        gen_pa_mats[[i]][rownames(dist_mat),
                         initial_species]
      
      
      
      # else empty matrix for global extinction
    }else{
      
      gen_pa_mats[[i]] <-
        sdm_pa_mat
      
      gen_pa_mats[[i]][,] <- 0
      
    }
  } # end of simulation missing check
  
}

# export -----------------------------------------------------------------------
# SDM PA matrix
saveRDS(sdm_pa_mat,
        "./results/sdm_gen_pa_fit/02_sdm_pa_mat_wrangled.rds") #output

# genesis PA matrices
saveRDS(gen_pa_mats,
        "./results/sdm_gen_pa_fit/02_gen_pa_mats_wrangled.rds") #output

