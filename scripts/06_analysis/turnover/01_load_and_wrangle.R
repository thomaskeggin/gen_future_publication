# outputs: extinctions / colonisations / species turnover / Jaccard similarity

# set --------------------------------------------------------------------------
library(tidyverse)
library(progress)
library(gen3sisExtra)

# load -------------------------------------------------------------------------
# species start
species_start <-
  readRDS("./data_processed/species/species_initialisation_information.rds") #input

# timesteps to years
t2y <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F) |> 
  mutate(year = gsub("y_","",year) |> as.numeric())

# outputs
dir_outputs <-
  "./output/partial_homogenisation/" #input

# categories
categories <-
  readRDS("./results/categorised_parameters.rds") #input

# landscape object
landscape_object <-
  readRDS("./data_processed/seascapes/landscapes.rds") #input

# initial presence/absence data frame ------------------------------------------
pa_start <-
  list()

for(sp in names(species_start)){
  
  pa_start[[sp]] <-
    tibble(species = sp,
           cell    = names(species_start[[sp]]$abundance))
}

pa_start <-
  do.call(rbind.data.frame,
          pa_start) |> 
  mutate(year = "t_1",
         present = 1)

# 2100 presence/absence data frame ---------------------------------------------
pb <-
  progress_bar$new(total = 500,
                   format = ":current of :total [:bar] :percent :eta")

col_exts <-
  list()

# loop simulations
for(sim in as.numeric(list.files(dir_outputs))){
  
  pb$tick()
  
  # colonisations, extinctions, and temporal turnover --------------------------
  
  # load in 2100 outputs
  species_t <-
    speciesDF(readRDS(paste0(dir_outputs,sim,"/species/species_t_0.rds")),
              id = "species_name")
  
  # create placeholder in case of total extinction
  if(dim(species_t)[1] == 0){
    species_t <-
      tibble(species = "placeholder",
             cell    = NA,
             year    = "t",
             present = 0)
    
    # or wrangle to match 
  }else{
    species_t <-
      species_t |> 
      select(species,cell) |> 
      mutate(year = "t",
             present = 1)
  }
  
  # bind for comparison
  compared <-
    
    # bind time steps
    rbind.data.frame(species_t,
                     pa_start) |> 
    
    # widen for calculations
    pivot_wider(names_from = year,
                values_from = present,
                values_fill = 0) |> 
    
    # if there was global extinction there will now be a "placeholder" species
    # to remove
    na.omit()
  
  
  # calculate turnover, colonisations, and extinctions -------------------------
  
  col_ext <-
    
    compared |> 
    mutate(turnover     = t - t_1,
           colonisation = turnover == 1,
           extinction   = turnover == -1) |> 
    group_by(cell) |> 
    
    # sum by cell
    reframe(colonisations = sum(colonisation),
            extinctions   = sum(extinction),
            turnover      = sum(turnover)) |> 
    
    # add year and simulation
    mutate(year = 2100,
           run_id = sim)
  
  # dis/similarity indices -----------------------------------------------------
  # Jaccard 0 = different, 1 = equal
  
  # see how many species ended up in their starting cells
  same_count <-
    
    compared |> 
    
    # see if the species persisted locally or not
    mutate(same = t == t_1) |> 
    
    # count the number of species persisting or not per cell
    group_by(cell,same) |> 
    reframe(n = n())
  
  # if no species are where they started, skip the calculation and set all
  # values to 0
  if(length(unique(same_count$same)) == 2){
    
    similarity <-
      same_count |> 
      
      # wrangle for calculations
      pivot_wider(names_from = same,
                  values_from = n,
                  values_fill = 0) |> 
      
      # calculate indices
      mutate(
        
        # total number of unique species at both time steps
        temporal_union = `FALSE` + `TRUE`,
        
        # indices
        jaccard = `TRUE` / temporal_union) |> 
      
      # clean up
      dplyr::select(cell,temporal_union,jaccard)
    
    
  }else{
    
    similarity <-
      
      same_count |> 
      
      mutate(temporal_union = n,
             jaccard = 0) |> 
      
      # clean up
      dplyr::select(cell,temporal_union,jaccard)
  }
  
  # combine metrics ------------------------------------------------------------
  col_exts[[sim]] <-
    col_ext |> 
    left_join(similarity, by = "cell")
  
}

# combine all the runs
col_exts_df <-
  do.call(rbind.data.frame,
          col_exts)

# output -----------------------------------------------------------------------
saveRDS(col_exts_df,
        "./results/turnover/01_turnover_df.rds") #output
