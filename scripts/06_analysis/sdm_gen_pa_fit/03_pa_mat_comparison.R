# set --------------------------------------------------------------------------
library(gen3sisExtra)
library(tidyverse)
library(progress)

# load -------------------------------------------------------------------------
sdm_pa_mat <-
  readRDS("./results/sdm_gen_pa_fit/02_sdm_pa_mat_wrangled.rds") #input

gen_pa_mats <-
  readRDS("./results/sdm_gen_pa_fit/02_gen_pa_mats_wrangled.rds") #input

# calculate metrics ------------------------------------------------------------
rmse <-
  pearson <-
  spearman <-
  jaccard <-
  mean_sr <-
  
  c()

pb <- 
  progress_bar$new(total = length(gen_pa_mats),
                   format = ":current of :total [:bar] :eta")

for(i in 1:length(gen_pa_mats)){
  
  pb$tick()
  
  if(!is.na(gen_pa_mats[i])){
    
    # species richness per cell mean
    sr_sdm <-
      rowSums(sdm_pa_mat)
    
    gen_sdm <-
      rowSums(gen_pa_mats[[i]])
    
    # mean species richness
    mean_sr[i] <-
      abs(sr_sdm-gen_sdm) |> mean()
    
    # root mean squared error
    rmse[i] <-
      sqrt((sdm_pa_mat - gen_pa_mats[[i]]) ^ 2) |> 
      mean()
    
    # pearson and spearman correlations in per cell species richness
    # correlations (NA values for globally extinct)
    pearson[i] <-
      cor(c(sr_sdm),
          c(gen_sdm),
          method = "pearson")
    
    spearman[i] <-
      cor(c(sr_sdm),
          c(gen_sdm),
          method = "spearman")
    
    # jaccard
    jaccard[i] <-
      calculateJaccardPA(sdm_pa_mat,
                         gen_pa_mats[[i]]) |>
      pull(jaccard) |>
      mean()
    
  }else{
    rmse[i] <-
      pearson[i] <-
      spearman[i] <-
      jaccard[i] <-
      mean_sr[i] <-
      NA
  }
}

results <-
  tibble(run_id = 1:length(jaccard),
         species_richness = mean_sr,
         pa_rmse_mean = rmse,
         pa_pearson = pearson,
         pa_spearman = spearman,
         pa_jaccard = jaccard)

# export -----------------------------------------------------------------------
write_csv(results,
          "./results/sdm_gen_pa_fit/fit_results.csv") #output
