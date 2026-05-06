# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
metrics <-
  readRDS("./results/latitudinal_diversity_gradient/latitudinal_metrics.rds") |> #input
  filter(year == 2100)

peaks <-
  readRDS("./results/latitudinal_diversity_gradient/peak_metrics.rds")  |> #input
  filter(year == 2100)

# test for gaussian response ---------------------------------------------------
runs <-
  unique(metrics$run_id)

gaussian_summaries <-
  list()

for(run in runs){
  
  gaussian_summaries[[paste0("run_",run)]] <-
    list()
  
  for(hemi in c(-1,1)){
    
    # extract values -----------------------------------------------------------
    # extract metrics
    run_metrics <-
      metrics |> 
      filter(run_id == run,
             hemisphere == hemi)
    
    # skip if there are insufficient data (mostly extinct)
    if(dim(run_metrics)[1] > 10){
      
      # extract peaks
      peak_metrics <-
        peaks |> 
        filter(run_id == run,
               hemisphere == hemi)
      
      # extract predictor and response -----------------------------------------
      trimmed_metrics <-
        run_metrics |> 
        filter(richness_mean > (max(richness_mean) * 0.05))
      
      # latitude
      x <-
        trimmed_metrics |> 
        pull(y)
      
      # richness response
      y <-
        trimmed_metrics |> 
        pull(richness_mean)
      
      # first, try to fit a uniform distribution -------------------------------
      uniform_model <-
        lm(y ~ 1)
      
      # extract starting point estimates ---------------------------------------
      starting_values <-
        list(
          
          # peak richness
          lat_peak_richness = 
            peak_metrics |> 
            pull(richness_mean),
          
          # peak latitude
          lat_peak_latitude =
            peak_metrics  |> 
            pull(y),
          
          # standard deviation
          lat_peak_sd =
            peak_metrics |> 
            pull(sd) |> sqrt()
        )
      
      # test and record fit ----------------------------------------------------
      # test gaussian fit
      gauss_model <-
        
        # skip if nls() doesn't converge
        try(
          
          # try to fit a gaussian model
          nls(
            y ~ lat_peak_richness * exp(-((x - lat_peak_latitude)^2) / (2 * lat_peak_sd^2)),
            
            start = starting_values,
            
            control = list(maxiter = 500)
            
          ),
          silent = TRUE)
      
      # skip if there is no convergence
      if(class(gauss_model) == "try-error"){
        
        gaussian_summaries[[paste0("run_",run)]][[paste0("h_",hemi)]] <-
          tibble()
        
      }else{
        
        # fit summary 
        summary_object <-
          summary(gauss_model)
        
        gaussian_summaries[[paste0("run_",run)]][[paste0("h_",hemi)]] <-
          as_tibble(summary_object$coefficients,
                    rownames = "parameter") |> 
          
          mutate(hemisphere = hemi,
                 run_id = run)
        
        # set as null if uniform model fits better
        null_comparison <-
          AIC(uniform_model,
              gauss_model)
        
        best_fit <-
          null_comparison |> 
          as_tibble(rownames = "model") |> 
          filter(AIC == min(AIC)) |> 
          pull(model)
        
        if(best_fit == "uniform_model"){
          
          gaussian_summaries[[paste0("run_",run)]][[paste0("h_",hemi)]] <-
            tibble()
        }
        
      }
      
      # end of data paucity if statement
    }else{
      gaussian_summaries[[paste0("run_",run)]][[paste0("h_",hemi)]] <-
        tibble()
    }
    
  } # end hemisphere
  gaussian_summaries[[paste0("run_",run)]] <-
    do.call(rbind.data.frame,
            gaussian_summaries[[paste0("run_",run)]])
  
} # end run

gaussian_summaries <-
  do.call(rbind.data.frame,
          gaussian_summaries) |> 
  
  # Bonferroni corrections
  mutate(sig_bon = ifelse((`Pr(>|t|)`*1000) < 0.05,
                          TRUE,FALSE)) 

# export -----------------------------------------------------------------------
write_csv(gaussian_summaries,
          "./results/latitudinal_diversity_gradient/peak_gaussian_summaries.csv") #output


