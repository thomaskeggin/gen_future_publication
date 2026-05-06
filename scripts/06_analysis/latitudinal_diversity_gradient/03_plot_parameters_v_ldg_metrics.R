# set --------------------------------------------------------------------------
library(tidyverse)
library(gen3sisExtra)
library(scales)

# load and clean ---------------------------------------------------------------
peaks <-
  
  # load in peak model definitions
  read_csv("./results/latitudinal_diversity_gradient/peak_gaussian_summaries.csv", #input
           show_col_types = F) |> 
  
  # set non-significant parameter estimates to NA and remove
  mutate(sig_bon = ifelse((`Pr(>|t|)`*1000) < 0.05,
                          TRUE,FALSE)) |> 
  na.omit() |> 
  
  # pivot parameter estimates out to be their own, linked variables
  select(run_id, hemisphere, parameter, Estimate) |> 
  pivot_wider(names_from = parameter,
              values_from = Estimate) |> 
  
  # rename the hemispheres
  mutate(hemisphere = as.character(hemisphere),
         hemisphere = gsub("-1", "Southern\nhemisphere", hemisphere),
         hemisphere = gsub("1", "Northern\nhemisphere", hemisphere)) |> 
  
  # remove that one wild outlier
  filter(abs(lat_peak_latitude) < 30)

# load in the parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# join the parameters to the peaks to include all runs
results <-
  left_join(peaks,
            parameters,
            "run_id")

# wrangle ----------------------------------------------------------------------
# extract simulation metrics
predictors <-
  c("dispersal_range",
    "adaptive_rate")

responses <-
  c("lat_peak_richness",
    "lat_peak_latitude",
    "lat_peak_sd")

# split by hemisphere and wrangle to heat plot matrix 
heat_plot_data_list <-
  list()

for(hemi in unique(results$hemisphere)){
  
  heat_plot_data_list[[paste0("hemisphere_",hemi)]] <-
    list()
  
  # pivot longer across response metrics, and rescale and interpolate
  results_split <-
    results |> 
    filter(hemisphere == hemi) |> 
    select(dispersal_range,
           adaptive_rate,
           all_of(responses)) |> 
    
    # scale parameters
    mutate(dispersal_range = rescale(dispersal_range),
           adaptive_rate   = rescale(adaptive_rate),
           across(.cols = all_of(responses),
                  ~rescale(abs(.x)))) |>
    
    # pivot longer
    pivot_longer(cols = all_of(responses),
                 names_to = "response",
                 values_to = "response_value") |> 
    
    # replace infinite values
    mutate(response_value = ifelse(is.infinite(response_value),NA,response_value)) |> 
    na.omit() |> 
    
    # split by response
    group_by(response) |> 
    group_split()
  
  # interpolate results
  for(i in 1:length(results_split)){
    
    # extract target response
    target_response <- 
      unique(results_split[[i]]$response)
    
    # apply interpolation
    heat_plot_data_list[[paste0("hemisphere_",hemi)]][[i]] <-
      
      results_split[[i]] |> 
      
      # filter for target response and parameter values
      filter(response == target_response) |> 
      select(dispersal_range,
             adaptive_rate,
             response_value) |> 
      
      # apply interpolation function
      interpolateParameterResponse() |> 
      
      # add info
      mutate(response = target_response,
             hemisphere = hemi)
    
  }
  
  heat_plot_data_list[[paste0("hemisphere_",hemi)]] <-
    do.call(rbind.data.frame,
            heat_plot_data_list[[paste0("hemisphere_",hemi)]])
  
}

# tidy for plotting
heat_plot_data_df <-
  do.call(rbind.data.frame,
          heat_plot_data_list) |> 
  
  # add line breaks to strip labels 
  mutate(hemisphere = factor(hemisphere,
                             levels = c("Northern\nhemisphere","Southern\nhemisphere")),
         
         response = gsub("lat_peak_latitude",
                         "Latitude of\npeak richness",
                         response),
         response = gsub("lat_peak_richness",
                         "Species richness\nof latitudinal peak",
                         response),
         response = gsub("lat_peak_sd",
                         "Standard deviation of\nlatitudinal response curve",
                         response)) |> 
  
  # scale parameters back to original values
  mutate(dispersal_range = rescale(dispersal_range,
                                   range(results$dispersal_range)),
         adaptive_rate = rescale(adaptive_rate,
                                 range(results$adaptive_rate)))

# plot -------------------------------------------------------------------------
aspect_ratio <-
  max(results$dispersal_range) / max(results$adaptive_rate)

composite_plot <-
  ggplot(heat_plot_data_df) +
  
  # tiles
  geom_tile(aes(x=dispersal_range,
                y=adaptive_rate,
                fill = response_value)) +
  
  geom_contour(aes(x=dispersal_range,
                   y=adaptive_rate,
                   z = response_value),
               colour = "white") +
  
  # layout
  facet_grid(cols = vars(response),
             rows = vars(hemisphere)) +
  coord_fixed(ratio = aspect_ratio) +
  
  # theme
  labs(fill = "Scaled response\nvalue",
       x = "Dispersal range (km)",
       y = "Adaptive rate (°C)") +
  
  scale_fill_gradient(high = "#D8FFFF",
                      low = "#001818",
                      na.value = "white",
                      limits = c(0,1)) +
  theme_classic()

# export -----------------------------------------------------------------------
ggsave("./plots/latitudinal_diversity_gradient/parameters_v_ldg_metrics.png", #output
       composite_plot,
       height = 8,
       width = 12)


