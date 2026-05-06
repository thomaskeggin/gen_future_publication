# set --------------------------------------------------------------------------
library(tidyverse)
library(gen3sisExtra)
library(scales)

point_size <-
  3

point_colour_alpha <-
  0.5

# load and clean ---------------------------------------------------------------
# response peaks
peaks <-
  
  # load in peak model definitions
  read_csv("./results/latitudinal_diversity_gradient/peak_gaussian_summaries.csv", #input
           show_col_types = F) |> 
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

# initial peaks
peaks_initial <-
  readRDS("./results/latitudinal_diversity_gradient/initial_ldg.rds") #input

# load in the parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# join the parameters to the peaks to include all runs
results <-
  peaks |> 
  left_join(peaks_initial) |> 
  left_join(parameters,
            "run_id")

# find non-significant and extinct runs
#non-sig or missing
missing_one_hemi <-
  peaks |> 
  group_by(run_id) |> 
  reframe(n = n()) |> 
  filter(n == 1) |> 
  pull(run_id)

missing_two_hemi <-
  parameters$run_id[which(!parameters$run_id %in% peaks$run_id)]

non_gaussian <-
  c(missing_one_hemi,
    missing_two_hemi)

# wrangle ----------------------------------------------------------------------
results <-
  results |> 
  mutate(lat_peak_latitude       = abs(lat_peak_latitude),
         lat_peak_latitude_start = abs(lat_peak_latitude_start),
         
         delta_richness = lat_peak_richness - lat_peak_richness_start,
         delta_latitude = lat_peak_latitude - lat_peak_latitude_start,
         delta_sd       = lat_peak_sd - lat_peak_sd_start)

# extract simulation metrics
predictors <-
  c("dispersal_range",
    "adaptive_rate")

responses <-
  c("delta_richness",
    "delta_latitude",
    "delta_sd")

results_split <-
  list()

non_gauss <-
  list()

for(hemi in unique(results$hemisphere)){
  
  # pivot longer across response metrics, and rescale and interpolate
  results_split[[hemi]] <-
    results |> 
    filter(hemisphere == hemi) |> 
    select(run_id,
           dispersal_range,
           adaptive_rate,
           all_of(responses)) |> 
    
    # scale parameters
    mutate(#dispersal_range = rescale(dispersal_range),
      #adaptive_rate   = rescale(adaptive_rate),
      across(.cols = all_of(responses),
             ~.x / max(abs(.x))
      )) |> 
    
    # pivot longer
    pivot_longer(cols = all_of(responses),
                 names_to = "response",
                 values_to = "response_value") |> 
    
    # replace infinite values
    mutate(response_value = ifelse(is.infinite(response_value),NA,response_value),
           hemisphere = hemi) |> 
    na.omit()
  
  # add non-gaussian sims
  non_gauss[[hemi]] <-
    parameters |> 
    select(run_id, dispersal_range, adaptive_rate) |> 
    filter(run_id %in% non_gaussian) |> 
    
    mutate(delta_richness = NA,
           delta_latitude = NA,
           delta_sd       = NA,
           non_gauss      = "",
           hemisphere     = hemi) |> 
    
    pivot_longer(cols = all_of(responses),
                 names_to = "response",
                 values_to = "response_value")
  
}

results_long <-
  bind_rows(results_split) |> 
  
  
  mutate(hemisphere = factor(hemisphere,
                             levels = c("Northern\nhemisphere","Southern\nhemisphere")),
         
         response = gsub("delta_latitude",
                         "Latitude of\npeak richness",
                         response),
         response = gsub("delta_richness",
                         "Species richness\nof latitudinal peak",
                         response),
         response = gsub("delta_sd",
                         "Standard deviation of\nlatitudinal response curve",
                         response)) 

non_gauss_long <-
  bind_rows(non_gauss) |> 
  
  
  mutate(hemisphere = factor(hemisphere,
                             levels = c("Northern\nhemisphere","Southern\nhemisphere")),
         
         response = gsub("delta_latitude",
                         "Latitude of\npeak richness",
                         response),
         response = gsub("delta_richness",
                         "Species richness\nof latitudinal peak",
                         response),
         response = gsub("delta_sd",
                         "Standard deviation of\nlatitudinal response curve",
                         response)) 

# plot -------------------------------------------------------------------------
aspect_ratio <-
  max(results$dispersal_range) / max(results$adaptive_rate)

composite_plot <-
  ggplot() +
  
  # gaussian
  geom_point(data = results_long,
             aes(x=dispersal_range,
                 y=adaptive_rate,
                 fill = response_value),
             colour = alpha("black",point_colour_alpha),
             shape = 21,
             size = point_size) +
  
  # non-gaussian
  geom_point(data = non_gauss_long,
             aes(x=dispersal_range,
                 y=adaptive_rate,
                 colour = non_gauss),
             size = point_size) +
  
  # layout
  facet_grid(cols = vars(response),
             rows = vars(hemisphere)) +
  
  coord_fixed(ratio = aspect_ratio) +
  
  # theme
  labs(fill = "Scaled response\nvalue",
       colour = "Non-gaussian\nresponse",
       x = "Dispersal range (km / year)",
       y = "Adaptive rate (°C / year)") +
  
  scale_fill_gradient2(high = "#2F5768",
                       mid = "white",
                       low = "#863C34",
                       na.value = "black",
                       limits = c(-1,1)) +
  scale_colour_manual(values = "black") +
  
  theme_classic() +
  theme(text = element_text(family = "serif"))

# export -----------------------------------------------------------------------
ggsave("./plots/latitudinal_diversity_gradient/parameters_v_ldg_metrics_delta.png", #output
       composite_plot,
       height = 8 * 0.7,
       width = 12 * 0.7)


