# set --------------------------------------------------------------------------
library(tidyverse)
library(scales)
library(akima)
library(cowplot)
library(progress)
library(patchwork)

# load -------------------------------------------------------------------------
results <-
  read_csv("./data_processed/configs/partial_homogenisation/config_parameters.csv") |>  #input
  left_join(read_csv("G:/gen_future/results/04_metrics_simulation/04_metrics_simulation_0.csv"))  #input

# wrangle ----------------------------------------------------------------------
# set some metrics to zero with global extinction
results <-
  results |> 
  mutate(
    across(.cols = c(species_richness_cell_mean:abundance_cell_mean,
                     occupied_cells,
                     species_richness_global),
           ~ ifelse(is.na(.x),0,.x))
    ) |> 
  
  # calculate
  #   effective inter cluster distances
  #   scaled betweenness
  mutate(icd_effective = inter_cluster_distance_mean / dispersal_range,
         betweenness_scaled = betweenness_mean / cluster_size_mean_mean)

# extract simulation metrics
predictors <-
  c("dispersal_range","adaptive_rate")

responses <-
  results |> 
  select(-c(all_of(predictors),"run_id","timestep","seed")) |> 
  colnames()

# pivot longer across response metrics
results_long <-
  results |> 
  pivot_longer(cols = all_of(responses),
               names_to = "response",
               values_to = "response_value")


# plot -------------------------------------------------------------------------

pb <-
  progress_bar$new(total = length(responses),
                   format = "params vs metrics :current of :total :eta")

for(target_response in responses){
  
  pb$tick()
  
  # filter target metric
  plot_data <-
    results_long |> 
    filter(response == target_response)
  
  # dispersal plot ----
  disp_plot <-
    ggplot(plot_data) +
    geom_point(aes(x = dispersal_range,
                   y = response_value,
                   colour = adaptive_rate)) +
    
    scale_colour_gradient(high = "#0FC2C0",
                          low = "#023535") +
    theme_classic() +
    ylab(target_response)
  
  # adaptive pot ----
  adapt_plot <-
    ggplot(plot_data) +
    geom_point(aes(x = adaptive_rate,
                   y = response_value,
                   colour = dispersal_range)) +
    
    scale_colour_gradient(high = "#FF0D22",
                          low = "#52040B") +
    ylab(target_response) +
    theme_classic()
  
  # heat map ----
  # extract target data
  heat_data <-
    results_long |> 
    filter(response == target_response) |> 
    mutate(dispersal_range = rescale(dispersal_range),
           adaptive_rate   = rescale(adaptive_rate)) |>
    mutate(response_value = ifelse(is.infinite(response_value),NA,response_value)) |> 
    na.omit()
    
  
  # interpolate to create a consistent grid
  interpolated <-
    with(heat_data,
         akima::interp(x = dispersal_range,
                       y = adaptive_rate,
                       z = response_value,
                       nx=100, ny=100))
  
  # wrangle
  results_matrix <-
    interpolated$z
  
  dimnames(results_matrix) <-
    list(interpolated$x,
         interpolated$y)
  
  heat_plot_data <-
    results_matrix |> 
    as_tibble(rownames = "dispersal_range") |> 
    pivot_longer(cols = -dispersal_range,
                 names_to = "adaptive_rate",
                 values_to = "metric_value") |> 
    mutate(across(.cols = c(dispersal_range,adaptive_rate),
                  as.numeric))
  
  # plot
  heat_plot <-
    ggplot(heat_plot_data) +
    
    # tiles
    geom_tile(aes(x=dispersal_range,
                  y=adaptive_rate,
                  fill = metric_value)) +
    
    geom_contour(aes(x=dispersal_range,
                     y=adaptive_rate,
                     z = metric_value),
                 colour = "white") +
    
    # theme
    labs(fill = str_wrap(gsub("_"," ",target_response),width = 10)) +
    scale_fill_viridis_c(na.value = "transparent") +
    theme_classic() 
  
  # compile plots
  composite_plot <-
    (disp_plot / adapt_plot) | heat_plot +
    plot_annotation(title = target_response)
  
  # export plot
  ggsave(paste0("./plots/simulation_metrics/", #output
                target_response,".jpg"),
         composite_plot,
         height = 7,
         width = 14,
         dpi = 300)
  
}
