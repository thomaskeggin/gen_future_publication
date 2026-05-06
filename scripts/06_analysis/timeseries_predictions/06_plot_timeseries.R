# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# delta metrics
delta_metrics <-
  read_csv("./results/timeseries/delta_metrics_simulation.csv", #input
           show_col_types = F)

# time step information
steps_to_years <-
  read_csv("./data_processed/seascapes/steps_to_years.csv", #input
           show_col_types = F) |> 
  mutate(year = as.numeric(gsub("y_","",year)))

# simulation parameters
params <-
  read_csv("./results/categorised_parameters.csv", #input
           show_col_types = F)

# wrangle ----------------------------------------------------------------------
plot_data <-
  delta_metrics |> 
  filter(metric %in% c("species_richness_global",
                       "occupied_cells",
                       "range_size_mean")) |> 
  left_join(steps_to_years,
            by = "timestep") |> 
  left_join(params,
            by = "run_id") |> 
  mutate(gain = sign(delta))

# plot -------------------------------------------------------------------------

ggplot(plot_data) +
  geom_point(aes(x = year,
                y = delta,
                group = run_id,
                colour = gain)) +
  
  facet_grid(rows = vars(metric),
             cols = vars(category),
             scales = "free_y")
