# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# simulation metrics
sim_metrics <-
  readRDS("./results/timeseries_characterisation/simulation_timeseries/01_simulation_metrics.rds") |>  #input
  filter(year > 2000)

# LDG metrics
peak_metrics <-
  read_csv("./results/latitudinal_diversity_gradient/peak_gaussian_summaries.csv", #input
           show_col_types = F)

# clean metric names
metric_descriptions <-
  read_csv("./results/metric_descriptions.csv", #input
           show_col_types = F) |> 
  filter(grepl("simulation",file))

# wrangle ----------------------------------------------------------------------
# simulations experiencing global extinction
extinct_sims <-
  sim_metrics |> 
  filter(timestep == 0,
         species_richness_global |> is.na()) |> 
  pull(run_id)

# simulations with non-gaussian LDG instances
all_sims <-
  1:500

extant_sims <-
  all_sims[!all_sims %in% extinct_sims]

gauss_n <-
  peak_metrics |> 
  filter(hemisphere == 1) |> 
  select(run_id) |> 
  distinct() |> 
  mutate(north = 1)

gauss_s <-
  peak_metrics |> 
  filter(hemisphere == -1) |> 
  select(run_id) |> 
  distinct() |> 
  mutate(south = 1)

non_gauss <-
  tibble(run_id = all_sims) |> 
  left_join(gauss_n) |> 
  left_join(gauss_s) |> 
  mutate(double_gauss = north + south,
         extinct = run_id %in% extinct_sims) |> 
  filter(extinct == FALSE,
         is.na(double_gauss)) |> 
  pull(run_id)

# simulations experiencing some extinction
extinction_sims <-
  sim_metrics |> 
  filter(timestep == 0) |> 
  pull(run_id)

sim_metrics_ext <-
  sim_metrics |> 
  filter(run_id %in% c(extinction_sims,extinct_sims)) |> 
  mutate(status = "LDG present",
         status = ifelse(run_id %in% extinct_sims,
                         "Global extinction",
                         status),
         status = ifelse(run_id %in% non_gauss,
                         "LDG collapse",
                         status),
         status = factor(status,
                         levels = c("Global extinction",
                                    "LDG collapse",
                                    "LDG present")),
         
         icd_effective = log(inter_cluster_distance_mean / dispersal_range),
         betweenness_mean = log(betweenness_mean))

# pivot for target metrics
all_metrics <-
  c("species_richness_global",
    "species_richness_cell_mean",
    "abundance_global_total",
    "abundance_cell_mean",
    "abundance_species_mean",
    "occupied_cells",
    "range_size_mean",
    "cohesion_mean",
    "betweenness_mean",
    "fragmentation_mean")

target_metrics_clean <-
  metric_descriptions |> 
  filter(column %in% all_metrics) |> 
  arrange(match(column, all_metrics)) |> 
  pull(clean_name) |> 
  str_wrap(15)

vul <-
  sim_metrics_ext |> 
  pivot_longer(cols = all_of(all_metrics)) |> 
  
  select(run_id:timestep,
         status,
         year,
         name,
         value) |> 
  
  mutate(name_clean = name)

# make metric names clean
for(i in 1:length(all_metrics)){
  
  vul$name_clean <-
    gsub(all_metrics[i],
         target_metrics_clean[i],
         vul$name_clean)
}

vul$name_clean <-
  factor(vul$name_clean,
         levels = target_metrics_clean)

# export -----------------------------------------------------------------------
saveRDS(vul,
        "./results/timeseries_characterisation/simulation_timeseries/02_simulation_timeseries.rds") #output
