# set --------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# load meta --------------------------------------------------------------------
# metric names
metric_names <-
  read_csv("./results/metric_descriptions.csv", #input
           show_col_types = F) |> 
  filter(grepl("simulation",file)) |> 
  select(column,clean_name) |> 
  rename(metric = column) |> 
  rbind(tibble(metric = "extinctions",
               clean_name = "Extinctions")) |> 
  rbind(tibble(metric = "extinctions_by_popmetrics",
               clean_name = "Extinctions"))

# load models ------------------------------------------------------------------
# extinctions
extinctions <-
  readRDS("./results/timeseries/model_extinctions.rds") #input

# extinctions by pop metrics
extinctions_by_popmetrics <-
  readRDS("./results/timeseries/model_extinction_by_popmetrics.rds") #input

# occupied cells
occupied_cells <-
  readRDS("./results/timeseries/model_occupied_cells.rds") #input

# cohesion
cohesion <-
  readRDS("./results/timeseries/model_cohesion.rds") #input

# range size
range_size_mean <-
  readRDS("./results/timeseries/model_range_size_mean.rds") #input

# fragementation
fragmentation_mean <-
  readRDS("./results/timeseries/model_fragmentation_mean.rds") #input

# betweenness
betweenness_mean <-
  readRDS("./results/timeseries/model_betweenness_mean.rds") #input

# wrangle ----------------------------------------------------------------------
# extinctions data
data_extinction <-
  extinctions$model_information |> 
  select(time_window,
         seg_p_1,
         seg_r_adj) |> 
  mutate(significant = seg_p_1 <= 0.05,
         metric = "extinctions") |> 
  rename(p = seg_p_1,
         adj_r_squared = seg_r_adj)

# extinctions by popmetrics data
data_extinctions_by_popmetrics <-
  extinctions_by_popmetrics$model_information |> 
  select(time_window,
         p,
         r_adj) |> 
  mutate(metric = "extinctions_by_popmetrics",
         significant = p <= 0.05) |> 
  rename(adj_r_squared = r_adj)

# occupied cells data
data_occ_cells <-
  occupied_cells$model_information |> 
  select(time_window,
         p,
         r_adj) |> 
  mutate(significant = p <= 0.05,
         metric = "occupied_cells") |> 
  rename(adj_r_squared = r_adj)

# cohesion data
data_cohesion <-
  cohesion$model_information |> 
  select(time_window,
         p,
         r_adj) |> 
  mutate(significant = p <= 0.05,
         metric = "cohesion_mean") |> 
  rename(adj_r_squared = r_adj)

# range_size data
data_range_size <-
  range_size_mean$model_information |> 
  select(time_window,
         p,
         r_adj) |> 
  mutate(significant = p <= 0.05,
         metric = "range_size_mean") |> 
  rename(adj_r_squared = r_adj)

# fragmentation data
data_fragmentation <-
  fragmentation_mean$model_information |> 
  select(time_window,
         p,
         r_adj) |> 
  mutate(significant = p <= 0.05,
         metric = "fragmentation_mean") |> 
  rename(adj_r_squared = r_adj)

# betweenness data
data_betweenness <-
  betweenness_mean$model_information |> 
  select(time_window,
         p,
         r_adj) |> 
  mutate(significant = p <= 0.05,
         metric = "betweenness_mean") |> 
  rename(adj_r_squared = r_adj)

# compile data
data_compiled <-
  do.call(rbind.data.frame,
          list(data_extinctions_by_popmetrics,
               data_extinction,
               data_occ_cells,
               data_range_size,
               data_cohesion,
               data_fragmentation,
               data_betweenness))

# plot -------------------------------------------------------------------------
# SST to vulnerability metrics
plot_vulnerability_metrics <-
  
  ggplot(data_compiled |> 
           
           filter(metric %in% c("fragmentation_mean",
                                "betweenness_mean",
                                "occupied_cells",
                                "range_size_mean",
                                "cohesion_mean")) |> 
           
           left_join(metric_names)) +
  
  geom_point(aes(x = time_window,
                 y = adj_r_squared,
                 fill = significant),
             shape = 21) +
  
  facet_grid(rows = vars(str_wrap(clean_name,10))) +
  
  labs(x = "Time window (years)",
       fill = "P-value < 0.05\nBonferroni corrected",
       y = "Adjusted\nR-squared",
       title = "Predicting ecological metrics\nfrom sea surface temperature change") +
  
  scale_fill_manual(values = c("white","black")) +
  theme_bw() +
  
  theme(text = element_text(family = "serif"))

# SST to extinctions
plot_extinctions <-
  
  ggplot(data_compiled |> 
           filter(metric %in% c("extinctions")) |> 
           
           left_join(metric_names)) +
  
  geom_point(aes(x = time_window,
                 y = adj_r_squared,
                 fill = significant),
             shape = 21) +
  
  facet_grid(rows = vars(clean_name)) +
  
  labs(x = "Time window (years)",
       fill = "P-value < 0.05\nBonferroni corrected",
       y = "Adjusted\nR-squared",
       title = "Predicting extinctions\nfrom sea surface temperature change") +
  
  scale_fill_manual(values = c("white","black")) +
  theme_bw() +
  
  theme(text = element_text(family = "serif"))

# vulnerability metrics to extinctions
plot_vul_ext <-
  
  ggplot(data_compiled |> 
           filter(metric %in% c("extinctions_by_popmetrics")) |> 
           
           left_join(metric_names)) +
  
  geom_point(aes(x = time_window,
                 y = adj_r_squared,
                 fill = significant),
             shape = 21) +
  
  facet_grid(rows = vars(clean_name)) +
  
  labs(x = "Time window (years)",
       fill = "P-value < 0.05\nBonferroni corrected",
       y = "Adjusted\nR-squared",
       title = "Predicting extinctions\nfrom ecological metrics") +
  
  scale_fill_manual(values = c("white","black")) +
  theme_bw() +
  
  theme(text = element_text(family = "serif"))

# compile plots
plot_compilation <-
  plot_vulnerability_metrics /
  plot_spacer() /
  plot_extinctions /
  plot_spacer() /
  plot_vul_ext +
  plot_annotation(tag_levels = "a") +
  plot_layout(heights = c(5,0.2,1,0.2,1),
              guides = "collect",
              axes = "collect_x")

# export -----------------------------------------------------------------------
ggsave("./plots/timeseries/multi_step_extinctions.jpg", #output
       plot_compilation,
       units = "mm",
       height = 225,
       width  = 225,
       dpi = 300)
