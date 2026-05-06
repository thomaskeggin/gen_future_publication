# set --------------------------------------------------------------------------
library(tidyverse)
library(corrplot)

# load -------------------------------------------------------------------------
model_data <-
  readRDS("./results/timeseries/delta_metrics_simulation_windows.rds") #input

yearly_extinctions <-
  read_csv("./results/timeseries/delta_metrics_simulation.csv") |> #input
  filter(metric == "species_richness_global") |> 
  group_by(timestep) |> 
  reframe(extinctions = -mean(delta,na.rm=T))

# wrangle ----------------------------------------------------------------------
vul_metrics <-
  c("abundance_species_mean",
    "fragmentation_mean",
    #"range_size_mean",
    #"occupied_cells",
    "cohesion_mean",
    #"inter_cluster_distance_mean",
    "betweenness_mean")
  
extinction_data <-
  model_data |> 
  filter(metric %in% c(vul_metrics)) |>
  group_by(timestep,metric,window) |> 
  reframe(value = mean(delta,na.rm = T)) |> 
  left_join(yearly_extinctions)

# evaluate for modelling -------------------------------------------------------
predictors <-
  extinction_data |> 
  select(timestep,metric,window,value) |> 
  pivot_wider(names_from = metric,
              values_from = value)

# correlations
correlations <-
  cor(predictors[,3:6])

corrplot(correlations)

plot(predictors[,3:6])

# histograms
ggplot(extinction_data) +
  geom_density(aes(x = value,
                   fill = factor(window)),
               alpha = 0.1) +
  
  facet_wrap(~metric,
             scales = "free")

ggplot(yearly_extinctions) +
  geom_histogram(aes(x = extinctions)) +
  geom_histogram(aes(x = log(extinctions), fill = "red"))

# model ------------------------------------------------------------------------
# split into each window
window_splits <-
  extinction_data |> 
  mutate(ext_sum_log  = log(extinctions),
         ext_mean_log = log(extinctions)) |>
  pivot_wider(names_from = metric,
              values_from = value) |> 
  
  group_split(window)

# fit a multiple linear regression to each window size
fits <-
  list()

summaries <-
  list()

r_adj <-
  c()

p <-
  c()

df <-
  c()

for(window in 1:length(window_splits)){
  
  fits[[window]] <-
    lm(ext_mean_log ~ 
         betweenness_mean +
         cohesion_mean +
         fragmentation_mean +
         abundance_species_mean,
       data = window_splits[[window]])
  
  summaries[[window]] <-
    summary(fits[[window]])
  
  r_adj <-
    c(r_adj,
      summaries[[window]]$adj.r.squared)
  
  f <-
    summaries[[window]]$fstatistic
  
  p <-
    c(p,
      pf(f[1],f[2],f[3],lower.tail=F))
  
  df <-
    c(df,
      summaries[[window]]$df[2])
  
  
}

model_info <-
  tibble(time_window = 1:length(window_splits),
         r_adj  = r_adj,
         p      = p,
         df     = df) |> 
  mutate(significant = p < 0.05) |>
  
  # apply bonferroni p-value correction
  mutate(p = p*length(time_window))

# export -----------------------------------------------------------------------
saveRDS(list(fit_lm = fits,
             model_information = model_info),
        "./results/timeseries/model_extinction_by_popmetrics.rds") #output

























