# set --------------------------------------------------------------------------
library(tidyverse)
library(segmented)

# load -------------------------------------------------------------------------
model_data <-
  readRDS("./results/timeseries/delta_wrangled_for_modelling.rds") #input

# extinctions ------------------------------------------------------------------
# wrangle
cohesion_data <-
  model_data |> 
  filter(metric == "cohesion_mean") |> 
  group_by(year,metric,window) |> 
  
  # mean across all simulations
  reframe(delta = mean(delta),
          delta_sst = mean(delta_sst))

# plot to have a look at the response
plot_response <-
  ggplot(cohesion_data) +
  geom_point(aes(x = delta_sst,
                 y = delta)) +
  facet_wrap(~window,scales = "free")

# plot response variable histogram
plot_histogram <-
  ggplot(cohesion_data) +
  geom_histogram(aes(x = delta))

# The response looks normally distributed and linear.

# split into each window
window_splits <-
  cohesion_data |> 
  ungroup() |> 
  group_split(window)

# fit a segmented regression to each window size
fits <-
  list()

for(window in 1:length(window_splits)){
  
  fits[[window]] <-
    lm(delta ~ delta_sst,
       data = window_splits[[window]])
  
}

# compile model fits
time_window <- c()
est <- c()
p <- c()
r <- c()
r_adj <- c()
df <- c()

for(window in 1:length(window_splits)){
  
  time_window[window] <-
    window
  
  # extract model information
  summary_obj <-
    summary(fits[[window]])
  
  # coefficients
  coef_obj <-
    summary_obj$coefficients |> 
    as_tibble(rownames = "info")
  
  est[window] <-
    coef_obj$Estimate[2]
  
  p[window] <-
    coef_obj$`Pr(>|t|)`[2]
  
  r[window] <-
    summary_obj$r.squared
  
  r_adj[window] <-
    summary_obj$adj.r.squared
  
  df[window] <-
    summary_obj$df[2]
  
}

# compile model information
model_info <-
  tibble(time_window,
         est,
         p,
         r,
         r_adj,
         df) |>
  
  # apply bonferroni p-value correction
  mutate(p = p*length(time_window))

# export -----------------------------------------------------------------------
saveRDS(list(fit_lm = fits,
             model_information = model_info),
        "./results/timeseries/model_cohesion.rds") #output



