# set --------------------------------------------------------------------------
library(tidyverse)
library(segmented)

plots <- list()
model_information <- list()

# load -------------------------------------------------------------------------
model_data <-
  readRDS("./results/timeseries/delta_wrangled_for_modelling.rds") #input

# extinctions ------------------------------------------------------------------
# wrangle
extinction_data <-
  model_data |> 
  filter(metric == "species_richness_global") |> 
  group_by(year,metric,window) |> 
  reframe(delta = mean(delta),
          delta_sst = mean(delta_sst))

# plot to check
# ggplot(extinction_data) +
#   geom_point(aes(x = delta_sst,
#                  y = log(-delta))) +
#   facet_wrap(~window,scales = "free")

# split into each window
window_splits <-
  extinction_data |> 
  mutate(ext_sum_log  = log(-delta),
         ext_mean_log = log(-delta)) |> 
  filter(!is.na(delta_sst),
         !is.infinite(ext_sum_log)) |> 
  ungroup() |> 
  group_split(window)

# fit a segmented regression to each window size
fits <-
  list(fits_lm  = list(),
       fits_seg = list())

for(window in 1:length(window_splits)){
  
  fits$fits_lm[[window]] <-
    lm(ext_mean_log ~ delta_sst,
       data = window_splits[[window]])
  
  fits$fits_seg[[window]] <-
    segmented(fits$fits_lm[[window]],
              seg.Z = ~ delta_sst,
              psi = mean(window_splits[[window]]$delta_sst,
                         na.rm = T))
}

# compile model fits
time_window <- c()
seg_break_est <- c() # breakpoint estimate
seg_break_err <- c() # breakpoint error
seg_est_1 <- c()
seg_est_2 <- c()
seg_p_1 <- c()
seg_p_2 <- c()
seg_r <- c()
seg_r_adj <- c()
df <- c()

for(window in 1:length(window_splits)){
  
  time_window[window] <-
    window
  
  # extract model information
  summary_obj <-
    summary(fits$fits_seg[[window]])
  
  # coefficients
  coef_obj <-
    summary_obj$coefficients |> 
    as_tibble(rownames = "info")
  
  # breakpoint information
  psi <-
    summary_obj$psi |> 
    as_tibble(rownames = "psi") |>
    mutate(psi = parse_number(psi)) |> 
    filter(St.Err == max(St.Err))
  
  best_psi <- psi |> pull(psi)
  
  seg_break_est[window] <-
    psi |> pull(Est.)
  
  seg_break_err[window] <-
    psi |> pull(St.Err)
  
  # first segment definitions
  seg_est_1[window]  <- 
    coef_obj |> 
    filter(info == "delta_sst") |> 
    pull(Estimate)
  
  seg_p_1[window]  <-
    coef_obj |> 
    filter(info == "delta_sst") |> 
    pull(`Pr(>|t|)`)
  
  # second segment definitions
  seg_est_2[window]  <- 
    coef_obj |> 
    filter(info == paste0("U",best_psi,".delta_sst")) |> 
    pull(Estimate)
  
  seg_p_2[window]  <-
    coef_obj |> 
    filter(info == paste0("U",best_psi,".delta_sst")) |> 
    pull(`Pr(>|t|)`)
  
  # multiple r-squared
  seg_r[window] <-
    summary_obj$r.squared
  
  # adjusted r-squared
  seg_r_adj[window] <-
    summary_obj$adj.r.squared
  
  # degrees of freedom
  df[window] <-
    summary_obj$df[2]
  
}

# compile model information
model_info <-
  tibble(time_window,
         seg_break_est, # breakpoint estimate
         seg_break_err, # breakpoint error
         seg_est_1, # the slope before the breakpoint
         seg_est_2, # the slope after the breakpoint
         seg_p_1, # the p value before the breakpoint
         seg_p_2, # the p value after the breakpoint
         seg_r, # the r squared value
         seg_r_adj, # the adjusted r squared value
         df) |>  # the degrees of freedom
  
  # apply bonferroni p-value correction
  mutate(seg_p_1 = seg_p_1*length(time_window),
         seg_p_2 = seg_p_2*length(time_window))

# export -----------------------------------------------------------------------
saveRDS(list(fits = fits,
             model_information = model_info),
        "./results/timeseries/model_extinctions.rds") #output




