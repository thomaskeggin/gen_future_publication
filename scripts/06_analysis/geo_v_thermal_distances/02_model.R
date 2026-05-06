# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
distances <-
  readRDS("./results/geo_v_thermal_distances/01_pairwise_distance_df.rds") #input

# checks -----------------------------------------------------------------------
ggplot(distances) +
  
  geom_histogram(aes(x = geo_lat_distances)) +
  facet_wrap(~timestep)

ggplot(distances) +
  
  geom_histogram(aes(x = thermal_distance)) +
  facet_wrap(~timestep)

# alles guet

# fit models -------------------------------------------------------------------
timesteps <-
  unique(distances$timestep)

fits <-
  list()

p <-
  adj_r <-
  slope <-
  c()

for(t in timesteps){
  
  # subset to timestep
  dist_t <-
    distances |> 
    filter(timestep == t)
  
  # fit model
  fits[[paste0("t_",t)]] <-
    lm(thermal_distance ~ geo_lat_distances,
       data = dist_t)
  
  # extract estimates
  p     <- c(p,     summary(fits[[paste0("t_",t)]])$coefficients[2,"Pr(>|t|)"])
  adj_r <- c(adj_r, summary(fits[[paste0("t_",t)]])$adj.r.squared)
  slope <- c(slope, summary(fits[[paste0("t_",t)]])$coefficients[2,"Estimate"])
  
}

# collate
fit_summary <-
  tibble(timestep = timesteps,
         slope = slope,
         adj_r = adj_r,
         p = p,
         p_bon = p * length(p))

# check min-max r and p values for reporting
range(fit_summary$adj_r) |> round(2)
range(fit_summary$p_bon) |> round(2)
  
  # export -----------------------------------------------------------------------
saveRDS(list(models = fits,
             summary_info = fit_summary),
        "./results/geo_v_thermal_distances/02_models.rds") #output



