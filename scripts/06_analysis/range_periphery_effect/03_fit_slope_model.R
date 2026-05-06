# set --------------------------------------------------------------------------
library(tidyverse)
library(corrplot)

# load -------------------------------------------------------------------------
# model fits
model_summaries <-
  readRDS("./results/range_periphery_effect/02_model_summary_tables.rds") #input

# region metrics
region_metrics <-
  read_csv("./data_processed/realms/02_region_metrics.csv", #input
           show_col_types = F)

# wrangle ----------------------------------------------------------------------
# combine model information
model_summaries$lm <-
  model_summaries$lm |> 
  select(-adj_r_sq) |> 
  mutate(model = "lm")

model_info <-
  do.call(rbind.data.frame,
          model_summaries)

# combine with region metrics (predictors)
slope_model_data <-
  model_info |> 
  left_join(region_metrics) |> 
  rename(latitude = y) |> 
  mutate(latitude = abs(latitude)) |> 
  filter(sig_bon == TRUE,
         model == "lm")

# wrangle for slopes to region metrics -----------------------------------------
# check data
slope_model_data_long <-
  slope_model_data |> 
  pivot_longer(cols = c(distance,
                        size,
                        delta_sst,
                        var_sst,
                        latitude),
               names_to = "region_metric")

# check distribution
ggplot(slope_model_data_long) +
  
  geom_histogram(aes(x = (value))) +
  
  facet_grid(cols = vars(region_metric),
             scales = "free_x")

# check correlations
slope_model_data |> 
  select(distance,
         size,
         delta_sst,
         var_sst,
         latitude) |> 
  scale() |> 
  cor() |> 
  corrplot()



ggplot(slope_model_data) +
  
  geom_point(aes(x = log(size), y = log(distance)))

# delta and var temperature are correlated (remove var)
# size and distance are correlated
slope_model_data_corrected_temp <-
  slope_model_data |> 
  mutate(
    
    # combine sst somehow
    combi_sst = delta_sst * var_sst,
    
    # distance / number of cells, aka distance per cell
    dist_over_size = distance / size,
    size_over_dist = size / distance,
    dist_x_size = distance * size
  )

slope_model_data_corrected_temp |> 
  select(dist_over_size,
         size_over_dist,
         dist_x_size,
         delta_sst) |> 
  scale() |> 
  cor() |> 
  corrplot()

# check new variables
hist(slope_model_data_corrected_temp$dist_over_size |> log())
hist(slope_model_data_corrected_temp$size_over_dist |> log())
hist(slope_model_data_corrected_temp$dist_x_size |> log())
hist(slope_model_data_corrected_temp$combi_sst |> log())
hist(slope_model_data_corrected_temp$delta_sst |> log())

# fit model --------------------------------------------------------------------
# fit data
fits <- list()

# distance x size
fits$fit_1 <-
  lm(slope ~
       log(dist_x_size) +
       log(delta_sst),
     data = slope_model_data_corrected_temp)

# distance only
fits$fit_2 <-
  lm(slope ~
       log(distance) +
       log(delta_sst),
     data = slope_model_data_corrected_temp)

# size only
fits$fit_3 <-
  lm(slope ~
       log(size) +
       log(delta_sst),
     data = slope_model_data_corrected_temp)

# distance / size
fits$fit_4 <-
  lm(slope ~
       log(dist_over_size) +
       log(delta_sst),
     data = slope_model_data_corrected_temp)


# size / distance
fits$fit_5 <-
  lm(slope ~
       log(size_over_dist) +
       log(delta_sst),
     data = slope_model_data_corrected_temp)

# size / distance
fits$fit_6 <-
  lm(slope ~
       log(size_over_dist) *
       log(delta_sst),
     data = slope_model_data_corrected_temp)

# find best fit
lapply(fits,
       AIC) |> 
  unlist() |> min()

# check predictors
best_pred_set <-
  step(fits$fit_5)

# best model fit summary
best_summary <-
  summary(best_pred_set)


# export -----------------------------------------------------------------------
saveRDS(best_pred_set,
        "./results/range_periphery_effect/03_periphery_effect_model.rds") #output

