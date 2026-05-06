# set --------------------------------------------------------------------------
library(tidyverse)
library(data.table)
library(progress)
library(patchwork)

# load -------------------------------------------------------------------------
predictor_df <-
  read_csv("./data_processed/species/species_start_metrics.csv", #input
           show_col_types = FALSE)

response_df <-
  fread("./results/04_metrics_species.csv") #input

taxonomy <-
  read_csv("./data_processed/species/taxonomy_fishtree.csv", #input
           show_col_types = FALSE)

# wrangle ----------------------------------------------------------------------
# numeric predictors
numeric_predictors <-
  predictor_df |> 
  select(-c(species,contains("realm"))) |> 
  colnames()

# common manipulation
results <- 
  response_df |> 
  as_tibble() |> 
  
  # aggregate to a single point per species
  group_by(timestep,species) |> 
  reframe(across(abundance_total:betweenness_max,
                 \(.x) mean(.x,na.rm = TRUE)),
          sims_survived = n()/500) |> 
  filter(timestep == 0) |> 
  left_join(predictor_df) |> 
  left_join(taxonomy)

# data frame for numeric predictors
results_num <-
  results |> 
  select(-c(timestep,species,
            contains("realm"))) |> 
  pivot_longer(cols = contains("i_"),
               names_to = "i_metric",
               values_to = "i_metric_values") |> 
  pivot_longer(cols = abundance_total:sims_survived,
               names_to = "r_metric",
               values_to = "r_metric_values")

# data frame for categorical predictors
results_cat <-
  results |> 
  select(-c(any_of(numeric_predictors))) |> 
  pivot_longer(cols = c(i_realm_dominant,class:genus),
               names_to = "i_metric",
               values_to = "i_metric_values") |>  
  pivot_longer(cols = abundance_total:sims_survived,
               names_to = "r_metric",
               values_to = "r_metric_values")


# plot numeric -----------------------------------------------------------------
responses <-
  unique(results_num$r_metric)

pb <-
  progress_bar$new(total = length(responses),
                   format = ":current of :total")

for(response in responses){
  
  pb$tick()
  
  # wrangle
  plot_data <-
    results_num |> 
    filter(r_metric == response)
  
  # plot
  plot_me <-
    ggplot(plot_data) +
    geom_point(aes(x = i_metric_values,
                   y = r_metric_values),
               alpha = 0.5,
               colour = "#010440",
               size = 0.5) +
    
    facet_wrap(~i_metric,
               scales = "free_x") +
    xlab("") +
    ylab(response) +
    theme_bw()
  
  # export
  ggsave(paste0("./plots/species_metrics/numeric/", #output
                response,".jpg"),
         plot_me,
         height = 7,
         width = 14,
         dpi = 300)
  
}

# plot categorical -------------------------------------------------------------

pb <-
  progress_bar$new(total = length(responses),
                   format = ":current of :total")

for(response in responses){
  
  pb$tick()
  
  # wrangle
  plot_data <-
    results_cat |> 
    filter(r_metric == response)
  
  # plot
  plot_me <-
    ggplot(plot_data) +
    geom_boxplot(aes(x = str_wrap(i_metric_values,10),
                     y = r_metric_values),
                 fill = "#6360BF",
                 colour = "#010440") +
    
    facet_wrap(~i_metric,
               scales = "free_x") +
    
    xlab("") +
    ylab(response) +
    theme_bw()
  
  # export
  ggsave(paste0("./plots/species_metrics/categorical/", #output
                response,".jpg"),
         plot_me,
         height = 7,
         width = 14,
         dpi = 300)
  
}












