# set --------------------------------------------------------------------------
library(tidyverse)

# load -------------------------------------------------------------------------
# peak summaries
peak_gaussian_summaries <-
  read_csv("./results/latitudinal_diversity_gradient/peak_gaussian_summaries.csv", #input
           show_col_types = F)

# load in the parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# simulation metrics
sim_metrics <-
  read_csv("./results/04_metrics_simulation/04_metrics_simulation_0.csv", #input
           show_col_types = F)

# unsupervised clusters
kmeans_clusters <-
  readRDS("./results/kmeans_response_clusters.rds") #input

# wrangle ----------------------------------------------------------------------
# add hemispheres to parameters and kmeans
paramameans <-
  parameters |> 
  left_join(kmeans_clusters) |> 
  select(run_id,
         `Dispersal range (km)`,
         `Adaptive rate (°C)`,
         kmeans_cluster_name)

paramameans_hemisphere <-
  bind_rows(paramameans |> mutate(hemisphere = -1),
            paramameans |> mutate(hemisphere = 1))



results <-
  
  # widen gaussian summaries to allow joining
  peak_gaussian_summaries |> 
  select(run_id,hemisphere,parameter,sig_bon) |> 
  pivot_wider(names_from = parameter,
              values_from = sig_bon) |> 
  
  # join kmeans clusters and parameters
  right_join(paramameans_hemisphere) |>
  
  # lengthen for plotting format
  pivot_longer(cols = contains("lat_peak"),
               names_to = "parameter",
               values_to = "sig_bon") |> 
  
  # define outcomes
  mutate(
    
    # split between significant an non-significant
    outcome = ifelse(sig_bon == TRUE,
                     "Significant","Non-significant"),
    
    # gaussian significant, but rejected in favour of uniform
    outcome = ifelse(is.na(sig_bon),
                     "Gaussian response\ncurve rejected",
                     outcome),
    
    # global extinction
    outcome = ifelse(kmeans_cluster_name == "Global extinction",
                     "Global extinction",
                     outcome),
    
    # prettify hemispheres
    hemisphere = ifelse(hemisphere == 1,
                        "Northern\nhemisphere",
                        "Southern\nhemisphere"),
    
    hemisphere = factor(hemisphere)
    
  ) |> 
  
  # tidy up
  select(run_id,
         hemisphere,
         kmeans_cluster_name,
         parameter,
         outcome) |>
  
  # summarise
  group_by(hemisphere,
           kmeans_cluster_name,
           parameter) |> 
  
  mutate(cluster_n = n()) |> 
  
  ungroup() |> 
  
  group_by(hemisphere,
           kmeans_cluster_name,
           parameter,
           outcome) |> 
  
  reframe(n = n(),
          cluster_n = mean(cluster_n)) |> 
  
  mutate(pro_n = n / cluster_n)


# plot -------------------------------------------------------------------------
# wrangle
results_plot <-
  results |> 
  
  mutate(outcome = 
           factor(outcome,
                  levels = c("Significant",
                             "Non-significant",
                             "Gaussian response\ncurve rejected",
                             "Global extinction") |> rev()),
         
         parameter = gsub("lat_peak_latitude",
                          "Latitude of\npeak richness",
                          parameter),
         parameter = gsub("lat_peak_richness",
                          "Species richness\nof latitudinal peak",
                          parameter),
         parameter = gsub("lat_peak_sd",
                          "Standard deviation of\nlatitudinal response curve",
                          parameter),
         
         kmeans_cluster_name =
           factor(kmeans_cluster_name,
                  levels = rev(levels(results$kmeans_cluster_name)))
  )

# plot
gauss_fit_plot <-
  ggplot(results_plot) +
  
  geom_col(aes(y = kmeans_cluster_name,
               x = pro_n,
               fill = outcome),
           colour = "black",
           width = 0.4,
           linewidth = 0.25,
           alpha = 1) +
  
  
  facet_grid(cols = vars(parameter),
             rows = vars(hemisphere),
             scales = "free") +
  
  labs(fill = "Gaussian response\nestimate outcome",
       x = "Proportion of simulations in category",
       y = "") +
  
  scale_fill_manual(values = c("#44AA99",
                               "#CC6677",
                               "lightgrey",
                               "white") |> rev()) +
  
  guides(fill = guide_legend(reverse=T)) +
  
  theme_classic() +
  theme(plot.background = element_rect(fill = "white",
                                       colour = "transparent"),
        panel.background = element_rect(fill = "white",
                                        colour = alpha("black",0.5)),
        strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(face = "bold", family = "serif"),
        axis.text.x = element_blank())

# export -----------------------------------------------------------------------
ggsave("./plots/latitudinal_diversity_gradient/parameters_v_ldg_fitting.png", #output,
       gauss_fit_plot,
       height = 6,
       width = 10)


