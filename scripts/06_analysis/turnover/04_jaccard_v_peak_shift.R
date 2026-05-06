# set --------------------------------------------------------------------------
library(tidyverse)

# load and wrangle -------------------------------------------------------------
# initial LDG
initial <-
  readRDS("./results/latitudinal_diversity_gradient/initial_ldg.rds") #input

# parameters
parameters <-
  readRDS("./results/categorised_parameters.rds") #input

# jaccard
jaccard <-
  readRDS("./results/turnover/01_turnover_df.rds") |> #input
  left_join(parameters, by = "run_id") |> 
  group_by(category_long,
           run_id,
           dispersal_range,
           adaptive_rate) |> 
  reframe(jaccard = mean(jaccard))

# LDG metrics
ldg_metrics <-
  read_csv("./results/latitudinal_diversity_gradient/peak_gaussian_summaries.csv", #input
           show_col_types = F) |> 
  left_join(jaccard,
            by = "run_id") |> 
  filter(sig_bon == TRUE) |> 
  mutate(hemisphere = ifelse(hemisphere == 1,
                             "Northern\nhemisphere",
                             "Southern\nhemisphere")) |> 
  na.omit() |> 
  left_join(initial, by = "hemisphere")

# latitude plot data
lat_plot_data <-
  ldg_metrics |>
  filter(parameter == "lat_peak_latitude") |> 
  mutate(delta_lat = Estimate - lat_peak_latitude_start)

# sd plot data
sd_plot_data <-
  ldg_metrics |>
  filter(parameter == "lat_peak_sd",
         Estimate > 0) |> 
  mutate(delta_sd = Estimate - lat_peak_sd_start)

# basic bitch linear regressions -----------------------------------------------
results <-
  list(lat = list(),
       sd = list())

# latitude
# north
results$lat$north <-
  lm(Estimate ~ jaccard,
     data = lat_plot_data |> filter(hemisphere == "Northern\nhemisphere")) |> 
  summary()

# south
results$lat$south <-
  lm(Estimate ~ jaccard,
     data = lat_plot_data |> filter(hemisphere == "Southern\nhemisphere")) |> 
  summary()

# sd
# north
results$sd$north <-
  lm(Estimate ~ jaccard,
     data = sd_plot_data |> filter(hemisphere == "Northern\nhemisphere")) |> 
  summary()

# south
results$sd$south <-
  lm(Estimate ~ jaccard,
     data = sd_plot_data |> filter(hemisphere == "Southern\nhemisphere")) |> 
  summary()

# summaries summaries
summaries <-
  tibble(metric = c("latitude",
                    "latitude",
                    "sd",
                    "sd"),
         
         hemisphere = c("Northern\nhemisphere",
                        "Southern\nhemisphere",
                        "Northern\nhemisphere",
                        "Southern\nhemisphere"),
         
         y_int = c(results$lat$north$coefficients["(Intercept)","Estimate"],
                   results$lat$south$coefficients["(Intercept)","Estimate"],
                   results$sd$north$coefficients["(Intercept)","Estimate"],
                   results$sd$south$coefficients["(Intercept)","Estimate"]),
         
         slope =  c(results$lat$north$coefficients["jaccard","Estimate"],
                    results$lat$south$coefficients["jaccard","Estimate"],
                    results$sd$north$coefficients["jaccard","Estimate"],
                    results$sd$south$coefficients["jaccard","Estimate"]),
         
         adj.r.squared = c(results$lat$north$adj.r.squared,
                           results$lat$south$adj.r.squared,
                           results$sd$north$adj.r.squared,
                           results$sd$south$adj.r.squared),
         
         p = c(results$lat$north$coefficients[2,"Pr(>|t|)"],
               results$lat$south$coefficients[2,"Pr(>|t|)"],
               results$sd$north$coefficients[2,"Pr(>|t|)"],
               results$sd$south$coefficients[2,"Pr(>|t|)"]))

# plot -------------------------------------------------------------------------
# latitude
plot_lat <-
  ggplot(lat_plot_data) +
  
  geom_abline(data = summaries |> filter(metric == "latitude"),
              aes(intercept = y_int,
                  slope = slope),
              colour = alpha("black", 0.5)) +
  
  geom_point(aes(x = jaccard,
                 y = Estimate),
             colour = alpha("black", 0.75)) +
  
  facet_grid(rows = vars(hemisphere),
             scales = "free") +
  
  labs(y = "Latitude of species richness peak\n(degrees latitude)",
       x = "Jaccard similarity of species composition, 2013 vs 2100") +
  
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  
  theme_bw()

# width
plot_sd <-
  ggplot(sd_plot_data) +
  
  geom_abline(data = summaries |> filter(metric == "sd"),
              aes(intercept = y_int,
                  slope = slope),
              colour = alpha("black", 0.5)) +
  
  geom_point(aes(x = jaccard,
                 y = Estimate),
             colour = alpha("black", 0.75)) +
  
  facet_grid(rows = vars(hemisphere),
             scales = "free") +
  
  
  labs(y = "Standard deviation of species richness peak\n(degrees latitude)",
       x = "Jaccard similarity of species composition, 2013 vs 2100") +
  
  theme_bw()

# export -----------------------------------------------------------------------
# model summaries
saveRDS(summaries,
        "./results/turnover/04_jac_v_ldg_model_summaries.rds") #output

# plots
ggsave("./plots/turnover/04_jac_v_ldg_lat.png", #output
       plot_lat,
       height = 4,
       width = 6)


ggsave("./plots/turnover/04_jac_v_ldg_sd.png", #output
       plot_sd,
       height = 4,
       width = 6)




