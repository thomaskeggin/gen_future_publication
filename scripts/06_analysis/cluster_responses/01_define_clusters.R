# assign simulations into groups based on the unsupervised clustering of their
# response values.
# set --------------------------------------------------------------------------
set.seed(1989)

library(tidyverse)
library(gen3sisExtra)
library(factoextra)
library(cluster)
library(patchwork)

target_metrics <-
  c("range_size_mean",
    "species_richness_global",
    "species_richness_cell_mean",
    "fragmentation_mean",
    "abundance_cell_mean",
    "occupied_cells",
    ##"betweenness_mean",
    #"cluster_count_mean",
    #"cluster_size_mean_mean",
    #"cohesion_mean",
    "inter_cluster_distance_mean")

# load -------------------------------------------------------------------------
# parameters
params <-
  readRDS("./results/categorised_parameters.rds") #input

# sim metrics
sim_metrics <-
  read_csv("./results/04_metrics_simulation/04_metrics_simulation_0.csv", #input
           show_col_types = F)

# wrangle ----------------------------------------------------------------------
# subset metrics of interest
sim_metrics_subset <-
  sim_metrics |> 
  select(run_id,
         all_of(target_metrics))

# check variable distributions
hist_data <-
  sim_metrics_subset |> 
  pivot_longer(cols = all_of(target_metrics))

ggplot(hist_data) +
  
  geom_histogram(aes(x = value)) +
  
  facet_wrap(~name,
             ncol = 1,
             scales = "free")

log_variables <-
  c(#"betweenness_mean",
    #"cluster_count_mean",
    ##"cluster_size_mean_mean",
    "fragmentation_mean",
    "range_size_mean",
    "species_richness_cell_mean")


# transform variables and re-check
sim_metrics_subset_trans <-
  sim_metrics_subset |> 
  mutate(extinctions_global = log(1427 - species_richness_global)) |> 
  
  mutate(across(all_of(log_variables),
                log))

hist_data <-
  sim_metrics_subset_trans |> 
  pivot_longer(cols = all_of(c(target_metrics,"extinctions_global")))

ggplot(hist_data) +
  
  geom_histogram(aes(x = value)) +
  
  facet_wrap(~name,
             ncol = 1,
             scales = "free")

# check correlations
sim_metrics_subset_trans |> 
  select(-run_id) |> 
  plot()

# turn into data frame and matrix
subset_df <-
  sim_metrics_subset_trans |> 
  select(all_of(target_metrics)) |> 
  as.data.frame()

row.names(subset_df) <-
  sim_metrics_subset_trans$run_id

subset_mat <-
  subset_df |> 
  na.omit() |> 
  filter(!if_any(everything(), is.infinite)) |> 
  as.matrix()

# scale variables
subset_mat_scaled <-
  subset_mat |> scale()

# PCA --------------------------------------------------------------------------
pca <-
  prcomp(subset_mat_scaled)

pca_summary <-
  summary(pca)

pca_var <-
  round(pca_summary$importance["Proportion of Variance",]*100,0)

pca_results <-
  pca$x |> 
  as_tibble(rownames = "run_id") |> 
  mutate(run_id = as.numeric(run_id)) |> 
  right_join(params)


# hclust -----------------------------------------------------------------------
subset_dist <-
  dist(subset_mat_scaled)

subset_hclust <-
  hclust(subset_dist)

# looks like ~ 8 clusters

# kmeans -----------------------------------------------------------------------
# find best fit k
fviz_nbclust(subset_mat_scaled,
             kmeans,
             "silhouette")

# run kmeans
subset_kmeans <-
  kmeans(subset_mat_scaled,
         5)

# assign kmeans clusters names
kmeans_cluster_key <-
  tibble(kmeans_cluster = c(1:6),
         kmeans_cluster_name = factor(c("Adaptation\nlimitation",
                                        "Gene flow -\nadaptation interplay",
                                        "Species expansion",
                                        "Species collapse",
                                        "Dispersal limitation",
                                        "Global extinction"),
                                      levels = c("Species expansion",
                                                 "Gene flow -\nadaptation interplay",
                                                 "Dispersal limitation",
                                                 "Adaptation\nlimitation",
                                                 "Species collapse",
                                                 "Global extinction")),
         kmeans_colour = c("#AA4499",
                           "#44AA99",
                           "#88CCEE",
                           "#CC6677",
                           "#332288",
                           "black"))


subset_kmeans_cluster_ids <-
  tibble(run_id = names(subset_kmeans$cluster) |> as.numeric(),
         kmeans_cluster = subset_kmeans$cluster)

kmeans_colours <-
  kmeans_cluster_key$kmeans_colour

names(kmeans_colours) <-
  kmeans_cluster_key$kmeans_cluster_name

# compile results --------------------------------------------------------------
# compile
results <-
  pca$x |> 
  as_tibble(rownames = "run_id") |> 
  mutate(run_id = as.numeric(run_id)) |> 
  right_join(params) |> 
  left_join(subset_kmeans_cluster_ids) |> 
  left_join(sim_metrics) |> 
  
  # create a group 6 for global extinctions
  mutate(kmeans_cluster = ifelse(is.na(kmeans_cluster),
                                 6,
                                 kmeans_cluster)) |> 
  
  left_join(kmeans_cluster_key)

results_all <-
  list(results = results,
       pca_variance = pca_var,
       kmeans_key = kmeans_cluster_key)

# export -----------------------------------------------------------------------
saveRDS(results_all,
        "./results/cluster_responses/01_cluster_responses.rds") #output







