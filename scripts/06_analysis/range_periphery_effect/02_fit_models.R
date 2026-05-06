# set --------------------------------------------------------------------------
library(tidyverse)
library(fishtree)
library(nlme)
library(ape)
library(progress)

# load -------------------------------------------------------------------------
# periphery metrics
peri_df <-
  readRDS("./results/range_periphery_effect/01_peripheralness.rds") #input

# region metrics
region_metrics <-
  read_csv("./data_processed/realms/02_region_metrics.csv", #input
           show_col_types = F) |> 
  select(-c(x,y))

# subset phylogeny
phylo <-
  fishtree::fishtree_phylogeny(peri_df$species |> unique())

# wrangle ----------------------------------------------------------------------
# by ecoregion
# modelling by ecoregion to give a control for habitat density. Can't do it by 
# cluster as cluster density will be determined by the dispersal range trait.
peri_eco <-
  peri_df |> 
  rename(region = ecoregion) |> 
  left_join(region_metrics |> filter(scale == "ecoregion"),
            by = "region") |> 
  
  # count number of observations per ecoregion
  group_by(region) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  
  # only keep ecoregions with more than 10 habitable cells and more than 5 
  # observations
  filter(size > 10,
         n > 4)

# fit thermal_mismatch to peripheralness models --------------------------------
# linear regression per ecoregion
ecoregions <-
  peri_eco$region |> unique()

lm_models <-
  lm_summaries <-
  list()

p_values <-
  adj_r_squared <-
  estimates <-
  c()

pb <-
  progress_bar$new(total = length(ecoregions),
                   format = ":current of :total [:bar] :eta")

for(ecoregion in ecoregions){
  
  pb$tick()
  
  # subset data for fitting model
  lm_data <-
    peri_eco |> 
    filter(region == ecoregion)
  
  # fit the model
  lm_models[[ecoregion]] <-
    lm(thermal_fitness ~
         env_peripheralness,
       data = lm_data)
  
  # extract summaries
  lm_summaries[[ecoregion]] <-
    summary(lm_models[[ecoregion]])
  
  # extract summary values
  p_values[ecoregion] <-
    lm_summaries[[ecoregion]]$coefficients["env_peripheralness",
                                           "Pr(>|t|)"]
  
  adj_r_squared[ecoregion] <-
    lm_summaries[[ecoregion]]$adj.r.squared
  
  estimates[ecoregion] <-
    lm_summaries[[ecoregion]]$coefficients["env_peripheralness",
                                           "Estimate"]
  
}

# create summary data frame
ecoregion_summaries <-
  tibble(scale = "ecoregion",
         model = "lm",
         region = ecoregions,
         adj_r_sq = adj_r_squared,
         slope = estimates,
         p = p_values) |> 
  mutate(p_bon = p * length(ecoregions),
         sig_bon = p_bon < 0.05)

# fit thermal_mismatch to peripheralness models pgls ---------------------------
# linear regression per ecoregion
ecoregions <-
  peri_eco$region |> unique()

pgls_models <-
  pgls_summaries <-
  list()

p_values <-
  estimates <-
  c()

pb <-
  progress_bar$new(total = length(ecoregions),
                   format = ":current of :total [:bar] :eta")

for(ecoregion in ecoregions){
  
  pb$tick()
  
  # subset data for fitting model
  pgls_data <-
    peri_eco |> 
    filter(region == ecoregion)
  
  phylo_sub <-
    keep.tip(phylo,unique(pgls_data$species))
  
  # fit the model
  pgls_models[[ecoregion]] <-
    gls(thermal_fitness ~
          env_peripheralness,
        correlation = corBrownian(phy = phylo_sub,
                                  form = ~species|run_id),
        data = pgls_data)
  
  # extract summaries
  pgls_summaries[[ecoregion]] <-
    summary(pgls_models[[ecoregion]])
  
  # extract summary values
  p_values[ecoregion] <-
    pgls_summaries[[ecoregion]]$tTable["env_peripheralness","p-value"]
  
  estimates[ecoregion] <-
    pgls_summaries[[ecoregion]]$coefficients["env_peripheralness"]
  
}

# create summary data frame
ecoregion_pgls_summaries <-
  tibble(scale = "ecoregion",
         model = "pgls",
         region = ecoregions,
         slope = estimates,
         p = p_values) |> 
  mutate(p_bon = p * length(ecoregions),
         sig_bon = p_bon < 0.05)

# compile models fits and summaries --------------------------------------------
model_information <-
  list(fits = list(lm = lm_models,
                   pgls = pgls_models),
       summaries = list(lm = lm_summaries,
                        pgls = pgls_summaries))

model_summary_tables <-
  list(lm = ecoregion_summaries,
       pgls = ecoregion_pgls_summaries)

# export -----------------------------------------------------------------------
saveRDS(model_information,
        "./results/range_periphery_effect/02_model_information.rds") #output

saveRDS(model_summary_tables,
        "./results/range_periphery_effect/02_model_summary_tables.rds") #output
