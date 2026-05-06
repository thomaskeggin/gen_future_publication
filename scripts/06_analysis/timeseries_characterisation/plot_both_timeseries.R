# set --------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

colour_scheme <-
  c(`Global extinction` = "#690527",
    `LDG present` = "#D6D6D6",
    `LDG collapse` = "#357F9D")

subset_metrics <-
  c("range_size_mean",
    "occupied_cells",
    "species_richness_global")

plots <- list()

# load -------------------------------------------------------------------------
# simulation time series
simulation_timeseries <-
  readRDS("./results/timeseries_characterisation/simulation_timeseries/02_simulation_timeseries.rds") #input

# tolerance mismatch time series
mismatch_v_time <-
  readRDS("./results/timeseries_characterisation/habitability_timeseries/02_sst_trait_mismatch.rds") #input

# uninhabitability time series
uninhabitable_times <-
  readRDS("./results/timeseries_characterisation/habitability_timeseries/02_extinction_times.rds") #input

# plot simulation subset -------------------------------------------------------
sim_subset <-
  simulation_timeseries |> 
  filter(name %in% subset_metrics) |> 
  
  # set plot order
  mutate(name_clean = factor(name_clean,
                             levels = c("Mean range size",
                                        "Number of\noccupied cells",
                                        "Global species\nrichness")))

# simulation time series
plots$simulation <-
  ggplot() +
  
  geom_line(data = sim_subset |> filter(status == "LDG present"),
            aes(x = year,
                y = value,
                group = run_id,
                colour = status)) +
  
  geom_line(data = sim_subset |> filter(status == "LDG collapse"),
            aes(x = year,
                y = value,
                group = run_id,
                colour = status)) +
  
  geom_line(data = sim_subset |> filter(status == "Global extinction"),
            aes(x = year,
                y = value,
                group = run_id,
                colour = status)) +
  
  labs(y = "",
       x = "",
       colour = "Status") +
  
  facet_grid(rows = vars(name_clean),
             scales = "free_y",
             switch = "y") +
  
  scale_colour_manual(values = colour_scheme) +
  
  theme_bw() +
  
  theme(panel.grid = element_blank()) + 
  theme(panel.spacing=unit(2, "lines"),
        strip.placement.y = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

# plot habitability histogram --------------------------------------------------
plots$habitability <-
  ggplot(uninhabitable_times |> filter(year != 2100)) +
  
  geom_bar(aes(year),
           fill = "#CC6677",
           colour = "black") +
  
  labs(y = "Species ranges\nmade uninhabitable",
       x = "",
       fill = "",
       colour = "") +
  
  theme_classic()

# plot thermal mismatch time series --------------------------------------------
ribbon_alpha <- 0.5

plots$sst_optima <-
  ggplot(mismatch_v_time) +
  
  # mean lines
  geom_line(aes(x = year,
                y = mean_values,
                colour = variable),
            linewidth = 1) +
  
  # ribbons
  geom_ribbon(aes(x = year,
                  ymin = mean_values - sd_values,
                  ymax = mean_values + sd_values,
                  colour = variable,
                  fill = variable),
              linetype = "dashed",
              alpha = 0.25) +
  
  # labels
  labs(y = "Temperature (C)",
       x = "",
       fill = "",
       colour = "") +
  
  # limits
  lims(y = c(25,35)) +
  
  # colours
  scale_fill_manual(values = c("#CC6677","#332288")) +
  scale_colour_manual(values = c("#CC6677","#332288")) +
  
  theme_classic()

# plot parameter space ---------------------------------------------------------
# simplify
parameters <-
  sim_subset |> 
  select(`Dispersal range (km / year)`,
         `Adaptive rate (°C / year)`,
         status) |> 
  distinct()

# plot
plots$parameters <-
  
  ggplot(parameters) +
  
  # points
  geom_point(aes(x = `Dispersal range (km / year)`,
                 y = `Adaptive rate (°C / year)`,
                 fill = status),
             shape = 21,
             colour = alpha("black",0.1)) +
  
  # labels
  labs(fill = "Status") +
  
  # colours
  scale_fill_manual(values = colour_scheme) +
  
  # theme
  theme_classic() +
  theme(legend.position = "none")


# compile ----------------------------------------------------------------------
design <-
  "AADD
   BBDD
  CCDD"

compiled_plot <-
  plots$sst_optima +
  plots$habitability +
  plots$parameters +
  plots$simulation +
  
  plot_layout(guides = "collect",
    axes = "collect",
    design = design) +
  plot_annotation(#title = "Entire original species ranges made uninhabitable per year",
    tag_levels = "a")

# export -----------------------------------------------------------------------
ggsave("./plots/timeseries/extinction_dynamics.png", #output
       compiled_plot,
       height = 90*1.4,
       width = 180*1.4,
       units = "mm",
       dpi = 300)
