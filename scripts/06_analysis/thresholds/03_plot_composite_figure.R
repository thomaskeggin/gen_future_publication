# set --------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# load -------------------------------------------------------------------------
plots <-
  readRDS("./plots/thresholds/02_dispersal_inflexion.rds") #output

maps <-
  readRDS("./plots/thresholds/02_dispersal_maps.rds") #output

# wrangle ----------------------------------------------------------------------
wrap_chrs <- 30

map_1 <-
  maps[[1]] +
  labs(title = str_wrap("111.5 km: orthogonally adjacent",wrap_chrs))

map_2 <-
  maps[[2]] +
  labs(title = str_wrap("157.5 km: diagonally adjacent",wrap_chrs))

map_3 <-
  maps[[3]] +
  labs(title = str_wrap("222.5 km: second adjacent",wrap_chrs))

map_4 <-
  maps[[4]] +
  labs(title = str_wrap("268.5 km: “knight” adjacent",wrap_chrs))

# create composite plots -------------------------------------------------------
# layout 
custom_layout <-
  c("AAABB
    AAABB
    CCCBB
    CCCDD
    CCCDD
    CCCDD
    CCCDD
    CCCDD")

# remove x axis from auc plot
auc_plot <-
  plots$auc +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# extract first four maps and lay them out
sub_maps <-
  (map_1 + map_2) /
  (map_3 + map_4)

# composite plot
composite_plot <-
  plots$histogram +
  auc_plot +
  sub_maps +
  plots$response +
  
  plot_annotation(tag_levels = list(c("a",
                                      "b",
                                      "c",
                                      "",
                                      "",
                                      "",
                                      "d")),
  ) +
  
  plot_layout(design = custom_layout,
              axes = "collect")

# export -----------------------------------------------------------------------
scalar <- 1.25

ggsave("./plots/thresholds/dispersal_thresholds.png", #output
       composite_plot,
       height = 180 * scalar,
       width  = 300 * scalar,
       units = "mm",
       dpi = 300)




