# set --------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(igraph)
library(ggraph)

# plot gradient function
# https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient
make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

g <- make_gradient(
  deg = 45,
  n = 500,
  cols = c("#E8E8E8","white","#E8E8E8")
)


# generate data ----------------------------------------------------------------
pop_n <- 4

# trait_start_mean <- 26.5
# sst_trait_sd     <- 0.15
# 
# adaptive_power <- 0.5
# dispersal_power <- 0.5
# 
# sst_change <- 2

temp_lower <- 26.2
temp_upper <- 26.5

adaptive_power <- 0.75
dispersal_power <- 0.75

sst_change <- 1.5

# null model (no dispersal, no adaptation)
null_process <-
  tibble(pop_id = 1:pop_n,
         trait_start = seq(from = temp_lower,
                           to = temp_upper,
                           by = ((temp_upper - temp_lower)/(pop_n - 1))),
         trait_null = trait_start,
         sst_start = trait_start[c(3,2,4,1)],
         sst_end = sst_start + sst_change)

# dispersal only model
dispersal <-
  null_process |> 
  mutate(trait_dispersal = trait_start,
         sst_dispersal = sst_end - dispersal_power)

# adaptation only model
adaptation <-
  dispersal |> 
  mutate(trait_adaptation = ifelse(trait_start + adaptive_power > sst_end,
                                   sst_end,
                                   trait_start + adaptive_power))

# adaptation with gene flow
gene_flow <-
  adaptation |> 
  mutate(trait_adaptive_mean = mean(trait_adaptation),
         trait_gene_flow = trait_adaptation + 0.5*(trait_adaptive_mean - trait_adaptation),
         
         # gene flow good or bad
         gene_flow_consequence =
           ifelse(abs(trait_gene_flow - sst_dispersal) < # difference between gene flow traits and SST
                    abs(trait_adaptation - sst_dispersal), # difference between adaptation only and SST
                  "Increases suitability",
                  "Decreases suitability")#,
         
         # gene_flow_consequence =
         #   ifelse(abs(trait_gene_flow - sst_dispersal) > # difference between gene flow traits and SST
         #            abs(trait_null - sst_dispersal), # difference between null traits and SST
         #          "Causes maladaptation",
         #          gene_flow_consequence)
  )

# wrangle gene flow ------------------------------------------------------------
# create underlying meta population graph
# create nodes
graph_layout <-
  list(start = gene_flow |>
         select(sst_start,trait_start) |> 
         mutate(step = "start"),
       
       null = gene_flow |>
         select(sst_end,trait_null)|> 
         mutate(step = "null"),
       
       disp = adaptation |> 
         select(sst_dispersal,trait_dispersal) |> 
         mutate(step = "disp"),
       
       ad = adaptation |> 
         select(sst_end,trait_adaptation) |> 
         mutate(step = "ad"),
       
       ad_disp = gene_flow |>
         select(sst_dispersal,trait_adaptation)|> 
         mutate(step = "ad_disp"))

colnames(graph_layout$start)[c(1,2)]   <- c("sst","trait")
colnames(graph_layout$null)[c(1,2)]    <- c("sst","trait")
colnames(graph_layout$disp)[c(1,2)]    <- c("sst","trait")
colnames(graph_layout$ad)[c(1,2)]      <- c("sst","trait")
colnames(graph_layout$ad_disp)[c(1,2)] <- c("sst","trait")

graph_layout <- 
  do.call(rbind.data.frame,graph_layout) |> 
  mutate(pop_id_1 = 1:20,
         pop_id_2 = pop_id_1,
         step_2 = step) |> 
  
  mutate(x = sst,
         y = trait,
         .before = everything())

# create edges
graph_edges <-
  expand.grid(graph_layout$pop_id_1,graph_layout$pop_id_1) |> 
  filter(Var1 != Var2) |> 
  rename(pop_id_1 = Var1,
         pop_id_2 = Var2) |> 
  left_join(graph_layout |> select(pop_id_1,step)) |> 
  left_join(graph_layout |> select(pop_id_2,step_2)) |> 
  filter(step == step_2) |> 
  select(contains("pop_id")) |> 
  graph_from_data_frame()

# common plot elements ---------------------------------------------------------
point_size <- 3
point_shape <- 21
start_alpha <- 0.5
null_fill <- "white"
limits <- c(26,28)

custom_theme <-
  theme_minimal() +
  theme(text = element_text(family = "serif",
                            size = 10),
        axis.text = element_blank())

flow_colours <-
  c("Causes maladaptation" = "#4D262D",
    "Decreases suitability" = "#CC6677",
    "No gene flow" = "white",
    "Increases suitability" = "#88CCEE")

# start plot -------------------------------------------------------------------
nice_plot <-
  
  ggraph(graph_edges,
         layout = graph_layout) +
  
  # background
  annotation_custom(grob = g,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf) +
  
  # perfect suitability
  geom_abline(slope = 1,
              alpha = 0.5,
              linetype = "dashed") +
  
  # adaptation and gene flow ---------------------------------------------------
# metapopulation lines
geom_edge_link(alpha = 0.1) +
  
  # gene flow lines
  geom_segment(data = gene_flow,
               aes(x = sst_dispersal,
                   y = trait_adaptation,
                   yend = trait_gene_flow),
               alpha = 0.6) +
  
  # adaptation and dispersal
  geom_point(data = gene_flow,
             aes(x = sst_dispersal,
                 y = trait_adaptation),
             fill = "grey",
             size = point_size,
             shape = point_shape) +
  
  # gene flow points
  geom_point(data = gene_flow,
             aes(x = sst_dispersal,
                 y = trait_gene_flow,
                 fill = gene_flow_consequence),
             size = point_size,
             shape = 22) +
  
  # no response ----------------------------------------------------------------
# start
geom_point(data = null_process,
           aes(x = sst_start,
               y = trait_start),
           fill = null_fill,
           size = point_size,
           shape = point_shape) +
  
  # end
  geom_point(data = null_process,
             aes(x = sst_end,
                 y = trait_null),
             fill = null_fill,
             size = point_size,
             shape = 21) +
  
  # dispersal only -------------------------------------------------------------
# end
geom_point(data = dispersal,
           aes(x = sst_dispersal,
               y = trait_dispersal),
           fill = "grey",
           size = point_size,
           shape = 22) +
  
  # adaptation only ------------------------------------------------------------
# adaptation
geom_point(data = adaptation,
           aes(x = sst_end,
               y = trait_adaptation),
           fill = "grey",
           size = point_size,
           shape = 22) +
  
  # layout ---------------------------------------------------------------------

# colours
scale_fill_manual(values = flow_colours) +
  
  # layout
  lims(x = limits,
       y = limits) +
  labs(title = "",
       y = "Thermal phenotype",
       x = "Environmental temperature",
       fill = str_wrap("Consequence of gene flow on environmental suitability",15)) +
  
  coord_fixed() +
  
  custom_theme 

# export -----------------------------------------------------------------------
scalar <- 1.5

h <- 90 * scalar
w <- 90 * scalar

ggsave("./plots/niche_tracking_plot.jpg", #output
       nice_plot,
       height = h,
       width = w,
       units = "mm",
       dpi = 300)
