---

editor_options: 
  markdown: 
    wrap: 72
---

# Prepare Species

Thomas Keggin 2024-07-17

# Set

``` r
library(tidyverse)
library(terra)
library(tidyterra)
library(gen3sis)
library(fishtree)
library(rfishbase)

# fish range loading function
source("./scripts/functions/load_species_ranges.R")

# where to store the species information
species_info <-
  list()
```

# Load

## Seascapes

``` r
# seascapes
annual_mean_sst <-
  readRDS("./data_processed/seascapes/landscapes.rds")$sst_mean

# sdm reef grid
env_grid <-
  rast("./data/Thomas Keggin Mechanistic Climate Change Project/environmental-grid/global_mask_v2.nc")

# coastline polygons
coasts <-
  vect("./data/coastlines/gshhg-shp-2.3.7/GSHHS_shp/l/GSHHS_l_L1.shp")

# bioregions
spalding <-
  vect("./data/ecoregions/Marine_Ecoregions_Of_the_World_(MEOW)-shp/Marine_Ecoregions_Of_the_World__MEOW_.shp")
```

## Fish ranges

``` r
# sdm outputs
sdm_output <-
  readRDS("./data/Thomas Keggin Mechanistic Climate Change Project/sdm-run-june2021/final-outputs/wide-matrix/SSP126_dispersal-limitation_novel-sst/1981_2015.RDS")
```

# Quality control SDM

## Check reef association using fish base.

``` r
# extract and reformat species names for fish base query
species_names <-
  colnames(sdm_output)[-1]

species_names <- 
  gsub("_"," ",species_names)

# query habitat association
hab_association <-
  species(species_names) |>
  select(Species,
         DemersPelag)
```

```         
## Joining with `by = join_by(SpecCode)`
```

``` r
table(hab_association$DemersPelag)
```

```         
## 
##   bathydemersal    bathypelagic   benthopelagic        demersal         pelagic 
##               1               1              68             257               1 
## pelagic-neritic pelagic-oceanic reef-associated 
##              46              16            1862
```

Remove non-reef fishes and species not in fishbase.

``` r
# filter for reef fishes
reef_species <-
  hab_association |>
  filter(DemersPelag == "reef-associated")

reef_species <-
  data.frame(species=gsub(" ","_",reef_species$Species))

sdm_output_reef <-
  sdm_output |>
  select(c("cell",reef_species$species))
```

## Remove SDM errors

Load in manual range checks:

``` r
sdm_correction <-
  readRDS("./data/Thomas Keggin Mechanistic Climate Change Project/realm_qc/QC_non_strict_reduced.rds") |> 
  rename(species = name)
```

Then apply the corrections

``` r
# coordinates
env_grid_df <-
  as.data.frame(env_grid,
                xy=T) |> 
  as_tibble(rownames = "cell") |> 
  mutate(cell = as.numeric(cell)) |> 
  select(-layer)

# pivot sdm output to a long format
sdm_output_reef_long <-
  
  sdm_output_reef |> 
  pivot_longer(cols = all_of(reef_species$species),
               names_to = "species",
               values_to = "suitability") |> 
  filter(!is.na(suitability)) |> 
  left_join(env_grid_df, by = "cell")
  
  # add coordinates

# remove uncorrected sdm outputs
sdm_output_qc <-
  sdm_output_reef_long |> 
  
  right_join(sdm_correction,
            by = c("cell","species"))
```

And check the corrections

``` r
plot_me <-
  rbind.data.frame(
    mutate(sdm_output_reef_long,stage = "before"),
    
    mutate(sdm_output_qc,stage = "after")
  ) |> 
  
  filter(species %in% c("Abudefduf_declivifrons",
                        "Acanthurus_nigricans")) |> 
  mutate(stage = factor(stage,levels = c("before","after")))


ggplot(plot_me) +
  
  geom_spatvector(data = coasts,
                  alpha = 0.3,
                  colour = "transparent") +
  
  geom_tile(data = plot_me,
            aes(x=x,
                y=y,
                fill = suitability)) +
  
  xlim(c(min(plot_me$x)-10,
         max(plot_me$x)+10)) +
  ylim(c(min(plot_me$y)-10,
         max(plot_me$y)+10)) +
  
  xlab("") +
  ylab("") +
  
  facet_grid(rows = vars(species),
             cols = vars(stage)) +
  
  theme_minimal() +
  theme(legend.position = "none")
```

![](01_prepare_species_files/figure-gfm/check%20sdm%20corrections-1.png)<!-- -->

# Determine species start ranges

## Create a reef mask

``` r
# cell_id column so we don't rely on rownames
annual_mean_sst$cell_id <-
  rownames(annual_mean_sst)

# create mask
sea_mask_temp <-
  annual_mean_sst |>
  
  # remove non-reef cells
  filter(!is.na(y_2100)) |>
  
  # simplify
  dplyr::select(x,y,cell_id) |>
  
  # composite coordinates for joining
  mutate(xy = paste0(x,"_",y))

# list of cells containing reef
reef_cells <-
  pull(sea_mask_temp,
       cell_id)
```

## Aggregate SDM outputs to 1 degree

``` r
# wrangle species data - aggregate to 1 degree resolution
sp_range_1 <-
  sdm_output_qc |>
  
  # round to nearest 0.5 (there are no integer coordinates)
  mutate(x = as.numeric(gsub("\\..*",".5",as.character(x))),
         y = as.numeric(gsub("\\..*",".5",as.character(y)))) |>
  
  # create composite coordinates for joining
  mutate(xy = paste0(x,"_",y)) |>
  
  # remove redundant x, y, and cell columns (the cell is for .25 res and no longer valid)
  select(-c(cell,x,y)) |>
  
  # find mean suitability for new cells
  group_by(species,xy) |>
  reframe(suitability = mean(suitability)) |>
  
  # right join with the mask to remove cells not the inhabitable seascape 
  # keeps entire seascape including where no species are present
  left_join(sea_mask_temp,
            by = "xy") |>
  
  # remove xy variable
  select(-c(xy)) |>
  
  # remove non-reef cells
  filter(cell_id %in% reef_cells)
```

## Have a look

``` r
plot_me <-
  filter(sp_range_1,
         species %in% unique(sp_range_1$species)[1])

buffer <- 3

ggplot() +
  
  # suitability and coastlines
  geom_spatvector(data = coasts,
                  fill = "#FFF7ED") +
  geom_tile(data = plot_me,
            aes(x=x,y=y,
                fill = suitability)) +
  
  # bounds
  xlim(c(min(plot_me$x)-buffer,
         max(plot_me$x)+buffer)) +
  
  ylim(c(min(plot_me$y-buffer),
         max(plot_me$y+buffer))) +
  
  # theme
  ggtitle(unique(sp_range_1$species)[1]) +
  scale_fill_viridis_c() +
  theme_minimal()
```

![](01_prepare_species_files/figure-gfm/check%20species%20range-1.png)<!-- -->

## Extract cell IDs for each species

``` r
for(sp in unique(sp_range_1$species)){
  
  species_info[[sp]]$cells <-
    sp_range_1 |>
    filter(species == sp) |>
    pull(cell_id)
  
}
```

## Extract abundances for each species

``` r
for(sp in unique(sp_range_1$species)){
  
  species_info[[sp]]$abundance <-
    sp_range_1 |>
    filter(species == sp) |>
    pull(suitability)
  
  names(species_info[[sp]]$abundance) <-
    species_info[[sp]]$cells
}
```

# Remove cells with suitability/abundance below the per species median.

``` r
for(sp in names(species_info)){
  
  species_median <-
    median(species_info[[sp]]$abundance)
  
  # identify low suitability cells
  high_prob_cells <-
    names(which(species_info[[sp]]$abundance >= species_median))
  
  # remove low suitability cells
  species_info[[sp]]$abundance <-
    species_info[[sp]]$abundance[high_prob_cells]
  
  species_info[[sp]]$cells <-
    high_prob_cells
  
}
```

## Have another look

``` r
example_sp <-
  unique(sp_range_1$species)[1]

plot_me_reduced <-
  filter(sp_range_1,
         species == example_sp) |>
  filter(cell_id %in% species_info[[example_sp]]$cells)
  

buffer <- 3

ggplot() +
  
  # suitability and coastlines
  geom_spatvector(data = coasts,
                  fill = "#FFF7ED") +
  geom_tile(data = plot_me_reduced,
            aes(x=x,y=y,
                fill = suitability)) +
  
  # bounds
  xlim(c(min(plot_me_reduced$x)-buffer,
         max(plot_me_reduced$x)+buffer)) +
  
  ylim(c(min(plot_me_reduced$y-buffer),
         max(plot_me_reduced$y+buffer))) +
  
  # theme
  ggtitle(example_sp) +
  scale_fill_viridis_c() +
  theme_minimal()
```

![](01_prepare_species_files/figure-gfm/plot%20range%20after%20median%20suitability%20filter-1.png)<!-- -->

# Determine species traits

Trait values are determined by environmental values at each cell.

- **Thermal optimum** - the mean local environmental values of each species across annual mean SST values.

- **Thermal standard deviation** - species weighted mean of the standard deviation of those same values. The weights are the SDM suitability values.

Basis:

- Realised thermal niche closely matches experimentally derived thermal niche <https://doi.org/10.1002/ece3.10974>.

- Geographic distribution of thermal limits - seasonality is important: <https://doi.org/10.1038/s41559-017-0353-x>

``` r
for(sp in unique(sp_range_1$species)){
  
  # extract historical information of occupied cells
  species_annual_mean_sst <-
    annual_mean_sst |>
    
    # add cell IDS
    mutate(cell_id = rownames(annual_mean_sst)) |>
    
    # filter to match the SDM years
    select(cell_id,x,y,contains(as.character(1981:2015))) |>
    
    # filter by cells in which species is present
    filter(cell_id %in% species_info[[sp]]$cells) |>
    
    # remove coordinates and cell IDs
    select(-c(x,y,cell_id))
  
  # create trait data frame per species
  traits <-
    data.frame(thermal_optimum = rowMeans(species_annual_mean_sst),
               thermal_sd      = weighted.mean(x = apply(species_annual_mean_sst, 1, sd),
                                               w = species_info[[sp]]$abundance))
  
  # reassign row names
  rownames(traits) <-
    species_info[[sp]]$cells
  
  # add to species list
  species_info[[sp]]$traits <-
    traits
}
```

# Check for phylogeny

Load in the phylogeny

``` r
fish_phylo <- 
  fishtree::fishtree_phylogeny()

phylo_spp <-
  fish_phylo$tip.label
```

Check the species names in the sdm dataset against the WORMS database for name consistency.

``` r
# check species names
sdm_resolved <-
  taxize::gnr_resolve(names(species_info),
                      data_source_ids = 9)

# retain mismatched names
sdm_mismatched <-
  sdm_resolved |>
  dplyr::filter(score < 0.9)

# check for errors (should be empty)
sdm_mismatched
```

```         
## # A tibble: 0 × 5
## # ℹ 5 variables: user_supplied_name <chr>, submitted_name <chr>,
## #   matched_name <chr>, data_source_title <chr>, score <dbl>
```

Now we do the same for the phylogenetic names

``` r
# check species names
phylo_resolved <-
  taxize::gnr_resolve(names(species_info),
                      data_source_ids = 9)

# retain mismatched names
phylo_mismatched <-
  sdm_resolved |>
  dplyr::filter(score < 0.9)

# check for errors (should also be empty)
phylo_mismatched
```

```         
## # A tibble: 0 × 5
## # ℹ 5 variables: user_supplied_name <chr>, submitted_name <chr>,
## #   matched_name <chr>, data_source_title <chr>, score <dbl>
```

Assuming the species names checks were fine, we filter out species from the SDM dataset for which we lack phylogenetic information.

``` r
sdm_phylo_spp <-
  names(species_info)[names(species_info) %in% phylo_spp]

species_info_phylo <-
  species_info[sdm_phylo_spp]
```

# Export

``` r
saveRDS(species_info_phylo,
        "./data_processed/species/species_initialisation_information.rds")
```
