# set --------------------------------------------------------------------------
library(tidyverse)
library(pipelinemapper)

wrap_chrs <-
  20

# 01_prepare_inputs ------------------------------------------------------------
directories <-
  dir("./scripts/01_prepare_inputs/",
             full.names = T)

for(i in directories){
  
  # map the pipeline
  pipeline <-
    mapPipeline(i) |> 
    as_tibble() |> 
    mutate(file   = paste(file_directory,file_basename)     |> str_wrap(wrap_chrs),
           script = paste(script_directory,script_basename) |> str_wrap(wrap_chrs)) |> 
    graphPipeline() |> 
    plotPipeline()
  
  # output the pipeline plot
  ggsave(paste0(i,"/pipeline_map.jpg"),
         pipeline)
  
}

# 02_configure_genesis ---------------------------------------------------------
# map the pipeline
pipeline <-
  mapPipeline("./scripts/02_configure_genesis/") |> 
  as_tibble() |> 
  mutate(file   = paste(file_directory,file_basename)     |> str_wrap(wrap_chrs),
        script = paste(script_directory,script_basename) |> str_wrap(wrap_chrs)) |> 
  graphPipeline() |> 
  plotPipeline()

# output the pipeline plot
ggsave("./scripts/02_configure_genesis/pipeline_map.jpg",
       pipeline)

# 03_run_genesis ---------------------------------------------------------------
# map the pipeline
pipeline <-
  mapPipeline("./scripts/03_run_genesis//") |> 
  as_tibble() |> 
  mutate(file   = paste(file_directory,file_basename)     |> str_wrap(wrap_chrs),
         script = paste(script_directory,script_basename) |> str_wrap(wrap_chrs)) |> 
  graphPipeline() |> 
  plotPipeline()

# output the pipeline plot
ggsave("./scripts/03_run_genesis/pipeline_map.jpg",
       pipeline)

# 04_define_ecoregions ---------------------------------------------------------
# map the pipeline
pipeline <-
  mapPipeline("./scripts/04_define_ecoregions/") |> 
  as_tibble() |> 
  mutate(file   = paste(file_directory,file_basename)     |> str_wrap(wrap_chrs),
         script = paste(script_directory,script_basename) |> str_wrap(wrap_chrs)) |> 
  graphPipeline() |> 
  plotPipeline()

# output the pipeline plot
ggsave("./scripts/04_define_ecoregions/pipeline_map.jpg",
       pipeline)

# 04_process outputs -----------------------------------------------------------
# map the pipeline
pipeline <-
  mapPipeline("./scripts/05_process_outputs/") |> 
  as_tibble() |> 
  mutate(file   = paste(file_directory,file_basename)     |> str_wrap(wrap_chrs),
         script = paste(script_directory,script_basename) |> str_wrap(wrap_chrs)) |> 
  graphPipeline() |> 
  plotPipeline()

# output the pipeline plot
ggsave("./scripts/05_process_outputs/pipeline_map.jpg",
       pipeline,
       height = 20,
       width = 20)

# 05_process outputs -----------------------------------------------------------
directories <-
  dir("./scripts/06_analysis/",
      full.names = T)

for(i in directories){
  
  # map the pipeline
  pipeline <-
    mapPipeline(i) |> 
    as_tibble() |> 
    mutate(file   = paste(file_directory,file_basename)     |> str_wrap(wrap_chrs),
           script = paste(script_directory,script_basename) |> str_wrap(wrap_chrs)) |> 
    graphPipeline() |> 
    plotPipeline()
  
  # output the pipeline plot
  ggsave(paste0(i,"/pipeline_map.jpg"),
         pipeline,
         height = 20,
         width = 20)
  
}







