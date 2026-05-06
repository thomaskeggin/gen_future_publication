# meta -------------------------------------------------------------------------
# author: Thomas Keggin

# set --------------------------------------------------------------------------
library(randtoolbox)
library(tidyverse)

# output directory
dir_out <-
  "./data_processed/configs/partial_homogenisation/" #output

# set the number of runs
start_from     <- 1   # if you are starting from scratch, set this to 1
number_of_runs <- 500 # the number of runs for this batch

# set the number of parameters
number_of_parameters <-
  2

# function to convert the sobol numbers to values within the defined parameter
# space
linMap <- 
  function(x, from, to){
    (x - min(x)) / max(x - min(x)) * (to - from) + from
  }

# generate parameter values ----------------------------------------------------
# if expanding the simulation scheme (you have already run some sims), burn in
# the sobol sequence to avoid parameter repetition. Otherwise, generate the 
# sobol sequence from the start.

if(start_from > 1){
  
  burnin <-
    sobol(start_from-1, init = T)
  
  sobol_sequence <- 
    sobol(number_of_runs, number_of_parameters, init = F)
  
} else {
  
  sobol_sequence <- 
    sobol(number_of_runs, number_of_parameters, init = T)
  
}

# create parameter value table
parameter_table <-
  data.frame(start_from:(start_from+number_of_runs-1),
             sobol_sequence)
colnames(parameter_table) <-
  c("run_id",
    "dispersal_range",
    "adaptive_rate")

# set seed
parameter_table$seed <-
   1989
  # 1995

# set dispersal parameter
# varies from <20 to <15000 km / generation depending on species
# (Green et al. 2015). After removing pelagic species, we get a maximum of
# 500 km.
parameter_table$dispersal_range <-
  round(linMap(parameter_table$dispersal_range,
               from=0,
               to=550),
        0) 

# set mutation rate
# from no adaptation, to the greatest cell temperature change from one timestep
# to another in the timeseries.
parameter_table$adaptive_rate <-
  round(linMap(parameter_table$adaptive_rate,
               from=0,
               to=0.22), 
        4)


# Write table ------------------------------------------------------------------
write_csv(parameter_table, paste0(dir_out,"config_parameters.csv")) #output

# Generate config files --------------------------------------------------------
for(i in 1:nrow(parameter_table)){ 
  
  parameters <- parameter_table[i,]
  
  config_i <- readLines('./scripts/02_configure_genesis/template.R') #input
  
  config_i <- gsub('parameter_table\\$seed', parameters$seed, config_i)
  config_i <- gsub('parameter_table\\$dispersal_range', parameters$dispersal_range, config_i)
  config_i <- gsub('parameter_table\\$adaptive_rate', parameters$adaptive_rate, config_i)
  
  writeLines(config_i, paste0(dir_out,i+start_from-1,'.R'))
  
}

# create bat file --------------------------------------------------------------
run_head     <- c("#!/bin/bash",
                  "#SBATCH --job-name=gen3sis",
                  "#SBATCH --mail-user=thomaskeggin@hotmail.com",
                  "#SBATCH --mail-type=FAIL,END",
                  "#SBATCH --cpus-per-task=1",
                  "#SBATCH --time=96:00:00",
                  "#SBATCH --mem-per-cpu=12000",
                  paste0("#SBATCH --array=1-",number_of_runs),
                  "",
                  "module load gcc/6.3.0 r/4.1.3 udunits2/2.2.28 gdal/3.1.4 geos/3.8.1 proj/6.3.2 zlib/1.2.9",
                  "",
                  paste0("run_id=$((${SLURM_ARRAY_TASK_ID} + ", start_from-1,"))"),
                  "")
rscript      <- 'Rscript'
script_name  <- './scripts/03_run_genesis/run_genesis.R'
config_dir   <- '-c /cluster/home/keggint/gen_future/data_processed/configs/partial_homogenisation/${run_id}.R'
input_dir    <- '-i /cluster/home/keggint/gen_future/data_processed/seascapes/'
output_dir   <- '-o /cluster/scratch/keggint/gen_future/output/'
other_par    <- '-s "all" -v 1'# save intermediate results and be verbose

run_body <- paste(rscript,
                  script_name,
                  config_dir,
                  input_dir,
                  output_dir,
                  other_par)

output_file  <- file(paste0(dir_out,"submit_sims.sh"), #output
                     "wb") 

write(c(run_head, run_body), file = output_file)

close(output_file)
