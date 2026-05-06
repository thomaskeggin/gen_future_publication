# set --------------------------------------------------------------------------
library(gen3sis)
library(parallel)

# configuration ----------------------------------------------------------------
# read all the files in the config folder
config_files <-
  list.files("./data_processed/configs/partial_homogenisation/") #input

# run sims in parallel ---------------------------------------------------------
# parallel loop
mclapply(1:500,
         mc.cores=15,
         function(config){
           
           # designate the configuration file
           configuration <-
             paste0("./data_processed/configs/partial_homogenisation/", #input
                    config,".R")
           
           # redirect the standard output into a log file
           sink(file = paste0("./gen3sis_output_",config,".txt"))
           
           # run the simulation
           run_simulation(config = configuration,
                          landscape = "./data_processed/seascapes/", #input
                          output_directory = "/storage/gen_future/output/partial_homogenisation", #output
                          call_observer = "all",
                          enable_gc = TRUE,
                          verbose = 1)
           
           # close the log file connection
           sink()
           
         })

