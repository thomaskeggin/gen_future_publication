# load -------------------------------------------------------------------------
sst_mean <-
  readRDS("./data_processed/seascapes/landscapes.rds")$sst_mean #input

# wrangle ----------------------------------------------------------------------
years <-
  colnames(sst_mean)[-c(1:2)]

steps_to_years <-
  data.frame(timestep = 0:(length(years)-1),
             year     = years)

# export -----------------------------------------------------------------------
readr::write_csv(steps_to_years,
                 "./data_processed/seascapes/steps_to_years.csv") #output
