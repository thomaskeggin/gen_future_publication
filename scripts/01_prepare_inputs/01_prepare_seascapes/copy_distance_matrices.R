library(progress)

timesteps <-
  1:86

pb <-
  progress_bar$new(total = length(timesteps),
                   format = ":curent of :total :eta")

for(step in timesteps){
  
  file.copy("./data_processed/seascapes/distances_full/distances_full_0.rds",
            paste0("./data_processed/seascapes/distances_full/distances_full_",step,".rds"))
  
}
