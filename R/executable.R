#This script prepares all the data and folder structure then executes the MMCNN
#R script in a for loop

wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)


nsim <-5L

# Construct the command to run the external R script
command <- sprintf('Rscript ./R/boot_age.R %d', nsim)
# Execute the command
system(command)

for (j in 1:nsim) {
  # Construct the command to run the external R script
  command <- sprintf('Rscript ./R/bootstrap_MMCNN_err.R %d', j)
  
  # Execute the command
  system(command)
}

for (j in 1:nsim) {
  # Construct the command to run the external R script
  command <- sprintf('Rscript ./R/bootstrap_MMCNN_known.R %d', j)
  
  # Execute the command
  system(command)
}