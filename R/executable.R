#This script prepares all the data and folder structure then executes the MMCNN
#R script in a for loop

wd <- "~/nir_boot"
#wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)


nsim <-101L:103L

# Construct the command to run the external R script
command <- sprintf('Rscript ./R/boot_age.R %d "%s"', nsim, wd)
# Execute the command
system(command)

for (j in 1:nsim) {
  # Construct the command to run the external R script
  command <- sprintf('Rscript ./R/bootstrap_MMCNN_err.R %d "%s"', j, wd)
  
  # Execute the command
  system(command)
}

for (j in 1:nsim) {
  # Construct the command to run the external R script
  command <- sprintf('Rscript ./R/bootstrap_MMCNN_known.R %d "%s"', j, wd)
  
  # Execute the command
  system(command)
}
