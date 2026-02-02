#This script prepares all the data and folder structure then executes the MMCNN
#R script in a for loop

#wd <- "~/nir_boot"
wd <- "G:/My Drive/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)

source(paste0(wd,"/R/Functions.R"))

df_path = "./data/AGP_MMCNN_BSsurvey_pollock2014to2018.csv"

#Apply Savitzky-Golay filter to FT-NIRS spectra
apply_sg(wd, 
         df_path, 
         apply_sg =  FALSE,  #CHANGE TO TRUE FOR FINAL!!!!
         start_col = 8)

nsim = 1L:2L

#generate bootstrap age files for n iterations
age_files(nsim, 
          wd, 
          reader_dir = "./data/7_readers_complete_dataset_TMB_Age_23/Results", 
          df_path, 
          age_col = 3, 
          train_test_col = 2, 
          meta_cols = 1:7)

for (j in nsim) {
  # Construct the command to run the external R script
  command <- sprintf('Rscript ./R/bootstrap_MMCNN_err.R %d "%s"', j, wd)
  
  # Execute the command
  system(command)
}

for (j in nsim) {
  # Construct the command to run the external R script
  command <- sprintf('Rscript ./R/bootstrap_MMCNN_known.R %d "%s"', j, wd)
  
  # Execute the command
  system(command)
}
