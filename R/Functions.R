samp_age <- function(input_age, nreaders, bias_mat, sd_mat){
  reader_id <- sample.int(nreaders,length(input_age), replace = TRUE)#apply a random reader to each age estimate
  output_age <- vector(length = length(input_age))
  
  for (i in 1:length(input_age)) {
    output_age[i] <- round(abs(rnorm(1, mean = bias_mat[input_age[i]+1,reader_id[i]], sd = sd_mat[input_age[i]+1,reader_id[i]])))
  }
  return(output_age)
}

boot_age <- function(n_boot, input_age, bias_mat, sd_mat){
  new_ages <- matrix(nrow = length(input_age), ncol = max(n_boot))
  
  for (i in n_boot) {
    new_ages[,i]<- samp_age(input_age, ncol(bias_mat), bias_mat, sd_mat)
  }
  
  return(new_ages)
}

age_files <- function(nsim, wd){
  #This script bootstraps the age data iterations
  
  library(signal)
  
  setwd(wd)
  
  #import TMA age error data, bias columns are (expected age-0.5)
  {
    TMA_bias <- matrix(ncol=7, nrow = 24)
    TMA_bias[,1] <- (as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader1.csv", header=TRUE)[5,-1])-0.5)
    TMA_bias[,2] <- (as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader2.csv", header=TRUE)[5,-1])-0.5)
    TMA_bias[,3] <- (as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader3.csv", header=TRUE)[5,-1])-0.5)
    TMA_bias[,4] <- (as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader4.csv", header=TRUE)[5,-1])-0.5)
    TMA_bias[,5] <- (as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader5.csv", header=TRUE)[5,-1])-0.5)
    TMA_bias[,6] <- (as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader6.csv", header=TRUE)[5,-1])-0.5)
    TMA_bias[,7] <- (as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader7.csv", header=TRUE)[5,-1])-0.5)
    colnames(TMA_bias) <- c("bias_R1", "bias_R2", "bias_R3", "bias_R4", "bias_R5", "bias_R6", "bias_R7")
  }
  
  
  {
    TMA_sd <- matrix(ncol=7, nrow = 24)
    TMA_sd[,1] <- as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader1.csv", header=TRUE)[4,-1])
    TMA_sd[,2] <- as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader2.csv", header=TRUE)[4,-1])
    TMA_sd[,3] <- as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader3.csv", header=TRUE)[4,-1])
    TMA_sd[,4] <- as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader4.csv", header=TRUE)[4,-1])
    TMA_sd[,5] <- as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader5.csv", header=TRUE)[4,-1])
    TMA_sd[,6] <- as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader6.csv", header=TRUE)[4,-1])
    TMA_sd[,7] <- as.numeric(read.csv("./data/7_readers_complete_dataset_TMB_Age_23/Results/Pollock SS3_format_Reader7.csv", header=TRUE)[4,-1])
    colnames(TMA_sd) <- c("SD_R1", "SD_R2", "SD_R3", "SD_R4", "SD_R5", "SD_R6", "SD_R7")
  }
  
  df <- read.csv(paste0("./data/AGP_MMCNN_BSsurvey_pollock2014to2018.csv"))
  df <- subset(df, !is.na(final_age))
  df2 <- df
  
  # Apply Savitzky-Golay filter with specified parameters
  spectra <- df[, 8:ncol(df)]
  for (i in 1:nrow(spectra)) {
    spectra[i,] <- sgolayfilt(as.matrix(spectra[i,]), p = 2, n = 17, m = 1)
  }
  
  write.csv(spectra, "./data/spectra_sav_golay.csv", row.names=FALSE)
  
  message("Sav-Golay Complete")
  
  #create an 80:20 training:test split
  split_ratio <- c("training" = 0.8, "test" = 0.2)
  num_training <- floor(length(df[, 2]) * split_ratio["training"])
  num_test <- length(df[, 2]) - num_training
  values <- c(rep("training", num_training), rep("test", num_test))
  
  #bootstrap the age data
  input_age <- as.numeric(df$final_age)
  set.seed(581)
  new_ages <- boot_age(nsim, input_age, TMA_bias, TMA_sd) 
  
  # Print the number of simulations
  message("Total age error files: ", length(nsim))
  
  #create a folder to store all the sims
  dir.create("./sims_err", showWarnings = FALSE) 
  
  df <- df[,1:7]
  
  set.seed(581)
  for (i in nsim) {
    dir.create(paste0("./sims_err/",i),showWarnings = FALSE)
    df[,2] <- sample(values, size = length(df[, 2]), replace = FALSE) #randomly assign test/train
    df[,3] <- new_ages[,i] #replace original age estimates with bootstrap ages
    #df[,8:ncol(df)] <- spectra #replace spectra with transformed spectra
    #df[, 3][df[, 3] == 0] <- 1 #change age 0 to age 1, only test purposes
    write.csv(df, paste0("./sims_err/",i,"/input.csv"), row.names=FALSE)
  }
  
  # Print the number of simulations
  message("Total known age files: ", length(nsim))
  
  #create a folder to store all the sims
  dir.create("./sims_known", showWarnings = FALSE) 
  
  df <- df2[,1:7]
  
  set.seed(581)
  for (i in nsim) {
    dir.create(paste0("./sims_known/",i),showWarnings = FALSE)
    df[,2] <- sample(values, size = length(df[, 2]), replace = FALSE) #randomly assign test/train
    #df[,8:ncol(df)] <- spectra #replace spectra with transformed spectra
    write.csv(df, paste0("./sims_known/",i,"/input.csv"), row.names=FALSE)
  }
  
}