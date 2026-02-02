apply_sg <- function(wd, df_path, apply_sg, start_col){
  library(signal)
  
  setwd(wd)
  
  df <- read.csv(paste0(df_path))
  df <- subset(df, !is.na(final_age))
  
  if (apply_sg) {
    message("Applying Savitzky-Golay filter...")
    spectra <- df[, start_col:ncol(df)]
    for (i in 1:nrow(spectra)) {
      spectra[i,] <- sgolayfilt(as.matrix(spectra[i,]), p = 2, n = 17, m = 1)
    }
    write.csv(spectra, "./data/spectra.csv", row.names=FALSE)
    message("Sav-Golay Complete")
  } else {
    message("Skipping Savitzky-Golay filter (using raw spectra).")
    spectra <- df[, start_col:ncol(df)]
    write.csv(spectra, "./data/spectra.csv", row.names=FALSE)
  }
}

samp_age <- function(input_age, nreaders, bias_mat, sd_mat){
  reader_id <- sample.int(nreaders,length(input_age), replace = TRUE)#apply a random reader to each age estimate
  output_age <- vector(length = length(input_age))
  
  #for (i in 1:length(input_age)) {
  #  output_age[i] <- round(abs(rnorm(1, mean = bias_mat[input_age[i]+1,reader_id[i]], sd = sd_mat[input_age[i]+1,reader_id[i]])))
  #}
  
  #AE_mat is list of ageing error matrices for each reader
  AE_mat <- vector("list", nreaders)
  for (i in 1:nreaders) {
    AE_mat[[i]] <- diag(nrow(bias_mat))
  }
  
  ages<-(1:nrow(AE_mat[[1]]))-1
  
  for (k in 1:nreaders) {
    for (i in 1:nrow(AE_mat[[1]])) {
      for(j in 1:nrow(AE_mat[[1]])){
        if(j==1){
          AE_mat[[k]][i,j]<-pnorm(ages[j]+0.5, mean = bias_mat[i,k], sd = sd_mat[i,k])
        }else if (j %in% 2:(nrow(AE_mat[[1]])-1)){
          AE_mat[[k]][i,j]<-pnorm(ages[j]+0.5, mean = bias_mat[i,k], sd = sd_mat[i,k])-pnorm(ages[j]-0.5, mean = bias_mat[i,k], sd = sd_mat[i,k])
        }else if (j==nrow(AE_mat[[1]])){
          AE_mat[[k]][i,j]<-1-pnorm(ages[j]-0.5, mean = bias_mat[i,k], sd = sd_mat[i,k])
        }
      }
    }
  }
  
  #sample age w/ ageing error from each readers age error mat w/ multinomial dist
  output_age <- vector(length = length(input_age))
  for (i in 1:length(input_age)){
    #the +1 accounts for the fact that AE_mat starts at age-0, the definition for age 0 is in row 1
    #the -1 accounts for the random samples of rmultinom also include age-0 and age-0 is in cell 1
    output_age[i] <- (which(rmultinom(1, size = 1, prob = AE_mat[[reader_id[i]]][input_age[i]+1,]) == 1)-1)
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

age_files <- function(nsim, wd, reader_dir, df_path, age_col, train_test_col, meta_cols){
  #This script bootstraps the age data iterations
  setwd(wd)
  
  reader_files <- list.files(reader_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(reader_files) == 0) {
    stop(paste("No CSV files found in directory:", reader_dir))
  }
  
  n_readers <- length(reader_files)
  message(paste("Found", n_readers, "reader files in directory. Processing..."))
  
  #Empty matrics for bias and SD
  TMA_bias <- NULL
  TMA_sd <- NULL
  
  for (i in 1:n_readers) {
    current_file_data <- read.csv(reader_files[i], header = TRUE)
    
    #import TMA age error data, bias columns are (expected age-0.5)
    current_bias <- as.numeric(current_file_data[5, -1]) - 0.5
    
    current_sd <- as.numeric(current_file_data[4, -1])
    
    TMA_bias <- cbind(TMA_bias, current_bias)
    TMA_sd <- cbind(TMA_sd, current_sd)
  }
  
  colnames(TMA_bias) <- paste0("bias_R", 1:n_readers)
  colnames(TMA_sd) <- paste0("SD_R", 1:n_readers)
  
  df <- read.csv(paste0(df_path))
  df <- subset(df, !is.na(df[,age_col]))
  df2 <- df
  
  #create an 80:20 training:test split
  split_ratio <- c("training" = 0.8, "test" = 0.2)
  num_training <- floor(length(df[, train_test_col]) * split_ratio["training"])
  num_test <- length(df[, train_test_col]) - num_training
  values <- c(rep("training", num_training), rep("test", num_test))
  
  #bootstrap the age data
  input_age <- as.numeric(df[,age_col])
  set.seed(581)
  new_ages <- boot_age(nsim, input_age, TMA_bias, TMA_sd) 
  
  # Print the number of simulations
  message("Total age error files: ", length(nsim))
  
  #create a folder to store all the sims
  dir.create("./sims_err", showWarnings = FALSE) 
  
  df <- df[,meta_cols]
  
  set.seed(581)
  for (i in nsim) {
    dir.create(paste0("./sims_err/",i),showWarnings = FALSE)
    df[,train_test_col] <- sample(values, size = length(df[,train_test_col]), replace = FALSE) #randomly assign test/train
    df[,age_col] <- new_ages[,i] #replace original age estimates with bootstrap ages
    write.csv(df, paste0("./sims_err/",i,"/input.csv"), row.names=FALSE)
  }
  
  # Print the number of simulations
  message("Total known age files: ", length(nsim))
  
  #create a folder to store all the sims
  dir.create("./sims_known", showWarnings = FALSE) 
  
  df <- df2[,meta_cols]
  
  set.seed(581)
  for (i in nsim) {
    dir.create(paste0("./sims_known/",i),showWarnings = FALSE)
    df[,train_test_col] <- sample(values, size = length(df[,train_test_col]), replace = FALSE) #randomly assign test/train
    write.csv(df, paste0("./sims_known/",i,"/input.csv"), row.names=FALSE)
  }
  
}