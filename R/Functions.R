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

known_vs_error <- function(j, wd) {
  library(ggplot2)
  library(dplyr)
  
  train_known <- read.csv(paste0(wd, "/sims_known/", j, "/Output/Data/train_predictions.csv"))
  test_known <- read.csv(paste0(wd, "/sims_known/", j, "/Output/Data/test_predictions.csv"))
  train_err <- read.csv(paste0(wd, "/sims_err/", j, "/Output/Data/train_predictions.csv"))
  test_err <- read.csv(paste0(wd, "/sims_err/", j, "/Output/Data/test_predictions.csv"))
  
  write.csv(train_known, file = paste0(wd, "/sims_err/", j, "/Output/Data/train_predictions_known.csv"), row.names = FALSE)
  
  plot <- ggplot(train_known, aes(x = train, y = pred)) +
    geom_point(color = 'blue', fill = 'lightblue', size = 4, alpha = 0.5, shape = 21, stroke = 1) +
    geom_smooth(method = 'lm', color = 'black', size = 1.5, alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1) +
    scale_x_continuous(limits = c(-1, 30)) +
    scale_y_continuous(limits = c(-1, 30)) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      plot.title = element_text(size = 25, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Training Set",
      x = "Traditional Age (years)",
      y = "FT-NIR Age (years)"
    )
  
  ggsave(filename = paste0(wd, "/sims_err/", j, "/Output/Figures/TrainingSet_Known.png"), plot = plot, width = 12, height = 12, units = "in", dpi = 300)
  
  print(plot)
  
  write.csv(test_known, file = paste0(wd, "/sims_err/", j, "/Output/Data/test_predictions_known.csv"), row.names = FALSE)
  
  plot <- ggplot(test_known, aes(x = train, y = pred)) +
    geom_point(color = 'blue', fill = 'lightblue', size = 4, alpha = 0.5, shape = 21, stroke = 1) +
    geom_smooth(method = 'lm', color = 'black', size = 1.5, alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1) +
    scale_x_continuous(limits = c(-1, 30)) +
    scale_y_continuous(limits = c(-1, 30)) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      plot.title = element_text(size = 25, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Test Set",
      x = "Traditional Age (years)",
      y = "FT-NIR Age (years)"
    )
  
  ggsave(filename = paste0(wd, "/sims_err/", j, "/Output/Figures/TestSet_Known.png"), plot = plot, width = 12, height = 12, units = "in", dpi = 300)
  
  print(plot)
  
  
  data_known <- test_known %>%
    mutate(
      Mean = (train + pred) / 2,
      Difference = train - pred,
      Mean_diff = mean(Difference),
      SD_diff = sd(Difference),
      Limit_of_Agreement_upper = Mean_diff + 1.96 * SD_diff,
      Limit_of_Agreement_lower = Mean_diff - 1.96 * SD_diff
    )
  
  plot <- ggplot(data_known, aes(x = Mean, y = Difference)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = data_known$Mean_diff, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = data_known$Limit_of_Agreement_upper, color = "red", linetype = "dashed") +
    geom_hline(yintercept = data_known$Limit_of_Agreement_lower, color = "red", linetype = "dashed") +
    labs(
      title = "Bland-Altman Plot",
      x = "Mean of Actual and Predicted",
      y = "Difference between Actual and Predicted"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  ggsave(filename = paste0(wd, "/sims_err/", j, "/Output/Figures/BlandAltman_Known.png"), plot = plot, width = 12, height = 8, units = "in", dpi = 300)
  
  print(plot)
  
  r2_score <- function(y_true, y_pred) {
    ss_total <- sum((y_true - mean(y_true))^2)
    ss_residual <- sum((y_true - y_pred)^2)
    r2 <- 1 - (ss_residual / ss_total)
    return(r2)
  }
  
  rmse_manual <- function(actual, predicted) {
    mse <- mean((actual - predicted)^2)  # Calculate Mean Squared Error
    return(sqrt(mse))  # Return the square root of MSE
  }
  
  r_squared_tr_known <- r2_score(train_known$train, train_err$pred)
  rmse_tr_known <- rmse_manual(train_known$train, train_err$pred)
  
  r_squared_known <- r2_score(test_known$train, test_err$pred)
  rmse_known <- rmse_manual(test_known$train, test_err$pred)
  
  #error scenario
  r_squared_tr <- r2_score(train_err$train, train_err$pred)
  rmse_tr <- rmse_manual(train_err$train, train_err$pred)
  
  r_squared <- r2_score(test_err$train, test_err$pred)
  rmse <- rmse_manual(test_err$train, test_err$pred)
  
  metrics <- matrix(data = NA, nrow = 1, ncol = 9)
  colnames(metrics) <- c("iteration","train_R2_known", "train_RMSE_known", "test_R2_known", "test_RMSE_known",
                         "train_R2", "train_RMSE", "test_R2", "test_RMSE")
  metrics[1,1] <- j
  metrics[1,2] <- r_squared_tr_known
  metrics[1,3] <- rmse_tr_known
  metrics[1,4] <- r_squared_known
  metrics[1,5] <- rmse_known
  metrics[1,6] <- as.numeric(r_squared_tr)
  metrics[1,7] <- as.numeric(rmse_tr)
  metrics[1,8] <- as.numeric(r_squared)
  metrics[1,9] <- as.numeric(rmse)
  
  write.csv(metrics, 
            file = paste0(wd, "/sims_err/", j, "/Output/Data/metrics",j,".csv"), 
            row.names = FALSE)
}

known <- function(j, wd) {
  library(ggplot2)
  library(dplyr)
  
  train_known <- read.csv(paste0(wd, "/sims_known/", j, "/Output/Data/train_predictions.csv"))
  test_known <- read.csv(paste0(wd, "/sims_known/", j, "/Output/Data/test_predictions.csv"))
 
  plot <- ggplot(train_known, aes(x = train, y = pred)) +
    geom_point(color = 'blue', fill = 'lightblue', size = 4, alpha = 0.5, shape = 21, stroke = 1) +
    geom_smooth(method = 'lm', color = 'black', size = 1.5, alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1) +
    scale_x_continuous(limits = c(-1, 30)) +
    scale_y_continuous(limits = c(-1, 30)) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      plot.title = element_text(size = 25, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Training Set",
      x = "Traditional Age (years)",
      y = "FT-NIR Age (years)"
    )
  
  ggsave(filename = paste0(wd, "/sims_known/", j, "/Output/Figures/TrainingSet_Known.png"), plot = plot, width = 12, height = 12, units = "in", dpi = 300)
  
  print(plot)

  plot <- ggplot(test_known, aes(x = train, y = pred)) +
    geom_point(color = 'blue', fill = 'lightblue', size = 4, alpha = 0.5, shape = 21, stroke = 1) +
    geom_smooth(method = 'lm', color = 'black', size = 1.5, alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1) +
    scale_x_continuous(limits = c(-1, 30)) +
    scale_y_continuous(limits = c(-1, 30)) +
    theme_minimal() +
    theme(
      text = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      plot.title = element_text(size = 25, hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = "Test Set",
      x = "Traditional Age (years)",
      y = "FT-NIR Age (years)"
    )
  
  ggsave(filename = paste0(wd, "/sims_known/", j, "/Output/Figures/TestSet_Known.png"), plot = plot, width = 12, height = 12, units = "in", dpi = 300)
  
  print(plot)
  
  
  data_known <- test_known %>%
    mutate(
      Mean = (train + pred) / 2,
      Difference = train - pred,
      Mean_diff = mean(Difference),
      SD_diff = sd(Difference),
      Limit_of_Agreement_upper = Mean_diff + 1.96 * SD_diff,
      Limit_of_Agreement_lower = Mean_diff - 1.96 * SD_diff
    )
  
  plot <- ggplot(data_known, aes(x = Mean, y = Difference)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = data_known$Mean_diff, color = "blue", linetype = "dashed") +
    geom_hline(yintercept = data_known$Limit_of_Agreement_upper, color = "red", linetype = "dashed") +
    geom_hline(yintercept = data_known$Limit_of_Agreement_lower, color = "red", linetype = "dashed") +
    labs(
      title = "Bland-Altman Plot",
      x = "Mean of Actual and Predicted",
      y = "Difference between Actual and Predicted"
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 16),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  
  ggsave(filename = paste0(wd, "/sims_known/", j, "/Output/Figures/BlandAltman_Known.png"), plot = plot, width = 12, height = 8, units = "in", dpi = 300)
  
  print(plot)
  
  r2_score <- function(y_true, y_pred) {
    ss_total <- sum((y_true - mean(y_true))^2)
    ss_residual <- sum((y_true - y_pred)^2)
    r2 <- 1 - (ss_residual / ss_total)
    return(r2)
  }
  
  rmse_manual <- function(actual, predicted) {
    mse <- mean((actual - predicted)^2)  # Calculate Mean Squared Error
    return(sqrt(mse))  # Return the square root of MSE
  }
  
  r_squared_tr <- r2_score(train_known$train, train_known$pred)
  rmse_tr <- rmse_manual(train_known$train, train_known$pred)
  
  r_squared <- r2_score(test_known$train, test_known$pred)
  rmse <- rmse_manual(test_known$train, test_known$pred)
  
  metrics <- matrix(data = NA, nrow = 1, ncol = 5)
  colnames(metrics) <- c("iteration", "train_R2", "train_RMSE", "test_R2", "test_RMSE")
  metrics[1,1] <- j
  metrics[1,2] <- as.numeric(r_squared_tr)
  metrics[1,3] <- as.numeric(rmse_tr)
  metrics[1,4] <- as.numeric(r_squared)
  metrics[1,5] <- as.numeric(rmse)
  
  write.csv(metrics, 
            file = paste0(wd, "/sims_known/", j, "/Output/Data/metrics",j,".csv"), 
            row.names = FALSE)
}