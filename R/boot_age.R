#This script bootstraps the age data and runs the MMCNN for all iterations

wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)
source(paste0(wd,"/R/Functions.R"))

nsim <- 2

#import TMA age error data, bias columns are (expected age-0.5)
{
  TMA_bias <- matrix(ncol=7, nrow = 19)
  TMA_bias[,1] <- (as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader1.csv", header=TRUE)[5,-1])-0.5)
  TMA_bias[,2] <- (as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader2.csv", header=TRUE)[5,-1])-0.5)
  TMA_bias[,3] <- (as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader3.csv", header=TRUE)[5,-1])-0.5)
  TMA_bias[,4] <- (as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader4.csv", header=TRUE)[5,-1])-0.5)
  TMA_bias[,5] <- (as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader5.csv", header=TRUE)[5,-1])-0.5)
  TMA_bias[,6] <- (as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader6.csv", header=TRUE)[5,-1])-0.5)
  TMA_bias[,7] <- (as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader7.csv", header=TRUE)[5,-1])-0.5)
  colnames(TMA_bias) <- c("bias_R1", "bias_R2", "bias_R3", "bias_R4", "bias_R5", "bias_R6", "bias_R7")
}


{
  TMA_sd <- matrix(ncol=7, nrow = 19)
  TMA_sd[,1] <- as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader1.csv", header=TRUE)[4,-1])
  TMA_sd[,2] <- as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader2.csv", header=TRUE)[4,-1])
  TMA_sd[,3] <- as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader3.csv", header=TRUE)[4,-1])
  TMA_sd[,4] <- as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader4.csv", header=TRUE)[4,-1])
  TMA_sd[,5] <- as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader5.csv", header=TRUE)[4,-1])
  TMA_sd[,6] <- as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader6.csv", header=TRUE)[4,-1])
  TMA_sd[,7] <- as.numeric(read.csv("./data/7_reader_TMA_TMB/Results/Pollock SS3_format_Reader7.csv", header=TRUE)[4,-1])
  colnames(TMA_sd) <- c("SD_R1", "SD_R2", "SD_R3", "SD_R4", "SD_R5", "SD_R6", "SD_R7")
}

df <- read.csv(paste0("./data/AGP_MMCNN_BSsurvey_pollock2014to2018.csv"))

input_age <- as.numeric(df$final_age)

set.seed(581)
new_ages <- boot_age(nsim, input_age, TMA_bias, TMA_sd) #bootstrap age estimates 

dir.create("./sims", showWarnings = FALSE) #creat a folder to store all the sims

#replace original age estimates with bootstrap ages
for (i in 1:ncol(new_ages)) {
  dir.create(paste0("./sims/",i),showWarnings = FALSE)
  df$final_age <- new_ages[,i]
  write.csv(df, paste0("./sims/",i,"/input.csv"), row.names=FALSE)
}

