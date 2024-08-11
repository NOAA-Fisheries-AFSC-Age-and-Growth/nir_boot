#This script bootstraps the age data iterations

library(signal)

wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)
source(paste0(wd,"/R/Functions.R"))

# Get command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Convert the argument to integer
nsim <- as.integer(args[1])

# Print the number of simulations
message("Total simulations files will be created for ", nsim)

df <- read.csv(paste0("./data/AGP_MMCNN_BSsurvey_pollock2014to2018.csv"))

# Apply Savitzky-Golay filter with specified parameters
spectra <- df[, 8:ncol(df)]
for (i in 1:nrow(spectra)) {
  spectra[i,] <- sgolayfilt(as.matrix(spectra[i,]), p = 2, n = 17, m = 1)
}

#create an 80:20 training:test split
split_ratio <- c("training" = 0.8, "test" = 0.2)
num_training <- floor(length(df[, 2]) * split_ratio["training"])
num_test <- length(df[, 2]) - num_training
values <- c(rep("training", num_training), rep("test", num_test))

#create a folder to store all the sims
dir.create("./sims_known", showWarnings = FALSE) 

for (i in 1:nsim) {
  dir.create(paste0("./sims_known/",i),showWarnings = FALSE)
  df[,2] <- sample(values, size = length(df[, 2]), replace = FALSE) #randomly assign test/train
  df[,8:ncol(df)] <- spectra #replace spectra with transformed spectra
  write.csv(df, paste0("./sims_known/",i,"/input.csv"), row.names=FALSE)
}
