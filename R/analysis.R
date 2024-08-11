wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)

metrics <- matrix(data = NA, nrow = nsim, ncol = 5)
colnames(metrics) <- c("iteration","train_R2", "train_RMSE", "test_R2", "test_RMSE")

for (j in 1:nsim) {
  metrics[j,] <- as.matrix(read.csv(paste0("./test_sims/",j,"/Output/Data/metrics",j,".csv"), header = TRUE)[1,])
}

metrics <- as.data.frame(metrics)
metrics$cumulative_mean_train_R2 <- cumsum(metrics$train_R2) / seq_along(metrics$train_R2)
metrics$cumulative_mean_test_R2 <- cumsum(metrics$test_R2) / seq_along(metrics$test_R2)
plot(metrics$cumulative_mean_train_R2, type = 'l', ylim = c(0.85, 0.89))
lines(metrics$cumulative_mean_test_R2, type = 'l')

write.csv(metrics, 
          file = "./sims/metrics.csv", 
          row.names = FALSE)