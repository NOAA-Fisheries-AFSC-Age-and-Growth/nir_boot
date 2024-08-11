library(ggplot2)
library(tidyr)

wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)

dir.create(paste0("./Output"),showWarnings = FALSE)

nsim = 20

#Ageing Error
{
  metrics_err <- matrix(data = NA, nrow = nsim, ncol = 5)
  colnames(metrics_err) <- c("iteration","train_R2", "train_RMSE", "test_R2", "test_RMSE")
  
  for (j in 1:nsim) {
    metrics_err[j,] <- as.matrix(read.csv(paste0("./sims_err/",j,"/Output/Data/metrics",j,".csv"), header = TRUE)[1,])
  }
  
  data_wide_R2_err <- as.data.frame(metrics_err[,c(1,2,4)])
  data_wide_RMSE_err <- as.data.frame(metrics_err[,c(1,3,5)])
  
  metrics_err <- as.data.frame(metrics_err)
  metrics_err$cumulative_mean_train_R2 <- cumsum(metrics_err$train_R2) / seq_along(metrics_err$train_R2)
  metrics_err$cumulative_mean_test_R2 <- cumsum(metrics_err$test_R2) / seq_along(metrics_err$test_R2)
  plot(metrics_err$cumulative_mean_train_R2, type = 'l', ylim = c(0.85, 0.89))
  lines(metrics_err$cumulative_mean_test_R2, type = 'l')
  
  write.csv(metrics_err, 
            file = "./Output/metrics_err.csv", 
            row.names = FALSE)
}

#known Age
{
  metrics_known <- matrix(data = NA, nrow = nsim, ncol = 5)
  colnames(metrics_known) <- c("iteration","train_R2", "train_RMSE", "test_R2", "test_RMSE")
  
  for (j in 1:nsim) {
    metrics_known[j,] <- as.matrix(read.csv(paste0("./sims_known/",j,"/Output/Data/metrics",j,".csv"), header = TRUE)[1,])
  }
  
  data_wide_R2_known <- as.data.frame(metrics_known[,c(1,2,4)])
  data_wide_RMSE_known <- as.data.frame(metrics_known[,c(1,3,5)])
  
  metrics_known <- as.data.frame(metrics_known)
  metrics_known$cumulative_mean_train_R2 <- cumsum(metrics_known$train_R2) / seq_along(metrics_known$train_R2)
  metrics_known$cumulative_mean_test_R2 <- cumsum(metrics_known$test_R2) / seq_along(metrics_known$test_R2)
  plot(metrics_known$cumulative_mean_train_R2, type = 'l', ylim = c(0.85, 0.89))
  lines(metrics_known$cumulative_mean_test_R2, type = 'l')
  
  write.csv(metrics_known, 
            file = "./Output/metrics_known.csv", 
            row.names = FALSE)
}



#NEED TO MODIFY TO ACTUALLY PLOT KNOWN SCENARIO ONCE THAT IS RUN!!!!!

# RMSE Violin Plot
legend_name <- "Age Scenario"
{
  # Reshape the data
  data_long_RMSE_err <- data_wide_RMSE_err %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),  # Select columns that start with "train" or "test"
      names_to = "Variable",            # Name for the new key column
      values_to = "Value"               # Name for the new value column
    )
  
  # Reshape the data
  data_long_RMSE_known <- data_wide_RMSE_known %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),  # Select columns that start with "train" or "test"
      names_to = "Variable",            # Name for the new key column
      values_to = "Value"               # Name for the new value column
    )
  
  data_long_RMSE_err$Category <- "Ageing Error"
  data_long_RMSE_known$Category <- "Known Age"
  data_long_RMSE <- rbind(data_long_RMSE_err, data_long_RMSE_known)
  
  # Create a violin plot
  RMSE_plot <- ggplot(data_long_RMSE, aes(x = Variable, y = Value, fill = Category, color = Category)) +
    geom_violin(trim = FALSE, linewidth = 1.25, alpha = 0.5) +
    labs(
      x = "",
      y = "RMSE (y)"
    ) +
    theme_classic() +
    scale_fill_manual(name = legend_name, values = c("Ageing Error" = "skyblue", "Known Age" = "orange")) +
    scale_color_manual(name = legend_name,values = c("Ageing Error" = "deepskyblue", "Known Age" = "darkorange")) +
    scale_x_discrete(
      labels = c(
        "train_RMSE" = "Training", 
        "test_RMSE" = "Test"
      )
    ) +
    theme(
      axis.title.x = element_text(size = 14, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 14, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 12),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 12),                        # Y-axis text font size
      legend.title = element_text(size = 12, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 10)                         # Legend text font size
    )
}

# R2 Violin Plot
{
  # Reshape the data
  data_long_R2_err <- data_wide_R2_err %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),  # Select columns that start with "train" or "test"
      names_to = "Variable",            # Name for the new key column
      values_to = "Value"               # Name for the new value column
    )
  
  # Reshape the data
  data_long_R2_known <- data_wide_R2_known %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),  # Select columns that start with "train" or "test"
      names_to = "Variable",            # Name for the new key column
      values_to = "Value"               # Name for the new value column
    )
  
  data_long_R2_err$Category <- "Ageing Error"
  data_long_R2_known$Category <- "Known Age"
  data_long_R2 <- rbind(data_long_R2_err, data_long_R2_known)
  
  # Create a violin plot
  R2_plot <- ggplot(data_long_R2, aes(x = Variable, y = Value, fill = Category, color = Category)) +
    geom_violin(trim = FALSE, linewidth = 1.25, alpha = 0.5) +
    labs(
      x = "",
      y = expression(R^2)  # Use expression() to make 2 a superscript
    ) +
    theme_classic() +
    scale_fill_manual(name = legend_name, values = c("Ageing Error" = "skyblue", "Known Age" = "orange")) +
    scale_color_manual(name = legend_name, values = c("Ageing Error" = "deepskyblue", "Known Age" = "darkorange")) +
    scale_x_discrete(
      labels = c(
        "train_R2" = "Training", 
        "test_R2" = "Test"
      )
    ) +
    theme(
      axis.title.x = element_text(size = 14, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 14, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 12),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 12),                        # Y-axis text font size
      legend.title = element_text(size = 12, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 10)                         # Legend text font size
    )
}

# Extract legend from one plot
legend <- get_legend(R2_plot)

# Arrange plots with shared legend
combined_plot <- plot_grid(
  R2_plot + theme(legend.position = "none"), # Remove legend from R2_plot
  RMSE_plot + theme(legend.position = "none"), # Remove legend from RMSE_plot
  legend,                                      # Add the shared legend
  ncol = 3, nrow = 1, rel_widths = c(1, 1, 0.4) # Adjust heights to fit legend
)

combined_plot <- ggdraw() +
  draw_plot(combined_plot) +
  draw_label(
    "Dataset",
    x = 0.5,
    y = 0.025,  # Adjust based on plot height
    hjust = 0.5,
    size = 14,
    fontface = "bold"
  )

# Display the combined plot
print(combined_plot)
# Save the plot
ggsave(filename = './Output/Violin.png', plot = combined_plot, width = 12, height = 8, units = "in", dpi = 300)
1