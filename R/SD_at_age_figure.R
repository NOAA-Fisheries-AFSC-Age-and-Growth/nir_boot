library(ggplot2)
library(tidyr)
library(cowplot)

wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)

dir.create(paste0("./Output"),showWarnings = FALSE)

nsim = 100L

max_age = 10

#Ageing Error
{
  SD_mat_test <- matrix(data = NA, nrow = nsim, ncol = max_age)
  colnames(SD_mat_test) <- c(1:ncol(SD_mat_test))
  
  SD_mat_train <- matrix(data = NA, nrow = nsim, ncol = max_age)
  colnames(SD_mat_train) <- c(1:ncol(SD_mat_train))
  
  
  for (i in 1:nsim) {
    test_pred <- as.matrix(read.csv(paste0("./sims_err/",i,"/Output/Data/test_predictions.csv"), header = TRUE))
    train_pred <- as.matrix(read.csv(paste0("./sims_err/",i,"/Output/Data/train_predictions.csv"), header = TRUE))
    
    temp_test_age <- matrix(data = NA, nrow = length(test_pred), ncol = 3)
    colnames(temp_test_age) <- c("test_TMA","test_NIR", "test_resid")
    temp_test_age[,1] <- as.numeric(test_pred[,1])
    temp_test_age[,2] <- as.numeric(test_pred[,2])
    temp_test_age[,3] <- temp_test_age[,1] - temp_test_age[,2]
    
    for (j in 1:max_age) {
      SD_mat_test[i, j] <- sd(subset(temp_test_age, temp_test_age[, 1] == j)[,3])
    }
    
    temp_train_age <- matrix(data = NA, nrow = length(train_pred), ncol = 3)
    colnames(temp_train_age) <- c("train_TMA", "train_NIR", "train_resid")
    temp_train_age[,1] <- as.numeric(train_pred[,1])
    temp_train_age[,2] <- as.numeric(train_pred[,2])
    temp_train_age[,3] <- temp_train_age[,1] - temp_train_age[,2]
    
    for (j in 1:max_age) {
      SD_mat_train[i, j] <- sd(subset(temp_train_age, temp_train_age[, 1] == j)[,3])
    }
  }
}

#SD at Age Violin Plot
{
  legend_name <- "Data Set"
  
  SD_mat_test <- as.data.frame(SD_mat_test)
  SD_mat_train <- as.data.frame(SD_mat_train)
  
  SD_mat_test$Source <- "Test"
  SD_mat_train$Source <- "Train"
  
  combined_data <- rbind(SD_mat_test, SD_mat_train)
  
  library(tidyr)
  long_data <- pivot_longer(combined_data, cols = -Source, names_to = "Age", values_to = "Value")
  
  # Make sure the Age column is treated as a factor
  long_data$Age <- factor(long_data$Age, levels = unique(long_data$Age))
  
  err_plot <- ggplot(long_data, aes(x = Age, y = Value, fill = Source, color = Source)) +
    geom_violin(position = position_dodge(width = 0.9), trim = FALSE, linewidth = 0.75, alpha = 0.5) +
    labs(x = "Age y",
         y = "SD y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_classic() +
    scale_fill_manual(name = legend_name, values = c("Test" = "skyblue", "Train" = "orange")) +
    scale_color_manual(name = legend_name, values = c("Test" = "deepskyblue", "Train" = "darkorange")) +
    theme(
      axis.title.x = element_text(size = 14, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 14, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 12),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 12),                        # Y-axis text font size
      legend.title = element_text(size = 12, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 10)                         # Legend text font size
    )
}

#Known Age
{
  SD_mat_test <- matrix(data = NA, nrow = nsim, ncol = max_age)
  colnames(SD_mat_test) <- c(1:ncol(SD_mat_test))
  
  SD_mat_train <- matrix(data = NA, nrow = nsim, ncol = max_age)
  colnames(SD_mat_train) <- c(1:ncol(SD_mat_train))
  
  
  for (i in 1:nsim) {
    test_pred <- as.matrix(read.csv(paste0("./sims_known/",i,"/Output/Data/test_predictions.csv"), header = TRUE))
    train_pred <- as.matrix(read.csv(paste0("./sims_known/",i,"/Output/Data/train_predictions.csv"), header = TRUE))
    
    temp_test_age <- matrix(data = NA, nrow = length(test_pred), ncol = 3)
    colnames(temp_test_age) <- c("test_TMA","test_NIR", "test_resid")
    temp_test_age[,1] <- as.numeric(test_pred[,1])
    temp_test_age[,2] <- as.numeric(test_pred[,2])
    temp_test_age[,3] <- temp_test_age[,1] - temp_test_age[,2]
    
    for (j in 1:max_age) {
      SD_mat_test[i, j] <- sd(subset(temp_test_age, temp_test_age[, 1] == j)[,3])
    }
    
    temp_train_age <- matrix(data = NA, nrow = length(train_pred), ncol = 3)
    colnames(temp_train_age) <- c("train_TMA", "train_NIR", "train_resid")
    temp_train_age[,1] <- as.numeric(train_pred[,1])
    temp_train_age[,2] <- as.numeric(train_pred[,2])
    temp_train_age[,3] <- temp_train_age[,1] - temp_train_age[,2]
    
    for (j in 1:max_age) {
      SD_mat_train[i, j] <- sd(subset(temp_train_age, temp_train_age[, 1] == j)[,3])
    }
  }
}

#SD at Age Violin Plot
{
  legend_name <- "Data Set"
  
  SD_mat_test <- as.data.frame(SD_mat_test)
  SD_mat_train <- as.data.frame(SD_mat_train)
  
  SD_mat_test$Source <- "Test"
  SD_mat_train$Source <- "Train"
  
  combined_data <- rbind(SD_mat_test, SD_mat_train)
  
  library(tidyr)
  long_data <- pivot_longer(combined_data, cols = -Source, names_to = "Age", values_to = "Value")
  
  # Make sure the Age column is treated as a factor
  long_data$Age <- factor(long_data$Age, levels = unique(long_data$Age))
  
  known_plot <- ggplot(long_data, aes(x = Age, y = Value, fill = Source, color = Source)) +
    geom_violin(position = position_dodge(width = 0.9), trim = FALSE, linewidth = 0.75, alpha = 0.5) +
    labs(x = "Age y",
         y = "SD y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme_classic() +
    scale_fill_manual(name = legend_name, values = c("Test" = "skyblue", "Train" = "orange")) +
    scale_color_manual(name = legend_name, values = c("Test" = "deepskyblue", "Train" = "darkorange")) +
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
legend <- get_legend(known_plot)

# Arrange plots with shared legend
combined_plot <- plot_grid(
  known_plot + theme(legend.position = "none"), # Remove legend from R2_plot
  err_plot + theme(legend.position = "none"), # Remove legend from RMSE_plot
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