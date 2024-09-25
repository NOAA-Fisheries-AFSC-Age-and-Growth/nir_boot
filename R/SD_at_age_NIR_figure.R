library(ggplot2)
library(tidyr)
library(cowplot)
library(dplyr)

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
  legend_name <- "Dataset"
  
  SD_mat_test <- as.data.frame(SD_mat_test)
  SD_mat_train <- as.data.frame(SD_mat_train)
  
  SD_mat_test$Source <- "Test"
  SD_mat_train$Source <- "Train"
  
  combined_data_err <- rbind(SD_mat_test, SD_mat_train)
  
  library(tidyr)
  long_data <- pivot_longer(combined_data_err, cols = -Source, names_to = "Age", values_to = "Value")
  
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
      axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 16),                        # Y-axis text font size
      legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 16)                         # Legend text font size
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
  legend_name <- "Dataset"
  
  SD_mat_test <- as.data.frame(SD_mat_test)
  SD_mat_train <- as.data.frame(SD_mat_train)
  
  SD_mat_test$Source <- "Test"
  SD_mat_train$Source <- "Train"
  
  combined_data_known <- rbind(SD_mat_test, SD_mat_train)
  
  library(tidyr)
  long_data <- pivot_longer(combined_data_known, cols = -Source, names_to = "Age", values_to = "Value")
  
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
      axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 16),                        # Y-axis text font size
      legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 16)                         # Legend text font size
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
ggsave(filename = './Output/Violin_SD_at_Age.png', plot = combined_plot, width = 12, height = 8, units = "in", dpi = 300)

#Test Known and Error Plot
combined_data_known <- combined_data_known %>%
  mutate(Source = case_when(
    Source == "Test" ~ "Test-Known",
    Source == "Train" ~ "Train-Known",
    TRUE ~ Source
  ))
combined_data_err <- combined_data_err %>%
  mutate(Source = case_when(
    Source == "Test" ~ "Test-Err",
    Source == "Train" ~ "Train-Err",
    TRUE ~ Source
  ))

test <- rbind(subset(combined_data_known, Source == "Test-Known"), subset(combined_data_err, Source == "Test-Err"))
test <- test %>%
  mutate(Source = case_when(
    Source == "Test-Err" ~ "Age Error",
    Source == "Test-Known" ~ "No Age Error",
    TRUE ~ Source
  ))


long_data <- pivot_longer(test, cols = -Source, names_to = "Age", values_to = "Value")

# Make sure the Age column is treated as a factor
long_data$Age <- factor(long_data$Age, levels = unique(long_data$Age))

test_plot <- ggplot(long_data, aes(x = Age, y = Value, fill = Source, color = Source)) +
  geom_violin(position = position_dodge(width = 0.9), trim = FALSE, linewidth = 0.75, alpha = 0.5) +
  labs(x = "Age y",
       y = "SD y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic() +
  scale_fill_manual(name = legend_name, values = c("No Age Error" = "skyblue", "Age Error" = "orange")) +
  scale_color_manual(name = legend_name, values = c("No Age Error" = "deepskyblue", "Age Error" = "darkorange")) +
  theme(
    axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
    axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
    axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
    axis.text.y = element_text(size = 16),                        # Y-axis text font size
    legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
    legend.text = element_text(size = 16)                         # Legend text font size
  )+
  coord_cartesian(ylim = c(0, 3))
print(test_plot)
ggsave(filename = './Output/Violin_Test_SD_at_Age.png', plot = test_plot, width = 16, height = 8, units = "in", dpi = 300)


#NIR BOOT, NIR PUNT, TMA PUNT SD
{
  {
    TMA_sd <- matrix(ncol=7, nrow = 24)
    TMA_sd[,1] <- as.numeric(read.csv("./data/7_reader_TMA_TMB_Age_23/Results/Pollock SS3_format_Reader1.csv", header=TRUE)[4,-1])
    TMA_sd[,2] <- as.numeric(read.csv("./data/7_reader_TMA_TMB_Age_23/Results/Pollock SS3_format_Reader2.csv", header=TRUE)[4,-1])
    TMA_sd[,3] <- as.numeric(read.csv("./data/7_reader_TMA_TMB_Age_23/Results/Pollock SS3_format_Reader3.csv", header=TRUE)[4,-1])
    TMA_sd[,4] <- as.numeric(read.csv("./data/7_reader_TMA_TMB_Age_23/Results/Pollock SS3_format_Reader4.csv", header=TRUE)[4,-1])
    TMA_sd[,5] <- as.numeric(read.csv("./data/7_reader_TMA_TMB_Age_23/Results/Pollock SS3_format_Reader5.csv", header=TRUE)[4,-1])
    TMA_sd[,6] <- as.numeric(read.csv("./data/7_reader_TMA_TMB_Age_23/Results/Pollock SS3_format_Reader6.csv", header=TRUE)[4,-1])
    TMA_sd[,7] <- as.numeric(read.csv("./data/7_reader_TMA_TMB_Age_23/Results/Pollock SS3_format_Reader7.csv", header=TRUE)[4,-1])
    colnames(TMA_sd) <- c("SD_R1", "SD_R2", "SD_R3", "SD_R4", "SD_R5", "SD_R6", "SD_R7")
  }
  
  {
    NIR_sd <- matrix(ncol=6, nrow = 19)
    NIR_sd[,1] <- as.numeric(read.csv("C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/Age_Error_Matrices/Tester/NIR/Results/Mod_Age SS3_format_Reader2.csv", header=TRUE)[4,-1])
    NIR_sd[,2] <- as.numeric(read.csv("C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/Age_Error_Matrices/Tester/NIR/Results/Mod_Age SS3_format_Reader3.csv", header=TRUE)[4,-1])
    NIR_sd[,3] <- as.numeric(read.csv("C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/Age_Error_Matrices/Tester/NIR/Results/Mod_Age SS3_format_Reader4.csv", header=TRUE)[4,-1])
    NIR_sd[,4] <- as.numeric(read.csv("C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/Age_Error_Matrices/Tester/NIR/Results/Mod_Age SS3_format_Reader5.csv", header=TRUE)[4,-1])
    NIR_sd[,5] <- as.numeric(read.csv("C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/Age_Error_Matrices/Tester/NIR/Results/Mod_Age SS3_format_Reader6.csv", header=TRUE)[4,-1])
    NIR_sd[,6] <- as.numeric(read.csv("C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/Age_Error_Matrices/Tester/NIR/Results/Mod_Age SS3_format_Reader7.csv", header=TRUE)[4,-1])
    colnames(NIR_sd) <- c("SD_NIR1", "SD_NIR2", "SD_NIR3", "SD_NIR4", "SD_NIR5", "SD_NIR6")
  }
  
  TMA <- matrix(data = NA, nrow = nrow(TMA_sd), ncol = 3)
  TMA[,1] <- 0:(nrow(TMA_sd)-1)
  
  for (i in 1:nrow(TMA)) {
    TMA[i,2] <- mean(as.numeric(TMA_sd[i,]))
    TMA[i,3] <- sd(as.numeric(TMA_sd[i,]))
  }
  
  NIR <- matrix(data = NA, nrow = nrow(NIR_sd), ncol = 3)
  NIR[,1] <- 0:(nrow(NIR_sd)-1)
  
  for (i in 1:nrow(NIR)) {
    NIR[i,2] <- mean(as.numeric(NIR_sd[i, ]))
    NIR[i,3] <- sd(as.numeric(NIR_sd[i,]))
  }
  
  
  long_data <- long_data %>%
    filter(!is.na(Value))
  
  NIR_boot <- matrix(data = NA, nrow = nrow(NIR_sd), ncol = 3)
  NIR_boot[,1] <- 0:(nrow(NIR_boot)-1)
  for (i in 1:nrow(NIR)) {
    NIR_temp <- subset(long_data, Age == i-1)
    NIR_boot[i,2] <- mean(NIR_temp$Value)
    NIR_boot[i,3] <- sd(NIR_temp$Value)
  }
  NIR_boot <- NIR_boot[-1,]
  #NIR_boot <- NIR_boot[-11:-23,]
  
  # Convert TMA and NIR matrices to data frames
  TMA_df <- as.data.frame(TMA)
  colnames(TMA_df) <- c("Age", "Mean_TMA", "SD_TMA")
  TMA_df$SE_TMA <- TMA_df$SD_TMA/sqrt(7)
  TMA_df$Type <- "TMA"
  
  NIR_df <- as.data.frame(NIR)
  colnames(NIR_df) <- c("Age", "Mean_NIR", "SD_NIR")
  NIR_df$SE_NIR <- NIR_df$SD_NIR/sqrt(6)
  NIR_df$Type <- "NIR"
  
  NIR_boot_df <- as.data.frame(NIR_boot)
  colnames(NIR_boot_df) <- c("Age", "Mean_NIR_Boot", "SD_NIR_Boot")
  NIR_boot_df$SE_NIR_Boot <- NIR_boot_df$SD_NIR_Boot/sqrt(7)
  NIR_boot_df$Type <- "NIR_Boot"
  
  # Convert to long format for ggplot
  TMA_long <- TMA_df %>%
    pivot_longer(cols = starts_with("Mean_TMA"), names_to = "Measure", values_to = "Value") %>%
    pivot_longer(cols = starts_with("SE_TMA"), names_to = "SE_Measure", values_to = "SE")
  TMA_long$Age <- TMA_long$Age - 0.25
  
  NIR_long <- NIR_df %>%
    pivot_longer(cols = starts_with("Mean_NIR"), names_to = "Measure", values_to = "Value") %>%
    pivot_longer(cols = starts_with("SE_NIR"), names_to = "SE_Measure", values_to = "SE")
  
  NIR_boot_long <- NIR_boot_df %>%
    pivot_longer(cols = starts_with("Mean_NIR_Boot"), names_to = "Measure", values_to = "Value") %>%
    pivot_longer(cols = starts_with("SE_NIR_Boot"), names_to = "SE_Measure", values_to = "SE")
  NIR_boot_long$Age <- NIR_boot_long$Age + 0.25
  
  # Combine all data into one long data frame
  combined_df <- bind_rows(
    TMA_long %>% mutate(Type = "TMA"),
    NIR_long %>% mutate(Type = "NIR"),
    NIR_boot_long %>% mutate(Type = "NIR_Boot")
  )
  
  combined_df <- subset(combined_df, Age <= 17.25)
  
  TMA_NIR_NIR_Boot_plot<- ggplot(combined_df, aes(x = Age, y = Value, color = Type, shape = Type)) +
    geom_point(size = 6) +
    geom_errorbar(aes(ymin = Value - 1.96 * SE, ymax = Value + 1.96 * SE), width = 0.5, size = 1) +
    scale_color_manual(values = c("TMA" = "skyblue", "NIR" = "tomato1", "NIR_Boot" = "springgreen3")) +
    scale_shape_manual(values = c("TMA" = 16, "NIR" = 15, "NIR_Boot" = 17)) +
    labs(x = "Age y", y = "SD") +
    theme_classic() +
    theme(legend.title = element_blank())+
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 16),                        # Y-axis text font size
      legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 16)                         # Legend text font size
    )+
    coord_cartesian(ylim = c(0, 3), xlim = c(0, 20))
  print(TMA_NIR_NIR_Boot_plot)
  ggsave(filename = './Output/TMA_NIR_NIR_Boot_SD_Summary_3.png', plot = TMA_NIR_NIR_Boot_plot, width = 16, height = 8, units = "in", dpi = 300)
  
  
  combined_df_2 <- subset(combined_df, Type != "NIR")
  
  TMA_NIR_NIR_Boot_plot<- ggplot(combined_df_2, aes(x = Age, y = Value, color = Type, shape = Type)) +
    geom_point(size = 6) +
    geom_errorbar(aes(ymin = Value - 1.96 * SE, ymax = Value + 1.96 * SE), width = 0.5, size = 1) +
    scale_color_manual(values = c("TMA" = "skyblue", "NIR_Boot" = "springgreen3")) +
    scale_shape_manual(values = c("TMA" = 16, "NIR_Boot" = 17)) +
    labs(x = "Age y", y = "SD") +
    theme_classic() +
    theme(legend.title = element_blank())+
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 16),                        # Y-axis text font size
      legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 16)                         # Legend text font size
    )+
    coord_cartesian(ylim = c(0, 3), xlim = c(0, 20))
  print(TMA_NIR_NIR_Boot_plot)
  ggsave(filename = './Output/TMA_NIR_NIR_Boot_SD_Summary_4.png', plot = TMA_NIR_NIR_Boot_plot, width = 16, height = 8, units = "in", dpi = 300)
  
  
  combined_df <- subset(combined_df, Type != "NIR_Boot")
  
  TMA_NIR_NIR_Boot_plot<- ggplot(combined_df, aes(x = Age, y = Value, color = Type, shape = Type)) +
    geom_point(size = 6) +
    geom_errorbar(aes(ymin = Value - 1.96 * SE, ymax = Value + 1.96 * SE), width = 0.5, size = 1) +
    scale_color_manual(values = c("TMA" = "skyblue", "NIR" = "tomato1")) +
    scale_shape_manual(values = c("TMA" = 16, "NIR" = 15)) +
    labs(x = "Age y", y = "SD") +
    theme_classic() +
    theme(legend.title = element_blank())+
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 16),                        # Y-axis text font size
      legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 16)                         # Legend text font size
    )+
    coord_cartesian(ylim = c(0, 3), xlim = c(0, 20))
  print(TMA_NIR_NIR_Boot_plot)
  ggsave(filename = './Output/TMA_NIR_NIR_Boot_SD_Summary_2.png', plot = TMA_NIR_NIR_Boot_plot, width = 16, height = 8, units = "in", dpi = 300)
  
  combined_df <- subset(combined_df, Type != "NIR")
  
  TMA_NIR_NIR_Boot_plot<- ggplot(combined_df, aes(x = Age, y = Value, color = Type, shape = Type)) +
    geom_point(size = 6) +
    geom_errorbar(aes(ymin = Value - 1.96 * SE, ymax = Value + 1.96 * SE), width = 0.5, size = 1) +
    scale_color_manual(values = c("TMA" = "skyblue")) +
    scale_shape_manual(values = c("TMA" = 16)) +
    labs(x = "Age y", y = "SD") +
    theme_classic() +
    theme(legend.title = element_blank())+
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
      axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
      axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
      axis.text.y = element_text(size = 16),                        # Y-axis text font size
      legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
      legend.text = element_text(size = 16)                         # Legend text font size
    )+
    coord_cartesian(ylim = c(0, 3), xlim = c(0, 20))
  print(TMA_NIR_NIR_Boot_plot)
  ggsave(filename = './Output/TMA_NIR_NIR_Boot_SD_Summary_1.png', plot = TMA_NIR_NIR_Boot_plot, width = 16, height = 8, units = "in", dpi = 300)
}



