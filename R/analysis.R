library(ggplot2)
library(tidyr)
library(cowplot)

wd <- "C:/Users/Derek.Chamberlin/Work/Research/TMA_FT_NIR_Uncertainty/nir_boot"
setwd(wd)

dir.create(paste0("./Output"),showWarnings = FALSE)

nsim = 10L

#Ageing Error
{
  metrics_err <- matrix(data = NA, nrow = nsim, ncol = 9)
  colnames(metrics_err) <- c("iteration", "train_R2_known", "train_RMSE_known",
                             "test_R2_known", "test_RMSE_known", "train_R2",
                             "train_RMSE", "test_R2", "test_RMSE")
  
  for (j in 1:nsim) {
    metrics_err[j,] <- as.matrix(read.csv(paste0("./sims_err/",j,"/Output/Data/metrics",j,".csv"), header = TRUE)[1,])
  }
  
  data_wide_R2_err <- as.data.frame(metrics_err[,c(1,6,8)])
  data_wide_RMSE_err <- as.data.frame(metrics_err[,c(1,7,9)])
  
  data_wide_R2_err_known <- as.data.frame(metrics_err[,c(1,2,4)])
  data_wide_RMSE_err_known <- as.data.frame(metrics_err[,c(1,3,5)])
  
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
  plot(metrics_known$cumulative_mean_train_R2, type = 'l', ylim = c(0.90, 0.95))
  lines(metrics_known$cumulative_mean_test_R2, type = 'l')
  
  write.csv(metrics_known, 
            file = "./Output/metrics_known.csv", 
            row.names = FALSE)
}

colnames(data_wide_RMSE_err_known)[colnames(data_wide_RMSE_err_known) == "test_RMSE_known"] <- "test_RMSE"
colnames(data_wide_RMSE_err_known)[colnames(data_wide_RMSE_err_known) == "train_RMSE_known"] <- "train_RMSE"

colnames(data_wide_R2_err_known)[colnames(data_wide_R2_err_known) == "test_R2_known"] <- "test_R2"
colnames(data_wide_R2_err_known)[colnames(data_wide_R2_err_known) == "train_R2_known"] <- "train_R2"

# RMSE Violin Plot
legend_name <- "Model"
{
  data_long_RMSE_err <- data_wide_RMSE_err %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),
      names_to = "Variable",
      values_to = "Value"
    )
  
  data_long_RMSE_known <- data_wide_RMSE_known %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),
      names_to = "Variable",
      values_to = "Value"
    )
  
  data_long_RMSE_err_known <- data_wide_RMSE_err_known %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),
      names_to = "Variable",
      values_to = "Value"
    )
  
  data_long_RMSE_err$Category <- "Age Error"
  data_long_RMSE_known$Category <- "Null"
  data_long_RMSE_err_known$Category <- "Age Error v Null"
  
  data_long_RMSE <- rbind(data_long_RMSE_known, data_long_RMSE_err, data_long_RMSE_err_known)
  data_long_RMSE$Category <- factor(data_long_RMSE$Category, levels = c("Null", "Age Error", "Age Error v Null"))
  
  # Create a violin plot
  RMSE_plot <- ggplot(data_long_RMSE, aes(x = Variable, y = Value, fill = Category, color = Category)) +
    geom_violin(trim = FALSE, linewidth = 1.25, alpha = 0.5) +
    labs(
      x = "",
      y = "RMSE (y)"
    ) +
    theme_classic() +
    scale_fill_manual(name = legend_name, values = c("Null" = "orange", "Age Error" = "skyblue", "Age Error v Null" = "springgreen3")) +
    scale_color_manual(name = legend_name,values = c("Null" = "darkorange", "Age Error" = "deepskyblue", "Age Error v Null" = "springgreen4")) +
    scale_x_discrete(
      labels = c(
        "train_RMSE" = "Training", 
        "test_RMSE" = "Test"
      )
    ) +
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 16)
    )
}

# R2 Violin Plot
{
  data_long_R2_err <- data_wide_R2_err %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),
      names_to = "Variable",
      values_to = "Value"
    )
  
  data_long_R2_known <- data_wide_R2_known %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),
      names_to = "Variable",
      values_to = "Value"
    )
  
  data_long_R2_err_known <- data_wide_R2_err_known %>%
    pivot_longer(
      cols = starts_with(c("train", "test")),
      names_to = "Variable",
      values_to = "Value"
    )
  
  data_long_R2_err$Category <- "Age Error"
  data_long_R2_known$Category <- "Null"
  data_long_R2_err_known$Category <- "Age Error v Null"
  
  data_long_R2 <- rbind(data_long_R2_known, data_long_R2_err, data_long_R2_err_known)
  data_long_R2$Category <- factor(data_long_R2$Category, levels = c("Null", "Age Error", "Age Error v Null"))
  
  # Create a violin plot
  R2_plot <- ggplot(data_long_R2, aes(x = Variable, y = Value, fill = Category, color = Category)) +
    geom_violin(trim = FALSE, linewidth = 1.25, alpha = 0.5) +
    labs(
      x = "",
      y = expression(R^2)
    ) +
    theme_classic() +
    scale_fill_manual(name = legend_name, values = c("Null" = "orange", "Age Error" = "skyblue", "Age Error v Null" = "springgreen3")) +
    scale_color_manual(name = legend_name, values = c("Null" = "darkorange", "Age Error" = "deepskyblue", "Age Error v Null" = "springgreen4")) +
    scale_x_discrete(
      labels = c(
        "train_R2" = "Training", 
        "test_R2" = "Test"
      )
    ) +
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 16)
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
    y = 0.025,
    hjust = 0.5,
    size = 20,
    fontface = "bold"
  )

print(combined_plot)

ggsave(filename = './Output/Violin.png', plot = combined_plot, width = 12, height = 8, units = "in", dpi = 300)


#boxplot figure
{
  all_model1_pred <- c()
  all_model2_pred <- c()
  all_model1_actual <- c()
  all_model2_actual <- c()
  
  for (i in 1:nsim) {
    model1_file <- paste0("./sims_known/", i, "/Output/Data/test_predictions.csv")
    model2_file <- paste0("./sims_err/", i, "/Output/Data/test_predictions.csv")
    
    model1_pred <- read.csv(model1_file)[, 2]
    model2_pred <- read.csv(model2_file)[, 2]
    model1_actual <- read.csv(model1_file)[, 1]
    model2_actual <- read.csv(model2_file)[, 1]
    
    all_model1_pred <- c(all_model1_pred, model1_pred)
    all_model2_pred <- c(all_model2_pred, model2_pred)
    all_model1_actual <- c(all_model1_actual, model1_actual)
    all_model2_actual <- c(all_model2_actual, model2_actual)
  }
  
  # Combine the true values and predictions into a long format dataframe
  comparison_long <- data.frame(
    TrueAge = c(all_model1_actual, all_model2_actual, all_model1_actual),
    PredictedAge = c(all_model1_pred, all_model2_pred, all_model2_pred),
    Model = rep(c("Null", "Age Error", "Age Error v Null"))
  )
  
  comparison_long$Model <- factor(comparison_long$Model, levels = c("Null", "Age Error", "Age Error v Null"))
  
  box_plot <- ggplot(comparison_long, aes(x = as.factor(TrueAge), y = PredictedAge, fill = Model)) +
    geom_boxplot(outlier.size = 1.75, size = 0.75) +
    geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
    labs(x = "Reference age y",
         y = "FT-NIR Age y") +
    scale_fill_manual(name = "Model", values = c("Null" = "orange", "Age Error" = "skyblue", "Age Error v Null" = "springgreen3")) +
    scale_y_continuous(breaks = seq(0, 24, by = 2), limits = c(0, 24), expand = c(0, 0)) +
    scale_x_discrete(breaks = seq(0, 24, by = 2), limits = as.factor(1:24), expand = c(0, 0)) +
    expand_limits(x = 0, y = 0)+
    theme_classic() +
    theme(
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 16)
    )
}

print(box_plot)

ggsave(filename = './Output/Boxplot_all_scenarios.png', plot = box_plot, width = 12, height = 8, units = "in", dpi = 300)