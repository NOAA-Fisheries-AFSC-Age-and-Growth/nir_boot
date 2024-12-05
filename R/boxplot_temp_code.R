
library(ggplot2)
library(tidyr)

nsim <- 1

# Initialize empty vectors to hold the data
all_model1_pred <- c()
all_model2_pred <- c()
all_model1_actual <- c()
all_model2_actual <- c()
all_scenario <- c()  # To keep track of scenarios

# Loop through scenarios (assuming scenarios are numbered 1 to 100)
for (i in 1:nsim) {
  # Define the file paths for each scenario
  model1_file <- paste0("C:/Users/Derek.Chamberlin/Downloads/nir_boot/nir_boot/sims_known/", i, "/Output/Data/test_predictions.csv")
  model2_file <- paste0("C:/Users/Derek.Chamberlin/Downloads/nir_boot/nir_boot/sims_err/", i, "/Output/Data/test_predictions.csv")
  
  # Read the data for each model
  model1_pred <- read.csv(model1_file)[, 2]
  model2_pred <- read.csv(model2_file)[, 2]
  model1_actual <- read.csv(model1_file)[, 1]
  model2_actual <- read.csv(model2_file)[, 1]
  
  # Append to the vectors
  all_model1_pred <- c(all_model1_pred, model1_pred)
  all_model2_pred <- c(all_model2_pred, model2_pred)
  all_model1_actual <- c(all_model1_actual, model1_actual)
  all_model2_actual <- c(all_model2_actual, model2_actual)
}

# Combine the true values and predictions into a long format dataframe
comparison_long <- data.frame(
  TrueAge = c(rep(all_model1_actual, 2), all_model2_actual), # Repeat the true age values for both models, need to add in reference values for error scenario
  PredictedAge = c(all_model1_pred, all_model2_pred, all_model2_pred),
  Model = rep(c("Null", "Age Error v Null", "Age Error"), each = length(all_model1_actual)*nsim) # Add a Model column
)

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
    axis.title.x = element_text(size = 20, face = "bold"),       # X-axis title font size and bold
    axis.title.y = element_text(size = 20, face = "bold"),       # Y-axis title font size and bold
    axis.text.x = element_text(size = 16),  # X-axis text font size and rotation
    axis.text.y = element_text(size = 16),                        # Y-axis text font size
    legend.title = element_text(size = 20, face = "bold"),        # Legend title font size and bold
    legend.text = element_text(size = 16)                         # Legend text font size
  )

print(box_plot)

# Save the plot as a PNG file
ggsave(filename = './Output/Boxplot_all_scenarios.png', plot = box_plot, width = 12, height = 8, units = "in", dpi = 300)
