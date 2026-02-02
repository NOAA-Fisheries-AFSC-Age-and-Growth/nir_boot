#model adapted from Benson et al. 2023
#translated from python to R
  
library(tidyverse)
library(tensorflow)
library(keras3)
library(kerastuneR)
library(ggplot2)
library(tidyr)

Sys.setenv(TF_ENABLE_ONEDNN_OPTS = '0')

# Check if TensorFlow can access the GPU
#tf$config$list_physical_devices("GPU")
#tf$config$experimental$set_memory_growth(tf$config$list_physical_devices("GPU")[[1]], TRUE)

args <- commandArgs(trailingOnly = TRUE)
j <- as.numeric(args[1])
wd <- args[2]
  
# Use 'j' in your script as needed
message("Running simulation number ", j)
  
setwd(paste0(wd,"/sims_known/",j))
  
data <- read.csv('./input.csv')
spectra <- read.csv(paste0(wd,"/data/spectra.csv"))

data <- cbind(data,spectra)
  
data <- data[data$sample != "outlier", , drop = FALSE]
  
names(data)[names(data) == "final_age"] <- "Age"
  
X_train <- data[data$sample == 'training', ]
X_test <- data[data$sample == 'test', ]
  
y_train <- X_train$Age
y_test <- X_test$Age
f_train <- X_train$file_name
f_test <- X_test$file_name
s_train <- X_train$sample
s_test <- X_test$sample
  
X_train_A <- X_train[, 4:7]
X_test_A <- X_test[, 4:7]

X_train_B <- X_train[, 8:ncol(X_train)]
X_test_B <- X_test[, 8:ncol(X_test)]
  
# Fit the scaler on 'y_train' and transform 'y_train' and 'y_test'
scaler_y <- scale(y_train)
y_train <- scaler_y
y_test <- scale(y_test, center = attr(scaler_y, "scaled:center"), scale = attr(scaler_y, "scaled:scale"))
  
# Fit the scaler on 'X_train_A' and transform 'X_train_A'
scaler_x <- scale(as.matrix(X_train_A))
X_train_A <- as.data.frame(scaler_x, col.names = colnames(X_train_A))
  
# Transform 'X_test_A' using the scaler fitted on 'X_train_A'
X_test_A <- as.data.frame(scale(as.matrix(X_test_A), center = attr(scaler_x, "scaled:center"), scale = attr(scaler_x, "scaled:scale")), col.names = colnames(X_test_A))
  
input_dim_A <- as.integer(ncol(X_train_A))
input_dim_B <- as.integer(ncol(X_train_B))
  
X_train_A <- as.matrix(X_train_A)
X_train_B <- as.matrix(X_train_B)
  
X_test_A <- as.matrix(X_test_A)
X_test_B <- as.matrix(X_test_B)
  
build_model <- function(hp) {
  input_A <- layer_input(shape = input_dim_A, name = "input_A")
  x <- input_A
  
  input_B <- layer_input(shape = c(input_dim_B, 1L), name = "input_B")
  y <- layer_conv_1d(filters = hp$Int('num_filters', min_value = 50L, max_value = 100L, step = 10L, default = 50L),
                       kernel_size = 201L, strides = 101L, activation = "relu", padding = "same")(input_B)
  y <- layer_flatten()(y)
  y <- layer_dense(units = 4L, activation = "relu", name = "output_B")(y)
    
  con <- layer_concatenate(inputs = list(x, y))  # merge
  
  z <- layer_dense(units = hp$Int('dense', min_value = 4L, max_value = 640L, step = 32L, default = 256L),
                   activation = "relu")(con)
  z <- layer_dropout(rate = hp$Float('dropout-2', min_value = 0.0, max_value = 0.5, step = 0.05, default = 0.0))(z)
  
  output <- layer_dense(units = 1L, activation = "linear")(z)
  
  # Define the model
  model <- keras_model(inputs = list(input_A, input_B), outputs = output)
  
  # Compile the model
  model %>% compile(optimizer = 'adam', loss = 'mse', metrics = c('mse', 'mae'))
  
  return(model)
}
  
# Define the Hyperband tuner
tuner <- Hyperband(
  build_model,                        # Model building function
  objective = "val_loss",             # Objective to minimize
  max_epochs = 200,                   # Maximum number of epochs
  executions_per_trial = as.integer(1),           # Number of executions per trial
  seed = 42,                          # Random seed for reproducibility
  directory = "Tuners",               # Directory to store tuning logs and checkpoints
  project_name = "mmcnn"              # Project name for organization
)
  
  
# Define parameters
nb_epoch <- 200L
batch_size <- 32L
outputFilePath <- 'Estimator'
  
# Define callbacks
checkpointer <- callback_model_checkpoint(
  filepath = paste0(outputFilePath, ".keras"),  # Save path for best model
  monitor = "val_loss",                         # Metric to monitor
  verbose = 1,                                  # Verbosity level
  save_best_only = TRUE                         # Save only the best model
)
  
earlystop <- callback_early_stopping(
  monitor = "val_loss",                      # Metric to monitor for early stopping
  patience = 7,                              # Number of epochs with no improvement
  verbose = 1,                               # Verbosity level
  restore_best_weights = TRUE                # Restore best weights when stopped
)
  
# Perform hyperparameter search with callbacks
history <- tuner$search(
  x = list(X_train_A, X_train_B),  # Input data list
  y = y_train,                     # Target data
  epochs = nb_epoch,               # Number of epochs
  batch_size = batch_size,         # Batch size
  shuffle = TRUE,                  # Shuffle data
  validation_split = 0.25,         # Validation split
  verbose = 1,                     # Verbosity level
  callbacks = list(earlystop)      # List of callbacks
)
  
best_model <- tuner$get_best_models(num_models = 1L)[[1]]
keras3::save_model(best_model, filepath = "best_model.keras")
  
best_hp <- tuner$get_best_hyperparameters(num_trials = 1L)[[1]]
  
  
# Define parameters
nb_epoch <- 2000L
batch_size <- 32L
outputFilePath <- 'Estimator'
  
# Define callbacks
callbacks <- list(
  callback_model_checkpoint(
    filepath = paste0(outputFilePath, ".keras"),  # Save path for best model
    monitor = "val_loss",                         # Metric to monitor
    verbose = 1,                                  # Verbosity level
    save_best_only = TRUE                         # Save only the best model
  ),
  callback_early_stopping(
    monitor = "val_loss",                         # Metric to monitor for early stopping
    patience = 100,                               # Number of epochs with no improvement
    verbose = 1,                                  # Verbosity level
    restore_best_weights = TRUE                   # Restore best weights when stopped
  )
)
  
history <- best_model %>% keras3::fit(
  x = list(X_train_A, X_train_B),   # Input data list
  y = y_train,                      # Target data
  epochs = nb_epoch,                # Number of epochs
  batch_size = batch_size,          # Batch size
  shuffle = TRUE,                   # Shuffle data
  validation_split = 0.25,          # Validation split
  verbose = 1,                      # Verbosity level
  callbacks = callbacks             # List of callbacks
)
  
# Extract the training history
history_data <- history$metrics
  
# Assuming 'history_data' is a data frame with 'loss' and 'val_loss' columns
# Create a data frame for plotting
history_df <- data.frame(
  epoch = seq_along(history_data$loss),
  loss = history_data$loss,
  val_loss = history_data$val_loss
)
  
# Convert to long format
history_long <- pivot_longer(history_df, cols = c("loss", "val_loss"), names_to = "type", values_to = "value")
  
# Plot
ggplot(history_long, aes(x = epoch, y = value, color = type)) +
  geom_line(linewidth = 1) +
  labs(title = "Model Loss", y = "Loss", x = "Epoch") +
  scale_color_manual(values = c("blue", "red"), labels = c("Train", "Validation")) +
  theme_minimal() +
  theme(legend.position = "top")

# Evaluate the model
eval_results <- best_model %>% evaluate(
  list(X_test_A, X_test_B),
  y_test
)
print(eval_results)

# Generate predictions
preds <- best_model %>% predict(list(X_test_A, X_test_B))

# Function to compute R² score
r2_score <- function(y_true, y_pred) {
  ss_total <- sum((y_true - mean(y_true))^2)
  ss_residual <- sum((y_true - y_pred)^2)
  r2 <- 1 - (ss_residual / ss_total)
  return(r2)
}

# Compute R² score
r2 <- r2_score(y_test, preds)
print(paste("R² Score:", r2))

# Convert predictions and true values to a data frame
plot_data <- data.frame(
  True = as.vector(y_test),
  Predicted = as.vector(preds)
)

# Plot
ggplot(plot_data, aes(x = True, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(x = 'True', y = 'Predicted', title = 'True vs. Predicted Values') +
  xlim(-2.5, 5) +
  ylim(-2.5, 5) +
  theme_minimal()

# Calculate the error
error <- c(preds) - c(y_test)

# Plot Histogram
hist(error, breaks=20, main="Histogram of Prediction Error", xlab="Prediction Error", ylab="Count", col="lightblue")

summary(best_model)

# Predict using the model, training data
preds_t <- best_model %>% predict(list(X_train_A, X_train_B))

# Assuming y_train and preds_t are vectors of the same length
r2 <- r2_score(y_train, preds_t)
print(paste("R^2 Score:", r2))

# Reverse the normalization
inverse_transform <- function(predictions, mean, sd) {
  return(predictions * sd + mean)
}

# Extract mean and standard deviation
mean_y <- attr(scaler_y, "scaled:center")
sd_y <- attr(scaler_y, "scaled:scale")

# Assuming preds_t are the predicted values (standardized)
y_pr_transformed <- inverse_transform(preds_t, mean_y, sd_y)

# Check length of transformed predictions
length_y_pr_transformed <- length(y_pr_transformed)
print(length_y_pr_transformed)

# Reshape y_train to be a column vector
y_train_reshaped <- matrix(y_train, ncol = 1)

# Reverse normalization for the reshaped training data
y_tr_transformed <- inverse_transform(scaler_y, mean_y, sd_y)

# Length of transformed training data
length_y_tr_transformed <- length(y_tr_transformed)
print(length_y_tr_transformed)

# Compute R^2 score
r_squared_tr <- r2_score(y_tr_transformed, y_pr_transformed)

# Round to two decimal places
r_squared_tr_rounded <- round(r_squared_tr, 2)

# Print the rounded R^2 score
print(paste("Rounded R^2 score:", r_squared_tr_rounded))

# Manual calculation of RMSE
rmse_manual <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)  # Calculate Mean Squared Error
  return(sqrt(mse))  # Return the square root of MSE
}

# Compute RMSE
rmse_tr <- rmse_manual(y_tr_transformed, y_pr_transformed)

# Round RMSE to two decimal places
rmse_tr_rounded <- round(rmse_tr, 2)

# Print the rounded RMSE
print(paste("Rounded RMSE:", rmse_tr_rounded))

# Convert vectors to data frames
y_tr_df <- data.frame(train = y_tr_transformed, pred = y_pr_transformed)

# Reset index (in R, this is typically done by ensuring the data is a data frame)
# We can add the file column directly to the data frame
y_tr_df$file <- f_train

# Display the first few rows of the resulting data frame
head(y_tr_df, 2)

dir.create('./Output/Data', recursive = TRUE, showWarnings = FALSE)
dir.create('./Output/Figures', showWarnings = FALSE)

# Save data frame to CSV file
write.csv(y_tr_df, file = './Output/Data/train_predictions.csv', row.names = FALSE)

# Create the plot
plot <- ggplot(y_tr_df, aes(x = train, y = pred)) +
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

# Save the plot
ggsave(filename = "./Output/Figures/TrainingSet.png", plot = plot, width = 12, height = 12, units = "in", dpi = 300)
  
# Print the plot
print(plot)


#Test Data
# Apply inverse transformation
y_pred_transformed <- inverse_transform(preds, mean_y, sd_y)
y_test_transformed <- inverse_transform(y_test, mean_y, sd_y)

# Compute R^2 score
r_squared <- r2_score(y_test_transformed, y_pred_transformed)

# Round to two decimal places
r_squared_rounded <- round(r_squared, 2)

# Print the rounded R^2 score
print(paste("Rounded R^2 score:", r_squared_rounded))

# Compute RMSE
rmse <- rmse_manual(y_test_transformed, y_pred_transformed)

# Round RMSE to two decimal places
rmse_rounded <- round(rmse, 2)

# Print the rounded RMSE
print(paste("Rounded RMSE:", rmse_rounded))

# Convert vectors to data frames
y_test_df <- data.frame(train = y_test_transformed, pred = y_pred_transformed)

# Reset index (in R, this is typically done by ensuring the data is a data frame)
# We can add the file column directly to the data frame
y_test_df$file <- f_test

# Display the first few rows of the resulting data frame
head(y_test_df, 2)

# Save data frame to CSV file
write.csv(y_test_df, file = './Output/Data/test_predictions.csv', row.names = FALSE)

# Create the plot
plot <- ggplot(y_test_df, aes(x = train, y = pred)) +
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

# Save the plot
ggsave(filename = "./Output/Figures/TestSet.png", plot = plot, width = 12, height = 12, units = "in", dpi = 300)

# Print the plot
print(plot)

data <- y_test_df %>%
  mutate(
    Mean = (train + pred) / 2,
    Difference = train - pred,
    Mean_diff = mean(Difference),
    SD_diff = sd(Difference),
    Limit_of_Agreement_upper = Mean_diff + 1.96 * SD_diff,
    Limit_of_Agreement_lower = Mean_diff - 1.96 * SD_diff
  )

# Create the plot
plot <- ggplot(data, aes(x = Mean, y = Difference)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = data$Mean_diff, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = data$Limit_of_Agreement_upper, color = "red", linetype = "dashed") +
  geom_hline(yintercept = data$Limit_of_Agreement_lower, color = "red", linetype = "dashed") +
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

# Save the plot
ggsave(filename = './Output/Figures/BlandAltman.png', plot = plot, width = 12, height = 8, units = "in", dpi = 300)

# Print the plot
print(plot)

metrics <- matrix(data = NA, nrow = 1, ncol = 5)
colnames(metrics) <- c("iteration","train_R2", "train_RMSE", "test_R2", "test_RMSE")
metrics[1,1] <- j

metrics[1,2] <- as.numeric(r_squared_tr)
metrics[1,3] <- as.numeric(rmse_tr)
metrics[1,4] <- as.numeric(r_squared)
metrics[1,5] <- as.numeric(rmse)

write.csv(metrics, 
          file = paste0("./Output/Data/metrics",j,".csv"), 
          row.names = FALSE)

#save.image(file = "./Output/workspace.RData")