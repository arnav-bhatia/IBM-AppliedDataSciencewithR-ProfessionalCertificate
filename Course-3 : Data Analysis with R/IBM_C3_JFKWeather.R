rm(list = ls())

#1

URL <- 'https://dax-cdn.cdn.appdomain.cloud/dax-noaa-weather-data-jfk-airport/1.1.4/noaa-weather-sample-data.tar.gz'
download.file(URL, destfile = 'noaa-weather-sample-data.tar.gz')

# Untar the zipped file
untar('noaa-weather-sample-data.tar.gz')

#2

# Extract the tar.gz file
untar("noaa-weather-sample-data.tar.gz")

# Read the CSV file into a dataframe
df <- read.csv("noaa-weather-sample-data/jfk_weather_sample.csv")

# Display the first few rows of the dataframe
head(df)

# Take a glimpse of the dataset
str(df)

#3
# Select subset of columns
selected_columns <- df[c("HOURLYRelativeHumidity", "HOURLYDRYBULBTEMPF", "HOURLYPrecip", "HOURLYWindSpeed", "HOURLYStationPressure")]

# Show the first 10 rows of the new dataframe
head(selected_columns, 10)

#4
library(dplyr)
library(stringr)

unique(df$HOURLYPrecip)

# Replace "T" values with "0.0"
df$HOURLYPrecip <- ifelse(df$HOURLYPrecip == "T", "0.0", df$HOURLYPrecip)

# Remove "s" from the end of values
df$HOURLYPrecip <- str_remove(df$HOURLYPrecip, "s$")

unique(df$HOURLYPrecip)

#5
# Check column types
glimpse(df)

# Convert HOURLYPrecip to numeric type
df$HOURLYPrecip <- as.numeric(df$HOURLYPrecip)

# Check column types again
glimpse(df)

#6

# Rename columns
df <- df %>%
  rename(
    relative_humidity = HOURLYRelativeHumidity,
    dry_bulb_temp_f = HOURLYDRYBULBTEMPF,
    precip = HOURLYPrecip,
    wind_speed = HOURLYWindSpeed,
    station_pressure = HOURLYStationPressure
  )

# Display the first few rows of the dataframe
head(df)

#7
library(tidyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(1234)

# Split the data into training and testing sets
train_indices <- sample(nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Plot histograms for each variable in the training set
train_data %>%
  gather(key = "variable", value = "value", -precip) %>%
  mutate(value = as.numeric(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Histograms of Weather Variables",
       x = "Value",
       y = "Frequency") +
  theme_minimal()




#8
library(broom)
# Function to create linear regression model and plot
create_lm_and_plot <- function(data, x_var, y_var) {
  # Create linear regression model
  lm_model <- lm(data = data, formula = as.formula(paste(y_var, "~", x_var)))
  
  # Extract coefficients
  lm_coefficients <- tidy(lm_model)
  
  # Plot scatter plot with regression line
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = paste("Linear Regression: ", y_var, " ~ ", x_var),
         x = x_var,
         y = y_var) +
    theme_minimal() +
    geom_text(x = max(data[[x_var]]), y = min(data[[y_var]]), 
              label = paste("R-squared =", round(summary(lm_model)$r.squared, 2)), 
              hjust = 1, vjust = 0)
}
# Create and plot linear regression models for each predictor variable
create_lm_and_plot(train_data, "relative_humidity", "precip")
create_lm_and_plot(train_data, "dry_bulb_temp_f", "precip")
create_lm_and_plot(train_data, "wind_speed", "precip")
create_lm_and_plot(train_data, "station_pressure", "precip")

#9
set.seed(1234)

train_indices <- sample(nrow(df), 0.8 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

compute_rmse <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2))
}

# Model 1: Add more features/predictors
model_1 <- lm(precip ~ relative_humidity + dry_bulb_temp_f + wind_speed + station_pressure, data = train_data)

# Model 2: Add regularization (ridge regression)
# Prepare the design matrices for both training and testing data
x_train <- model.matrix(precip ~ ., data = train_data)[, -1]  # Exclude intercept column
x_test <- model.matrix(precip ~ ., data = test_data)[, -1]    # Exclude intercept column
y_train <- train_data$precip
model_2 <- glmnet::glmnet(x = x_train,
                          y = y_train,
                          alpha = 0,    # Ridge regression
                          lambda = 0.1) # Regularization parameter

# Model 3: Add a polynomial component
model_3 <- lm(precip ~ poly(relative_humidity, degree = 2) + poly(dry_bulb_temp_f, degree = 2) +
                poly(wind_speed, degree = 2) + poly(station_pressure, degree = 2), data = train_data)

# Predictions
train_preds_1 <- predict(model_1, newdata = train_data)
test_preds_1 <- predict(model_1, newdata = test_data)

train_rmse_1 <- compute_rmse(train_data$precip, train_preds_1)
test_rmse_1 <- compute_rmse(test_data$precip, test_preds_1)

train_preds_2 <- predict(model_2, newx = x_train, s = 0.1)
test_preds_2 <- predict(model_2, newx = x_test, s = 0.1)

train_rmse_2 <- compute_rmse(y_train, train_preds_2)
test_rmse_2 <- compute_rmse(test_data$precip, test_preds_2)

train_preds_3 <- predict(model_3, newdata = train_data)
test_preds_3 <- predict(model_3, newdata = test_data)

train_rmse_3 <- compute_rmse(train_data$precip, train_preds_3)
test_rmse_3 <- compute_rmse(test_data$precip, test_preds_3)

# Print RMSE for each model
print(paste("Model 1 - Train RMSE:", train_rmse_1, "Test RMSE:", test_rmse_1))
print(paste("Model 2 - Train RMSE:", train_rmse_2, "Test RMSE:", test_rmse_2))
print(paste("Model 3 - Train RMSE:", train_rmse_3, "Test RMSE:", test_rmse_3))

#10
# Evaluate models on the testing set
test_predictions_model1 <- predict(model_1, newdata = test_data)
test_predictions_model2 <- predict(model_2, newx = model.matrix(precip ~ ., data = test_data)[, -1], s = 0.1)
test_predictions_model3 <- predict(model_3, newdata = test_data)

# Calculate RMSE for each model on testing set
rmse_model1 <- compute_rmse(test_data$precip, test_predictions_model1)
rmse_model2 <- compute_rmse(test_data$precip, test_predictions_model2)
rmse_model3 <- compute_rmse(test_data$precip, test_predictions_model3)

# Create a comparison table
model_names <- c("Model 1: Add More Features", "Model 2: Regularization (Ridge)", "Model 3: Polynomial Component")
test_rmse <- c(rmse_model1, rmse_model2, rmse_model3)
comparison_df <- data.frame(Model = model_names, Test_RMSE = test_rmse)

# Print comparison table
print(comparison_df)


