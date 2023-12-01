# This file includes sample code on how to check and visualize the confidence vs prediction intervals 




# Generate sample data
set.seed(123)  # for reproducibility
n <- 100
x <- rnorm(n)
y <- 2 * x + 1 + rnorm(n)

# Fit a linear regression model for confidence intervals
lm_model_confidence <- lm(y ~ x)

# Fit a linear regression model for prediction intervals
lm_model_prediction <- lm(y ~ x)

# Create new data for prediction
new_data <- data.frame(x = seq(-2, 2, by = 0.1))

# Set alpha levels for confidence and prediction intervals
alpha_confidence <- 0.90  # Confidence level for confidence intervals
alpha_prediction <- 0.80  # Confidence level for prediction intervals

# Predict the values and confidence intervals for the confidence model
predictions_confidence <- predict(lm_model_confidence, newdata = new_data, interval = "confidence", level = alpha_confidence)

# Predict the values and prediction intervals for the prediction model
predictions_prediction <- predict(lm_model_prediction, newdata = new_data, interval = "prediction", level = alpha_prediction)

# Create data frames for the results
results_confidence <- data.frame(
  x = new_data$x,
  Predicted = predictions_confidence[, 1],
  Lower_CI = predictions_confidence[, 2],
  Upper_CI = predictions_confidence[, 3]
)

results_prediction <- data.frame(
  x = new_data$x,
  Predicted = predictions_prediction[, 1],
  Lower_PI = predictions_prediction[, 2],
  Upper_PI = predictions_prediction[, 3]
)

# Print the results for confidence intervals
print("Results for Confidence Intervals:")
print(results_confidence)

# Print the results for prediction intervals
print("Results for Prediction Intervals:")
print(results_prediction)


